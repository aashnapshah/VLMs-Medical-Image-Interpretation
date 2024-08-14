# openai_utils.py
import os
import time
import csv
import glob
import base64
import logging
import pandas as pd
import requests
import functools
from openai import OpenAI
from concurrent.futures import ThreadPoolExecutor
import argparse

import sys
sys.path.append('../')

from config import Config

def parse_args():
    parser = argparse.ArgumentParser(description="Process images for a specific department.")
    parser.add_argument("--department", help="Specify the department (dermatology or radiology)", required=True)
    args = parser.parse_args()
    return args.department

# Configure logging
logging.basicConfig(level=logging.INFO)

def get_image_paths(input: str) -> list:
    """Get a list of image paths from the input directory or list."""
    if isinstance(input, list):
        image_paths = input
    elif isinstance(input, str):
        image_paths = glob.glob(os.path.join(input, '*'))
    elif isinstance(input, pd.Series):
        image_paths = input.tolist()
    else:
        raise ValueError("Input must be a list of image paths or a directory path")
    return image_paths

def encode_image(image_path):
    """Your docstring here"""
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
    
def process_request(filename, prompt, config, max_tokens=7):
    image_path = os.path.join(config.folder_path, filename)
    base64_image = encode_image(image_path)
    payload = {
        "model": "gpt-4-vision-preview",
        "messages": [
            {
                "role": "user",
                "content": [
                    {"type": "text", "text": prompt},
                    {"type": "image_url", "image_url": {"url": f"data:image/jpeg;base64,{base64_image}"}}
                ]
            }
        ],
        "max_tokens": max_tokens,
    }

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=config.headers, json=payload)
    
    if response.status_code == 200:
        response_text = response.json()['choices'][0]['message']['content']
        print(f"Processed {filename}")
        return filename, prompt, response_text
    else:
        print(f"Error processing {filename} with prompt '{prompt}': {response.text}")
        return filename, prompt, None
    
def process_pair(pair, config):
    print(config.OPENAI_API_KEY)

    file_name, prompt_id = pair
    if file_name.endswith(('.png', '.jpg')):
        if isinstance(prompt_id, int):
            text_prompt = config.prompts_dict[prompt_id] 
        else: 
            text_prompt = prompt_id
            
        try:
            response = process_request(file_name, text_prompt, config)
            return {"Filename": response[0], "PromptID": prompt_id, "Response": response[2]}
        except Exception as e:
            print(f"Error processing {file_name}: {e}")
            return None
        
def main():
    department = parse_args()
    config = Config(department)
    image_paths = get_image_paths(config.image_paths)
    date = time.strftime("%Y%m%d")
    date = '20240328'

    csvfile_path = f"../data/{department}/apiResults/gpt4v_{department}_results_{date}.csv"
    #date = '20240318'
    #csvfile_path = f"data/{department}/gpt4v_{department}_results_{date}_single_word.csv"

    processed_count = 0
    print(os.path.exists(csvfile_path))
    
    if os.path.exists(csvfile_path):
        mode = 'a'  # append if already exists
        all_image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in config.prompts_dict.keys()]
        
        # remove any na and remove if prompt bigger than 8
        completed_df = pd.read_csv(csvfile_path)
        completed_df = completed_df.dropna(subset=['Response'])
        completed_pairs = list(zip(completed_df['Filename'], completed_df['PromptID']))  
                
        image_prompt_pairs = [pair for pair in all_image_prompt_pairs if pair not in completed_pairs]
        print(len(image_prompt_pairs))
    else:
        mode = 'w'  # write if does not exist
        image_prompt_pairs = [] #[(image_path, prompt_id) for image_path in image_paths for prompt_id in config.prompts_dict.keys()]
        
    with ThreadPoolExecutor() as executor:
        results = list(executor.map(lambda pair: process_pair(pair, config), image_prompt_pairs))

    with open(csvfile_path, mode, newline='') as csvfile:
        fieldnames = ["Filename", "PromptID", "Response"]
        csv_writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        if csvfile.tell() == 0:
            csv_writer.writeheader()  # file doesn't exist yet, write a header

        for result in results:
            if result is not None:
                csv_writer.writerow(result)
                
if __name__ == "__main__":
    logging.info("Starting run...")
    main()