# openai_utils.py
import os
import time
import csv
import glob
import base64
import logging
import pandas as pd
import requests
from openai import OpenAI
from config import OPENAI_API_KEY, OPENAI_ORG_ID
from concurrent.futures import ThreadPoolExecutor

# Configure logging
logging.basicConfig(level=logging.INFO)

class Config:
    """Configuration for the script."""
    folder_path = 'data/CheXpert/'
    image_paths = pd.read_csv('data/processed_test_val_set_20240319.csv')['Path'] # Validation set only
    prompts_dict = pd.read_csv('prompts.csv')['Prompt'].to_dict() 
    headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {OPENAI_API_KEY}",
    }

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
    
def process_request(filename, prompt, max_tokens=7):
    image_path = os.path.join(Config.folder_path, filename)
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

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=Config.headers, json=payload)
    
    if response.status_code == 200:
        response_text = response.json()['choices'][0]['message']['content']
        print(f"Processed {filename}")
        return filename, prompt, response_text
    else:
        print(f"Error processing {filename} with prompt '{prompt}': {response.text}")
        return filename, prompt, None
def process_pair(pair):
    file_name, prompt_id = pair
    if file_name.endswith(('.png', '.jpg')):
        text_prompt = Config.prompts_dict[prompt_id] 
        try:
            response = process_request(file_name, text_prompt)
            return {"Filename": response[0], "PromptID": prompt_id, "Response": response[2]}
        except Exception as e:
            print(f"Error processing {file_name}: {e}")
            return None
        
def main():
    image_paths = get_image_paths(Config.image_paths)
    date = time.strftime("%Y%m%d")
    date = '20240318'
    csvfile_path = f"data/gpt4v_chexpert_results_{date}.csv"

    processed_count = 0
    
    if os.path.exists(csvfile_path):
        mode = 'a'  # append if already exists
        completed_df = pd.read_csv(csvfile_path)
        completed_df = completed_df.dropna(subset=['Response'])
        completed_pairs = list(zip(completed_df['Filename'], completed_df['PromptID']))  
        image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in Config.prompts_dict.keys()]
        image_prompt_pairs = [pair for pair in image_prompt_pairs if pair not in completed_pairs]

    else:
        mode = 'w'  # write if does not exist
        image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in Config.prompts_dict.keys()]
        
    with ThreadPoolExecutor() as executor:
        results = list(executor.map(process_pair, image_prompt_pairs))

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