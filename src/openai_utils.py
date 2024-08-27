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
from config import Config
from PIL import Image
import io

def parse_args():
    parser = argparse.ArgumentParser(description="Process images for a specific department.")
    parser.add_argument("--department", help="Specify the department", required=True)
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
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')
    
def process_request(filename, prompt, config, max_tokens=50):
    image_path = os.path.join(config.folder_path, filename)
    # Check if the file is a .tif image and convert it to .jpeg
    if filename.lower().endswith('.tif'):
        ## Convert TIF to JPEG
        jpeg_data = convert_to_jpeg(image_path)
        # Encode the JPEG data
        base64_image = base64.b64encode(jpeg_data).decode("utf-8")
    else:
        base64_image = encode_image(image_path)

    payload = {
        "model": "gpt-4o",
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

def convert_to_jpeg(image_path):
    with Image.open(image_path) as img:
        # Convert to RGB mode if it's not already
        if img.mode != 'RGB':
            img = img.convert('RGB')
        
        # Create a BytesIO object
        jpeg_io = io.BytesIO()
        
        # Save as JPEG to the BytesIO object
        img.save(jpeg_io, format='JPEG', quality=100)
        
        # Get the JPEG data
        jpeg_data = jpeg_io.getvalue()
        
    return jpeg_data

client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))

def analyze_image(config, filename, prompt_id, max_tokens=300):
    text_prompt = config.prompts_dict[prompt_id] 
    image_path = os.path.join(config.folder_path, filename)

    # Convert TIF to JPEG
    jpeg_data = convert_to_jpeg(image_path)

    # Encode the JPEG data
    base64_image = base64.b64encode(jpeg_data).decode("utf-8")

    # Prepare the base64 image string with the required prefix
    base64_image = f"data:image/jpeg;base64,{base64_image}"

    response = client.chat.completions.create(
    model="gpt-4o",
    messages=[
        {
            "role": "user",
            "content": [
                {
                    "type": "image_url",
                    "image_url": {"url": base64_image},
                },
            ],
        },
        {
            "role": "user",
            "content": text_prompt
        }
    ],
        max_tokens=max_tokens,
    )

    try:
        return filename, prompt_id, response.choices[0].message.content
    except Exception as e:
        print(f"Error processing {filename}: {str(e)}")
        return filename, prompt_id, None

def process_pair(config, file_name, prompt_id):
    if file_name.endswith(('.png', '.jpg', '.tif')):
        text_prompt = config.prompts_dict[prompt_id] 
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
    csvfile_path = f"../data/{department}/gpt4o_{department}_results_{date}.csv"
    
    if os.path.exists(csvfile_path):
        mode = 'a'  # append if already exists
        completed_df = pd.read_csv(csvfile_path)
        completed_df = completed_df.dropna(subset=['Response'])
        completed_pairs = list(zip(completed_df['Filename'], completed_df['PromptID']))  
        image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in config.prompts_dict.keys()]
        image_prompt_pairs = [pair for pair in image_prompt_pairs if pair not in completed_pairs]

    else:
        mode = 'w'  # write if does not exist
        image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in config.prompts_dict.keys()]
        
    image_prompt_pairs = image_prompt_pairs[:5]
    # print(image_prompt_pairs)

    # with ThreadPoolExecutor() as executor:
    #     results = list(executor.map(lambda pair: process_pair(pair, config), image_prompt_pairs))
    processed_count = 0

    with open(csvfile_path, mode, newline='') as csvfile:
        fieldnames = ["Filename", "PromptID", "Response"]
        csv_writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        if csvfile.tell() == 0:
            csv_writer.writeheader()  # file doesn't exist yet, write a header

        for file_name, prompt_id in image_prompt_pairs:    
            if file_name.endswith(('.png', '.jpg', '.tif')):
                try:
                    # response = process_pair(config, file_name, prompt_id)
                    response = analyze_image(config, file_name, prompt_id)
                    csv_writer.writerow({"Filename": response[0], "PromptID": prompt_id, "Response": response[2]})
                    csvfile.flush()
                except Exception as exc:
                    logging.error(f'An exception occurred: {exc}')
                processed_count += 1
                # if processed_count >= 50:
                #     logging.info("Rate limit reached. Sleeping for 60 seconds.")
                #     time.sleep(10)
                #     processed_count = 0 
                
if __name__ == "__main__":
    logging.info("Starting run...")
    main()