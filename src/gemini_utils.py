import os
import time
import glob
import csv
import pandas as pd
import logging
from PIL import Image
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
import google.generativeai as genai
from config import GOOGLE_API_KEY
from concurrent.futures import ThreadPoolExecutor, as_completed

# Configure logging
logging.basicConfig(level=logging.INFO)

class Config:
    """Configuration for the script."""
    genai.configure(api_key=GOOGLE_API_KEY)
    model = genai.GenerativeModel('gemini-1.0-pro-vision-latest')
    folder_path = 'data/CheXpert/'
    image_paths = pd.read_csv('data/processed_test_val_set_20240319.csv')['Path']
    prompts_dict = pd.read_csv('prompts.csv')['Prompt'].to_dict()
    safety_settings = [
        {"category": "HARM_CATEGORY_DANGEROUS", "threshold": "BLOCK_NONE"},
        {"category": "HARM_CATEGORY_HARASSMENT", "threshold": "BLOCK_NONE"},
        {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": "BLOCK_NONE"},
        {"category": "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold": "BLOCK_NONE"},
        {"category": "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold": "BLOCK_NONE"},
    ]

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

def process_image(file_name: str, text_prompt: str, safety_settings: list) -> tuple:
    """Process an image and return the filename, text prompt, and response."""
    img_path = os.path.join(Config.folder_path, file_name)
    with Image.open(img_path) as image:
        try:
            response = Config.model.generate_content([text_prompt, image], safety_settings=safety_settings)
            #logging.info(response.text)
            return file_name, text_prompt, response.text
        except Exception as e:
            time.sleep(60)
            logging.error(f"Error processing hererere {file_name}: {e}")
            return file_name, text_prompt, "Error"

def main():
    """Main function to process images and save results to a CSV file."""
    image_paths = get_image_paths(Config.image_paths)
    date = time.strftime("%Y%m%d")
    date = '20240312'
    csvfile_path = f"data/gemini_chexpert_results_{date}.csv"

    processed_count = 0
    
    if os.path.exists(csvfile_path):
        mode = 'a'  # append if already exists
        completed_df = pd.read_csv(csvfile_path)
        completed_pairs = list(zip(completed_df['Filename'], completed_df['PromptID']))  
        image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in Config.prompts_dict.keys()]
        image_prompt_pairs = [pair for pair in image_prompt_pairs if pair not in completed_pairs]

    else:
        mode = 'w'  # write if does not exist
        image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in Config.prompts_dict.keys()]
        
    with open(csvfile_path, mode, newline='') as csvfile:
        fieldnames = ["Filename", "PromptID", "Response"]
        csv_writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        if csvfile.tell() == 0:
            csv_writer.writeheader()  # file doesn't exist yet, write a header

        for file_name, prompt_id in image_prompt_pairs:    
            if file_name.endswith(('.png', '.jpg')):
                text_prompt = Config.prompts_dict[prompt_id] 
                try:
                    response = process_image(file_name, text_prompt, Config.safety_settings)
                    csv_writer.writerow({"Filename": response[0], "PromptID": prompt_id, "Response": response[2]})
                    csvfile.flush()
                except Exception as exc:
                    logging.error(f'An exception occurred: {exc}')
                processed_count += 1
                if processed_count >= 50:
                    logging.info("Rate limit reached. Sleeping for 180 seconds.")
                    time.sleep(60)
                    processed_count = 0 
    
if __name__ == "__main__":
    logging.info("Starting run...")
    main()