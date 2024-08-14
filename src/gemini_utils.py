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
from config import Config
from concurrent.futures import ThreadPoolExecutor, as_completed
import argparse

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

def load_model(config):
    genai.configure(api_key=Config.GOOGLE_API_KEY)
    model = genai.GenerativeModel(config.model_name)
    return model

def process_image(config, model, file_name: str, text_prompt: str, safety_settings: list) -> tuple:
    """Process an image and return the filename, text prompt, and response."""
    img_path = os.path.join(config.folder_path, file_name)
    with Image.open(img_path) as image:
        try:
            response = model.generate_content([text_prompt, image], safety_settings=safety_settings)
            #logging.info(response.text)
            return file_name, text_prompt, response.text
        except Exception as e:
            time.sleep(2)
            logging.error(f"Error processing hererere {file_name}: {e}")
            return file_name, text_prompt, None

def main():
    """Main function to process images and save results to a CSV file."""
    department = parse_args()
    config = Config(department)
    image_paths = get_image_paths(config.image_paths)
    date = time.strftime("%Y%m%d")
    date = '20240312'
    csvfile_path = f"../data/{department}/apiResults/gemini_chexpert_results_20240312.csv"
    #csvfile_path = f"data/{department}/gemini_ddi_results_{date}_single_word_errors.csv"

    processed_count = 0
    
    if os.path.exists(csvfile_path):
        mode = 'a'  # append if already exists
        all_image_prompt_pairs = [(image_path, prompt_id) for image_path in image_paths for prompt_id in config.prompts_dict.keys()]
        
        # remove any na and remove if prompt bigger than 8
        completed_df = pd.read_csv(csvfile_path)
        completed_df = completed_df.dropna(subset=['Response'])
        completed_df = completed_df[~completed_df['Response'].str.contains("Error")]
        completed_df.to_csv(csvfile_path, index=False)
        
        completed_pairs = list(zip(completed_df['Filename'], completed_df['PromptID']))  
        image_prompt_pairs = [pair for pair in all_image_prompt_pairs if pair not in completed_pairs]

    else:
        mode = 'w'  # write if does not exist
        image_prompt_pairs = [] #[(image_path, prompt_id) for image_path in image_paths for prompt_id in config.prompts_dict.keys()]
        
    with open(csvfile_path, mode, newline='') as csvfile:
        fieldnames = ["Filename", "PromptID", "Response"]
        csv_writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        if csvfile.tell() == 0:
            csv_writer.writeheader()  # file doesn't exist yet, write a header

        for file_name, prompt_id in image_prompt_pairs:    
            if file_name.endswith(('.png', '.jpg')):
                text_prompt = config.prompts_dict[prompt_id] 
                try:
                    model = load_model(config)
                    response = process_image(config, model, file_name, text_prompt, config.safety_settings)
                    #response = process_image(config, file_name, text_prompt, config.safety_settings)
                    csv_writer.writerow({"Filename": response[0], "PromptID": prompt_id, "Response": response[2]})
                    csvfile.flush()
                except Exception as exc:
                    logging.error(f'An exception occurred: {exc}')
                processed_count += 1
                if processed_count >= 50:
                    logging.info("Rate limit reached. Sleeping for 60 seconds.")
                    time.sleep(10)
                    processed_count = 0 
    
if __name__ == "__main__":
    logging.info("Starting run...")
    main()