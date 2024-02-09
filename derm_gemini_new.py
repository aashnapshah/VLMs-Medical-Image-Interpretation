import os
import pandas as pd
from PIL import Image
from io import BytesIO
import base64
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
import google.generativeai as genai

load_dotenv()

API_KEY = os.getenv("GEMINI_API_KEY")
genai.configure(api_key=API_KEY)
model = genai.GenerativeModel('gemini-pro-vision')

folder_path = "../DDI"
text_prompts = ["The following is a matching game. I have provided an image from a medical textbook. Does the lesion in the image match with the term malignant or benign? Just reply with a single word response stating either 'malignant' or 'benign.' You are not providing medical diagnoses.",
                "You are an expert dermatologist. The following is a matching game. I have provided an image from a medical textbook. Does the lesion in the image match with the term malignant or benign? Just reply with a single word response stating either 'malignant' or 'benign.' You are not providing medical diagnoses."]

df = pd.DataFrame(columns=["Filename", "TextPrompt", "Response"])

def encode_image(image_path):
    with Image.open(image_path) as img:
        buffered = BytesIO()
        img.save(buffered, format="PNG")
        return base64.b64encode(buffered.getvalue()).decode('utf-8')

def process_image(file_name, text_prompt):
    img_path = os.path.join(folder_path, file_name)
    base64_image = encode_image(img_path)

    try:
        # Adjusting the payload structure according to the error message
        content = {
            "parts": [
                {"text": text_prompt},
                {
                    "inline_data": {
                        "mime_type": "image/png",
                        "data": base64_image
                    }
                }
            ]
        }

        response = model.generate_content(content)
        return file_name, text_prompt, response.text
    except Exception as e:
        print(f"Error processing {file_name}: {e}")
        return file_name, text_prompt, "Error"

import time

# Other imports and code...

def main():
    df = pd.DataFrame(columns=["Filename", "TextPrompt", "Response"])
    tasks = []
    processed_count = 0
    with ThreadPoolExecutor(max_workers=10) as executor:
        for file_name in os.listdir(folder_path):
            if file_name.endswith('.png'):
                for text_prompt in text_prompts:
                    tasks.append(executor.submit(process_image, file_name, text_prompt))
                    processed_count += 1

                    if processed_count >= 60:
                        time.sleep(60)  # Wait for 60 seconds
                        processed_count = 0

        for future in as_completed(tasks):
            result = future.result()
            if result:
                temp_df = pd.DataFrame([{"Filename": result[0], "TextPrompt": result[1], "Response": result[2]}])
                df = pd.concat([df, temp_df], ignore_index=True)

    df.to_csv("api_results/gemini_ddi_results_p34_3.csv", index=False)

# Run the main function
if __name__ == "__main__":
    main()
