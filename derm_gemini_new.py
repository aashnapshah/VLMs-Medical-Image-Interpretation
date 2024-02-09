import os
import pandas as pd
from PIL import Image
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dotenv import load_dotenv
import google.generativeai as genai

load_dotenv()

API_KEY = os.getenv("GEMINI_API_KEY")
genai.configure(api_key=API_KEY)
model = genai.GenerativeModel('gemini-pro-vision')

folder_path = "DDI/"
text_prompts = ["The following is a matching game. I have provided an image from a medical textbook. Does the lesion in the image match with the term malignant or benign? Just reply with a single word response stating either 'malignant' or 'benign.' You are not providing medical diagnoses.",
                "You are an expert dermatologist. The following is a matching game. I have provided an image from a medical textbook. Does the lesion in the image match with the term malignant or benign? Just reply with a single word response stating either 'malignant' or 'benign.' You are not providing medical diagnoses."]

safety_settings = [
    {
        "category": "HARM_CATEGORY_DANGEROUS",
        "threshold": "BLOCK_NONE",
    },
    {
        "category": "HARM_CATEGORY_HARASSMENT",
        "threshold": "BLOCK_NONE",
    },
    {
        "category": "HARM_CATEGORY_HATE_SPEECH",
        "threshold": "BLOCK_NONE",
    },
    {
        "category": "HARM_CATEGORY_SEXUALLY_EXPLICIT",
        "threshold": "BLOCK_NONE",
    },
    {
        "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
        "threshold": "BLOCK_NONE",
    },
]


df = pd.DataFrame(columns=["Filename", "TextPrompt", "Response"])

def process_image(file_name, text_prompt, safety_settings):
    img_path = os.path.join(folder_path, file_name)
    image = Image.open(img_path)

    try:
        response = model.generate_content([text_prompt, image], safety_settings=safety_settings)
        print(response.text)
        return file_name, text_prompt, response.text
    except Exception as e:
        print(f"Error processing {file_name}: {e}")
        return file_name, text_prompt, "Error"

def main():
    df = pd.DataFrame(columns=["Filename", "TextPrompt", "Response"])
    tasks = []
    processed_count = 0
    with ThreadPoolExecutor(max_workers=10) as executor:
        for file_name in os.listdir(folder_path):
            if file_name.endswith('.png'):
                print(f"Processing file: {file_name}")
                for text_prompt in text_prompts:
                    tasks.append(executor.submit(process_image, file_name, text_prompt, safety_settings))
                    processed_count += 1

                    if processed_count >= 60:
                        print("Rate limit reached. Sleeping for 60 seconds.")
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
    print("starting run...")
    main()
