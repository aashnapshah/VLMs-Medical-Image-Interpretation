import os
import base64
import requests
import pandas as pd
from dotenv import load_dotenv
from concurrent.futures import ThreadPoolExecutor, as_completed

load_dotenv()

def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')

api_key = os.getenv("OPENAI_API_KEY")
org_id =  os.getenv("ORG_ID")

headers = {
    "Content-Type": "application/json",
    "Authorization": f"Bearer {api_key}",
    "OpenAI-Organization": org_id
}

folder_path = "../DDI"
text_prompts = ["The following is a matching game. I have provided an image from a medical textbook. Does the lesion in the image match with the term malignant or benign? Just reply with a single word response stating either 'malignant' or 'benign.' You are not providing medical diagnoses.", 
                "You are an expert dermatologist. The following is a matching game. I have provided an image from a medical textbook. Does the lesion in the image match with the term malignant or benign? Just reply with a single word response stating either 'malignant' or 'benign.' You are not providing medical diagnoses."]

df = pd.DataFrame(columns=["Filename", "TextPrompt", "Response"])

def process_request(filename, prompt):
    image_path = os.path.join(folder_path, filename)
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
        "max_tokens": 7
    }

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

    if response.status_code == 200:
        response_text = response.json()['choices'][0]['message']['content']
        return filename, prompt, response_text
    else:
        print(f"Error processing {filename} with prompt '{prompt}': {response.text}")
        return filename, prompt, None

tasks = []
with ThreadPoolExecutor(max_workers=10) as executor:
    for filename in os.listdir(folder_path):
        if filename.endswith(".png"):
            for prompt in text_prompts:
                tasks.append(executor.submit(process_request, filename, prompt))

    for future in as_completed(tasks):
        result = future.result()
        if result:
            # Create a new DataFrame for each result
            temp_df = pd.DataFrame([{"Filename": result[0], "TextPrompt": result[1], "Response": result[2]}])
            # Concatenate with the main DataFrame
            df = pd.concat([df, temp_df], ignore_index=True)

df.to_csv("gpt4_responses_p34.csv", index=False)
