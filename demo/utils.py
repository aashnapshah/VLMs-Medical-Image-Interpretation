
import os
import sys
import pandas as pd
import google.generativeai as genai
from dotenv import load_dotenv
from openai import OpenAI
import requests
import base64

OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')
GOOGLE_API_KEY = 'AIzaSyCb6q8gdLFt3KJ9g906CLAmcAHACtfhqhk' 

def encode_image(image_path):
    """Your docstring here"""
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')

def open_ai_process_request(model, prompt, filename, max_tokens=50):
    image_path = filename
    base64_image = encode_image(image_path)
    payload = {
        "model": model,
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

    headers = {
                    "Content-Type": "application/json",
                    "Authorization": f"Bearer {OPENAI_API_KEY}",
                    }
    
    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)
    
    if response.status_code == 200:
        response_text = response.json()['choices'][0]['message']['content']
        return filename, prompt, response_text
    else:
        return filename, prompt, None
    
