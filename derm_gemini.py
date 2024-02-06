import os
import asyncio
import aiohttp
import pandas as pd
import PIL.Image
from dotenv import load_dotenv
import google.generativeai as genai

load_dotenv()

# Set environment variables
API_KEY = os.getenv("GEMINI_API_KEY")
genai.configure(api_key=API_KEY)
model = genai.GenerativeModel('gemini-pro-vision')

async def process_image(file_name, text_prompt, session, folder_path):
    try:
        img_path = os.path.join(folder_path, file_name)
        img = PIL.Image.open(img_path)

        response = await model.generate_content_async([text_prompt, img], session=session)
        return file_name, text_prompt, response.text
    except Exception as e:
        print(f"Error processing {file_name}: {e}")
        return file_name, text_prompt, "Error"

async def main():
    folder_path = '../DDI/'
    text_prompts = ["Is this skin image malignant or benign?", "what is the diagnosis of this skin image?"]
    all_results = []
    rate_limit = 60

    async with aiohttp.ClientSession() as session:
        tasks = []
        for file_name in os.listdir(folder_path):
            if file_name.endswith('.png'):
                for text_prompt in text_prompts:
                    tasks.append(process_image(file_name, text_prompt, session, folder_path))

                    # Process in batches of 60
                    if len(tasks) >= rate_limit:
                        all_results.extend(await asyncio.gather(*tasks))
                        tasks = []
                        print(f"Processed a batch of {rate_limit} tasks, sleeping for rate limit...")
                        await asyncio.sleep(60)

        # Process any remaining tasks
        if tasks:
            all_results.extend(await asyncio.gather(*tasks))

    # Convert results to DataFrame and save
    df = pd.DataFrame(all_results, columns=['Filename', 'TextPrompt', 'Response'])
    df.to_csv('api_results/gemini_ddi_results_xyz.csv', index=False)

# Run the asynchronous main function
asyncio.run(main())
