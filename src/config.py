import os
import sys
import pandas as pd
import google.generativeai as genai
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

class Config:
    # Shared configurations
    OPENAI_API_KEY = os.getenv('OPENAI_API_KEY')
    # OPENAI_ORG_ID = os.getenv('OPENAI_ORG_ID')
    GOOGLE_API_KEY = os.getenv('GOOGLE_API_KEY')

    def __init__(self, department):
        # Initialize department-specific configurations
        self.department = department
        self.load_department_settings()
        self.load_model_settings()
    
    def load_department_settings(self):
        if self.department == "dermatology":
            self.folder_path = "data/dermatology/DDI"
            self.image_paths = pd.read_csv('../data/dermatology/DDI/gemini_errors.csv')['DDI_file']
            self.prompts_dict = pd.read_csv('../data/dermatology/prompts.csv')['Prompt'].to_dict()
            # Additional dermatology-specific settings can be added here
        elif self.department == "radiology":
            self.folder_path = "data/CheXpert/"
            self.image_paths = pd.read_csv('../data/processed_test_val_set_20240319.csv')['Path']
            self.prompts_dict = pd.read_csv('../data/radiology/prompts.csv')['Prompt'].to_dict()
            # Additional radiology-specific settings can be added here
        elif self.department == 'histology':
            self.folder_path = "data/histology/"
            self.image_paths = pd.read_csv('../data/histology/histo_metadata.csv')['file']
            self.prompts_dict = pd.read_csv('../data/histology/prompts.csv')['Prompt'].to_dict()
        else:
            raise ValueError(f"Unsupported department: {self.department}")
    def load_model_settings(self):
        # Identify the main script name
        main_script_name = os.path.basename(sys.argv[0])

        if "openai" in main_script_name:
            # Configure for OpenAI
            self.headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {self.OPENAI_API_KEY}",
            }
            # You can add more OpenAI specific settings here
        elif "gemini" in main_script_name:
            genai.configure(api_key=self.GOOGLE_API_KEY)
            # Configure for Google Gemini
            self.model = genai.GenerativeModel('gemini-1.5-flash')
            self.safety_settings = [
                {"category": "HARM_CATEGORY_DANGEROUS", "threshold": "BLOCK_NONE"},
                {"category": "HARM_CATEGORY_HARASSMENT", "threshold": "BLOCK_NONE"},
                {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": "BLOCK_NONE"},
                {"category": "HARM_CATEGORY_SEXUALLY_EXPLICIT", "threshold": "BLOCK_NONE"},
                {"category": "HARM_CATEGORY_DANGEROUS_CONTENT", "threshold": "BLOCK_NONE"},
            ]
            # Add any Google API specific settings or initializations here
        else:
            # Default or error handling
            raise ValueError(f"Script name '{main_script_name}' does not match any known configurations.")
