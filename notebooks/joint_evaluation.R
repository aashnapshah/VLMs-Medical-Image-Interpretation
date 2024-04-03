library(tidyverse)
library(caret)
library(reshape2)


base_dir <- "/Users/aashnashah/Dropbox/Research/derm-gemini-vs-gpt4/"

# Radiology 
radiology_data_dir <- "data/radiology/"
radiology_prompt_df <- read.csv(paste0(base_dir, data_dir, "prompts.csv"))

# Dermatology 
dermatology_data_dir <- "data/dermatology/"

tables_dir <- "tables/"
figures_dir <- "figures/"

# Read in Gemini results and add model column
radiology_gemini_results <- read.csv(paste0(base_dir, radiology_data_dir, "apiResults/gemini_chexpert_results_20240312.csv"))
radiology_gemini_results$Model <- "Gemini Pro Vision"

# Read in GPT-4v results and add Model column
radiology_openai_results <- read.csv(paste0(base_dir, radiology_data_dir, "apiResults/gpt4v_radiology_results_20240328.csv"))
radiology_openai_results$Model <- "GPT-4 Vision"

# Filter rows without prompt image pait
radiology_api_results <- rbind(radiology_openai_results, radiology_gemini_results)

# Dermatology
ddi <- read_csv(paste0(base_dir, dermatology_data_dir, "apiResults/ddi_metadata.csv"))
dermatology_gemini_results <- read_csv((paste0(base_dir, dermatology_data_dir, "apiResults/gemini_ddi_results_20240326_single_word.csv")))
dermatology_gemini_results$Model <- "Gemini Pro Vision"

dermatology_gpt4_results <- read_csv((paste0(base_dir, dermatology_data_dir, "apiResults/gpt4v_dermatology_results_20240318_single_word.csv")))
dermatology_gpt4_results$Model <- "GPT-4 Vision"               


