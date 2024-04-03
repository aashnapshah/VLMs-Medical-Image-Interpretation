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
openai_results$Model <- "GPT-4 Vision"

# Filter rows without prompt image pait
radiology_api_results <- rbind(openai_results, gemini_results)

# Dermatology
ddi <- read_csv(paste0(base_dir, dermatology_data_dir, "apiResults/ddi_metadata.csv"))

# Read in Gemini and GPT-4v results
gemini_results <- read_csv((paste0(base_dir, dermatology_data_dir, "apiResults/gemini_dermatology_results_20240318_single_word.csv")))
gpt4_results <- read_csv((paste0(base_dir, dermatology_data_dir, "apiResults/gpt4_dermatology_results_20240318_single_word.csv")))
                           




# Read in demographics data
demographics <- read.csv(paste0(base_dir, data_dir, "CheXpert/processed_test_set_2024-03-10.csv"))
demographics <- read.csv(paste0(base_dir, data_dir, "CheXpert/processed_test_val_set_20240319.csv"))
demographics$Age <- cut(demographics$AGE_AT_CXR, 
                        breaks = c(0, 44, 70, Inf), labels = c("18-44", "44-70", "70-96"))

# Merge GPT-4v results with demographics
df <- merge(api_results, demographics, by.x = "Filename", by.y = "Path", all.x = TRUE)
df <- df[order(df$Filename, df$PromptID), ]
df$Gender <- ifelse(df$GENDER == "" | is.na(df$GENDER), "unknown", df$GENDER)

# Define a function to categorize the Model"s response
categorize_response <- function(response) {
  response <- tolower(response)
  ifelse(grepl("\\b(error)\\b", response), "Blocked",
         ifelse(grepl("unable|sorry|cannot|not possible|impossible to", response, ignore.case = TRUE), "Refused",
                ifelse(grepl("\\b(normal)\\b", response) | grepl("\\b(abnormal)\\b", response), "Diagnosed",
                       ifelse(is.na(response), "Blocked", "Blocked")
                )
         )
  )
}

# Define a function to extract the initial response
extract_initial_response <- function(response) {
  response <- tolower(response)
  ifelse(grepl("\\b(abnormal)\\b", response), 1,
         ifelse(grepl("\\b(normal)\\b", response), 0, NA)
  )
}

# Clean the dataframe
df_cleaned <- df %>%
  mutate(CategorizedResponse = categorize_response(Response),
         PredictedDiagnosis = extract_initial_response(Response))

# Calculate unique values for cleaned Response column
unique_values <- calculate_unique_values(df_cleaned, 'Response')
unique_values <- calculate_unique_values(df_cleaned, 'CategorizedResponse')
unique_values <- calculate_unique_values(df_cleaned, 'PredictedDiagnosis')
print(unique_values)

df_cleaned <- df_cleaned %>%
  filter(PromptID < 8) %>% 
  mutate(PromptID = PromptID + 1)

result <- df_cleaned %>%
  mutate(PromptID_ = paste0("P", as.numeric(PromptID)))

# Merge with prompt_df based on PromptID_
result_with_prompt <- merge(result, prompt_df, by.x = "PromptID_", by.y = "ID")

# Reorder columns with Full_Prompt as the first column
result_filtered_sorted <- result_with_prompt %>%
  select(Prompt, everything())
# Calculate counts of each CategorizedResponse for each Model and PromptID

df_counts <- result_filtered_sorted %>%
  group_by(Model, PromptID, CategorizedResponse, .groups = "drop") %>%
  summarize(Count = n()) %>%
  ungroup()