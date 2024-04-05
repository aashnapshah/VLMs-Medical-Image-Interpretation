# dermatology_cleaning_functions.R

analyze_prompt_version_gemini <- function(df, prompt_version) {
  one_word_col <- paste0("one_word_", prompt_version)
  responded_col <- paste0("responded_", prompt_version)
  
  df <- df %>%
    mutate(!!one_word_col := case_when(
      str_to_lower(!!sym(prompt_version)) %in% c("malignant", "malignant.", "this lesion is malignant.", "the lesion is malignant.") ~ TRUE,
      grepl("likely malignant|concerning for melanoma|it is malignant|- malignant|concerning for basal|The lesion in the image is malignant.|matches with the term malignant", !!sym(prompt_version), ignore.case = TRUE) ~ TRUE,
      str_to_lower(!!sym(prompt_version)) %in% c("benign", "benign.", "this lesion is benign.", "the lesion is benign.", "This skin lesion is benign.", "The answer is benign.", "The lesion in the image appears benign.") ~ FALSE,
      grepl("likely benign|- benign|This skin lesion is benign.|appears to be a benign lesion|This does not appear to be a concerning lesion.|is a benign|versicolor is not malignant|which is a benign|The lesion in the image is benign|matches with the term benign|the image is a benign lesion.|therefore benign|is 'benign.'|looks benign|answer is benign|appears benign|Erysipelas", !!sym(prompt_version), ignore.case = TRUE) ~ FALSE,
      TRUE ~ NA
    ),
    !!responded_col := case_when(
      grepl("cannot|not possible|impossible to|too blurry|onion", !!sym(prompt_version), ignore.case = TRUE) ~ "Refused",
      is.na(!!sym(prompt_version)) ~ "Blocked",
      !is.na(!!sym(one_word_col)) ~ "Diagnosed",
      grepl("Error", !!sym(prompt_version), ignore.case = TRUE) ~ "Blocked",
      TRUE ~ "Other"  # Default case
    ))
  
  return(df)
}

analyze_prompt_version_gpt4 <- function(df, prompt_version) {
  one_word_col <- paste0("one_word_", prompt_version)
  responded_col <- paste0("responded_", prompt_version)
  
  df <- df %>%
    mutate(!!one_word_col := case_when(
      str_to_lower(!!sym(prompt_version)) %in% c("malignant", "malignant.") ~ TRUE,
      str_to_lower(!!sym(prompt_version)) %in% c("benign", "benign.") ~ FALSE,
      TRUE ~ NA
    ),
    !!responded_col := case_when(
      grepl("cannot|not possible|sorry|As an AI|I'm unable to provide this information|I'm sorry,|unable to|Unable to", !!sym(prompt_version), ignore.case = TRUE) ~ "Refused",
      is.na(!!sym(prompt_version)) ~ "Blocked",
      !is.na(!!sym(one_word_col)) ~ "Diagnosed",
      TRUE ~ "Other"  # Default case
    ))
  
  return(df)
}

clean_dermatology_results <- function(df, prompt_version) {
  gemini_results <- df %>% filter(Model == "Gemini Pro Vision")
  gpt4_results <- df %>% filter(Model == "GPT-4 Vision")

  #### Gemini ####
  gemini_results <- gemini_results %>% 
    mutate(PromptID = paste0("P",PromptID+1)) 
  
  gemini_wide <- gemini_results %>% 
    #mutate(Model = "Gemini-Pro-Vision") %>% 
    pivot_wider(names_from = PromptID, values_from = Response)
  
  for (version in unique(gemini_results$PromptID)) {
    gemini_wide <- analyze_prompt_version_gemini(gemini_wide, version)
  }

  #### GPT-4 ####
  gpt4_results <- gpt4_results %>% 
    #mutate(Model = "GPT-4V") %>% 
    mutate(PromptID = paste0("P",PromptID+1))
  
  gpt4_wide <- gpt4_results %>% 
    pivot_wider(names_from = PromptID, values_from = Response)
  
  for (version in unique(gpt4_results$PromptID)) {
    gpt4_wide <- analyze_prompt_version_gpt4(gpt4_wide, version)
  }
  
  results <- bind_rows(gemini_wide, gpt4_wide)
  prompt_response <- results %>%
    select(matches("^P|^Filename|^Model")) %>% 
    pivot_longer(cols = starts_with("P"), names_to = "PromptID", values_to = "Response") %>%
    arrange(Filename, PromptID) 
  prompt_diagnosis <- results %>%
    select(matches("^one_word|^Filename|^Model")) %>% 
    pivot_longer(cols = starts_with("one_word"), names_to = "PromptID2", values_to = "PredictedDiagnosis") %>%
    mutate(PromptID = gsub(".*_", "", PromptID2)) %>%
    arrange(Filename, PromptID) %>%
    inner_join(prompt_response, by = c("Filename", "Model", "PromptID"))
  prompt_category <- results %>%
    select(matches("^responded|^Filename|^Model")) %>%
    pivot_longer(cols = starts_with("responded"), names_to = "PromptID3", values_to = "CategorizedResponse") %>%
    mutate(PromptID = gsub(".*_", "", PromptID3)) %>%
    arrange(Filename, PromptID) %>%  
    inner_join(prompt_diagnosis, by = c("Filename", "Model", "PromptID"))
  dermatology_api_results_filtered <- prompt_category
  
  ddi <- read_csv(paste0(base_dir, dermatology_data_dir, "ddi_metadata.csv"))
  dermatology_api_results_filtered <- inner_join(dermatology_api_results_filtered, ddi, by = c("Filename" = "DDI_file"))
  
  return (dermatology_api_results_filtered)
}



# Radiology Functions
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

clean_radiology_results <- function(df) {
  # Clean the dataframe
  # Read in demographics data
  base_dir <- "/Users/aashnashah/Dropbox/Research/derm-gemini-vs-gpt4/"
  radiology_data_dir <- "data/radiology/"
  
  demographics <- read.csv(paste0(base_dir, radiology_data_dir, "CheXpert/processed_test_set_2024-03-10.csv"))
  demographics <- read.csv(paste0(base_dir, radiology_data_dir, "CheXpert/processed_test_val_set_20240319.csv"))
  demographics$Age <- cut(demographics$AGE_AT_CXR, 
                          breaks = c(0, 44, 70, Inf), labels = c("18-44", "44-70", "70-96"))
  
  # Merge GPT-4v results with demographics
  df <- merge(df, demographics, by.x = "Filename", by.y = "Path", all.x = TRUE)
  df <- df[order(df$Filename, df$PromptID), ]
  df$Gender <- ifelse(df$GENDER == "" | is.na(df$GENDER), "unknown", df$GENDER)
  
  df_cleaned <- df %>%
    mutate(CategorizedResponse = categorize_response(Response),
           PredictedDiagnosis = extract_initial_response(Response))
  df_cleaned <- df_cleaned %>%
    filter(PromptID < 8) %>% 
    mutate(PromptID = paste0("P",PromptID+1))

  
  return(df_cleaned)
  
}

read_results <- function(data_dir, domain) {
  gemini_files <- list.files(paste0(base_dir, data_dir, "apiResults"), pattern = "^gemini.*\\.csv$", full.names = TRUE)
  gemini_data <- lapply(gemini_files, function(file) read_csv(file, col_types = cols(), show_col_types = FALSE))
  gemini_data <- bind_rows(gemini_data)
  gemini_data$Model <- 'Gemini Pro Vision'
  
  gpt_files <- list.files(paste0(base_dir, data_dir, "apiResults"), pattern = "^gpt.*\\.csv$", full.names = TRUE)
  gpt_data <- lapply(gpt_files, function(file) read_csv(file, col_types = cols(), show_col_types = FALSE))
  gpt_data <- bind_rows(gpt_data)
  gpt_data$Model <- 'GPT-4 Vision'
  
  results <- bind_rows(gemini_data, gpt_data)
  if (domain == 'Dermatology') {
    result_cleaned <- clean_dermatology_results(results)
    ddi <- read_csv(paste0(base_dir, dermatology_data_dir, "ddi_metadata.csv"))
    
  }
  else if (domain == 'Radiology') {
    result_cleaned <- clean_radiology_results(results)
  }
  
  return(result_cleaned)
}


