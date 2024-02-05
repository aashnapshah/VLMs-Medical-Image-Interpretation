library(tidyverse)
library(caret)

ddi <- read_csv("api_results/ddi_metadata.csv")

# Read the CSV files
df2 <- read_csv("api_results/gemini_ddi_results_2.csv")
df3 <- read_csv("api_results/gemini_ddi_results_missing_2v1.csv")
df4 <- read_csv("api_results/gemini_ddi_results_missing_2v2.csv")
df5 <- read_csv("api_results/gemini_ddi_results_missing_2v3.csv")

# Merge the data frames
result_df <- df2 %>%
  left_join(df3, by = c("Filename", "TextPrompt"), suffix = c("", ".3")) %>%
  left_join(df4, by = c("Filename", "TextPrompt"), suffix = c("", ".4")) %>%
  left_join(df5, by = c("Filename", "TextPrompt"), suffix = c("", ".5")) 

# Fill NA values in 'Response' column until max responses 
result_df <- result_df %>%
  mutate(Response = coalesce(Response, Response.3, Response.4, Response.5))

# Remove the additional Response columns if not needed
result_df <- select(result_df, -starts_with("Response."))

result_df %>% filter(grepl("It is malignant", Response))

g2 <- result_df %>% 
  mutate(prompt_version = case_when(
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "expert_simple",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "standard_simple",
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "expert_eng",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "standard_eng",
    TRUE ~ NA_character_  # default case if none of the above conditions are met
  )) %>%
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert_simple = case_when(
    str_to_lower(expert_simple) %in% c("malignant", "malignant.", "this lesion is malignant.", "the lesion is malignant.") ~ TRUE,
    grepl("likely malignant|concerning for melanoma|it is malignant|- malignant|concerning for basal", expert_simple, ignore.case = TRUE) ~ TRUE,
    str_to_lower(expert_simple) %in% c("benign", "benign.", "this lesion is benign.", "the lesion is benign.", "This skin lesion is benign.") ~ FALSE,
    grepl("likely benign|- benign|This skin lesion is benign.|appears to be a benign lesion|This does not appear to be a concerning lesion.|is a benign solar|versicolor is not malignant|which is a benign", expert_simple, ignore.case = TRUE) ~ FALSE,
    TRUE ~ NA
  )) %>% 
  mutate(one_word_standard_simple = case_when(
    str_to_lower(standard_simple) %in% c("malignant", "malignant.", "this lesion is malignant.", "the lesion is malignant.") ~ TRUE,
    grepl("likely malignant|concerning for melanoma|it is malignant|- malignant|concerning for basal", standard_simple, ignore.case = TRUE) ~ TRUE,
    str_to_lower(standard_simple) %in% c("benign", "benign.", "this lesion is benign.", "the lesion is benign.", "This skin lesion is benign.") ~ FALSE,
    grepl("likely benign|- benign|This skin lesion is benign.|appears to be a benign lesion|This does not appear to be a concerning lesion.|is a benign solar|versicolor is not malignant|which is a benign", standard_simple, ignore.case = TRUE) ~ FALSE,
    TRUE ~ NA
  )) %>% 
  mutate(responded_expert_simple = case_when(
    grepl("cannot|not possible", expert_simple, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(expert_simple) ~ "Blocked",
    !is.na(one_word_expert_simple) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  )) %>% 
  mutate(responded_standard_simple = case_when(
    grepl("cannot|not possible", standard_simple, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(standard_simple) ~ "Blocked",
    !is.na(one_word_standard_simple) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  ))

g2 <- inner_join(g2, ddi, by = c("Filename" = "DDI_file"))

g2 %>% 
  drop_na(one_word_expert_simple) %>% 
  mutate(expert_correct = if_else(one_word_expert_simple == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(expert_correct), N = n())

g2 %>% 
  drop_na(one_word_standard_simple) %>% 
  mutate(standard_correct = if_else(one_word_standard_simple == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(standard_correct), N = n())

#### GPT-4 ####
gpt4 <- read_csv("api_results/gpt4_responses.csv")
gpt4_missing <- read_csv("api_results/gpt4_responses_missing.csv")

gpt4_missing <- gpt4_missing %>% 
  filter((Filename %in% c("000424.png", "000441.png") & !grepl("expert", TextPrompt)) |
           (Filename %in% c("000482.png", "000520.png") & grepl("expert", TextPrompt)))

gpt_results <- tibble(rbind(gpt4, gpt4_missing))

# Remove the additional Response columns if not needed
gpt_results <- select(gpt_results, -starts_with("Response."))

g4 <- gpt_results %>% 
  mutate(prompt_version = case_when(
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "expert_simple",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "standard_simple",
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "expert_eng",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "standard_eng",
    TRUE ~ NA_character_  # default case if none of the above conditions are met
  )) %>% 
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert_eng = case_when(
    str_to_lower(expert_eng) %in% c("malignant", "malignant.") ~ TRUE,
    str_to_lower(expert_eng) %in% c("benign", "benign.") ~ FALSE,
    TRUE ~ NA 
  ))%>% 
  mutate(one_word_standard_eng = case_when(
    str_to_lower(standard_eng) %in% c("malignant", "malignant.") ~ TRUE,
    str_to_lower(standard_eng) %in% c("benign", "benign.") ~ FALSE,
    TRUE ~ NA 
  )) %>% 
  mutate(responded_expert_eng = case_when(
    grepl("cannot|not possible|sorry|As an AI", expert_eng, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(expert_eng) ~ "Blocked",
    !is.na(one_word_expert_eng) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  )) %>% 
  mutate(responded_standard_eng = case_when(
    grepl("cannot|not possible|sorry|As an AI", standard_eng, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(standard_eng) ~ "Blocked",
    !is.na(one_word_standard_eng) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  ))

g4 <- inner_join(g4, ddi, by = c("Filename" = "DDI_file"))

g4 %>% 
  mutate(standard_eng_refusal = if_else(responded_standard_eng == "Refused/Undetermined", 1, 0)) %>% 
  mutate(expert_eng_refusal = if_else(responded_expert_eng == "Refused/Undetermined", 1, 0)) %>% 
  summarise(sum_std = sum(standard_eng_refusal, na.rm = T), sum_exp = sum(expert_eng_refusal, na.rm = T))

g4 %>% 
  drop_na(one_word_expert_eng) %>% 
  mutate(expert_correct = if_else(one_word_expert_eng == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(expert_correct), N = n())

g4 %>% 
  drop_na(one_word_standard_eng) %>% 
  mutate(standard_correct = if_else(one_word_standard_eng == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(standard_correct), N = n())


#### Get the new prompt responses ####
df_gem6 <- read_csv("api_results/gemini_ddi_results_p34_1.csv")
df_gem7 <- read_csv("api_results/gemini_ddi_results_missing_p34_1v1.csv")

# Merge the data frames
g6 <- df_gem6 %>%
  left_join(df_gem7, by = c("Filename", "TextPrompt"), suffix = c("", ".3")) 

# Fill NA values in 'Response' column until max responses 
g6 <- g6 %>%
  mutate(Response = coalesce(Response, Response.3))

# Remove the additional Response columns if not needed
g6 <- select(g6, -starts_with("Response."))


g6 <- g6 %>% 
  mutate(prompt_version = case_when(
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "expert_simple",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "standard_simple",
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "expert_eng",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "standard_eng",
    TRUE ~ NA_character_  # default case if none of the above conditions are met
  )) %>%
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert_eng = case_when(
    str_to_lower(expert_eng) %in% c("malignant", "malignant.", "this lesion is malignant.", "the lesion is malignant.") ~ TRUE,
    grepl("likely malignant|concerning for melanoma|it is malignant|- malignant|concerning for basal", expert_eng, ignore.case = TRUE) ~ TRUE,
    str_to_lower(expert_eng) %in% c("benign", "benign.", "this lesion is benign.", "the lesion is benign.", "This skin lesion is benign.") ~ FALSE,
    grepl("likely benign|- benign|This skin lesion is benign.|appears to be a benign lesion|This does not appear to be a concerning lesion.|is a benign solar|versicolor is not malignant|which is a benign", expert_eng, ignore.case = TRUE) ~ FALSE,
    TRUE ~ NA
  )) %>% 
  mutate(one_word_standard_eng = case_when(
    str_to_lower(standard_eng) %in% c("malignant", "malignant.", "this lesion is malignant.", "the lesion is malignant.") ~ TRUE,
    grepl("likely malignant|concerning for melanoma|it is malignant|- malignant|concerning for basal", standard_eng, ignore.case = TRUE) ~ TRUE,
    str_to_lower(standard_eng) %in% c("benign", "benign.", "this lesion is benign.", "the lesion is benign.", "This skin lesion is benign.") ~ FALSE,
    grepl("likely benign|- benign|This skin lesion is benign.|appears to be a benign lesion|This does not appear to be a concerning lesion.|is a benign solar|versicolor is not malignant|which is a benign", standard_eng, ignore.case = TRUE) ~ FALSE,
    TRUE ~ NA
  )) %>% 
  mutate(responded_expert_eng = case_when(
    grepl("cannot|not possible|impossible to tell", expert_eng, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(expert_eng) ~ "Blocked",
    !is.na(one_word_expert_eng) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  )) %>% 
  mutate(responded_standard_eng = case_when(
    grepl("cannot|not possible|impossible to tell", standard_eng, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(standard_eng) ~ "Blocked",
    !is.na(one_word_standard_eng) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  ))

g6 <- inner_join(g6, ddi, by = c("Filename" = "DDI_file"))

g6 %>% count(responded_expert_eng)
g6 %>% count(responded_standard_eng)

df_gpt6 <- read_csv("api_results/gpt4_responses_p12_1.csv")

g8 <- df_gpt6 %>% 
  mutate(prompt_version = case_when(
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "expert_simple",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "an image of a skin lesion") ~ "standard_simple",
    str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "expert_eng",
    !str_detect(TextPrompt, "expert dermatologist") & str_detect(TextPrompt, "matching game") ~ "standard_eng",
    TRUE ~ NA_character_  # default case if none of the above conditions are met
  )) %>% 
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert_simple = case_when(
    str_to_lower(expert_simple) %in% c("malignant", "malignant.") ~ TRUE,
    str_to_lower(expert_simple) %in% c("benign", "benign.") ~ FALSE,
    TRUE ~ NA 
  ))%>% 
  mutate(one_word_standard_simple = case_when(
    str_to_lower(standard_simple) %in% c("malignant", "malignant.") ~ TRUE,
    str_to_lower(standard_simple) %in% c("benign", "benign.") ~ FALSE,
    TRUE ~ NA 
  )) %>% 
  mutate(responded_expert_simple = case_when(
    grepl("cannot|not possible|sorry|As an AI", expert_simple, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(expert_simple) ~ "Blocked",
    !is.na(one_word_expert_simple) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  )) %>% 
  mutate(responded_standard_simple = case_when(
    grepl("cannot|not possible|sorry|As an AI", standard_simple, ignore.case = TRUE) ~ "Refused/Undetermined", 
    is.na(standard_simple) ~ "Blocked",
    !is.na(one_word_standard_simple) ~ "Satisfactory",
    TRUE ~ "Other"  # Default case
  ))

g8 <- inner_join(g8, ddi, by = c("Filename" = "DDI_file"))



#### Get intersection ####
# Function to filter datasets
filter_and_get_filenames <- function(dataset) {
  dataset %>%
    filter(if_all(starts_with("responded"), ~ . == "Satisfactory")) %>%
    distinct(Filename)
}

# Apply the function to each dataset
filenames_g2 <- filter_and_get_filenames(g2)
filenames_g4 <- filter_and_get_filenames(g4)
filenames_g6 <- filter_and_get_filenames(g6)

# View the results
filenames_g2
filenames_g4
filenames_g6

common_filenames <- Reduce(intersect, list(filenames_g2, filenames_g4, filenames_g6))

# View the common filenames
common_filenames


calculate_metrics_with_CI <- function(data, FST, column_name, n_bootstrap = 1000) {
  if (!column_name %in% names(data)) {
    # Return NA or another placeholder if the column does not exist
    return(data.frame(Accuracy = NA, Sensitivity = NA, Specificity = NA, Precision = NA, Recall = NA, F1 = NA, `Balanced Accuracy` = NA))
  }
  
  calculate_single_sample <- function() {
    sample_data <- data %>% 
      filter(skin_tone == FST) %>% 
      drop_na({{column_name}}) %>%
      sample_frac(1, replace = TRUE)
    
    cm <- confusionMatrix(as.factor(sample_data[[column_name]]), as.factor(sample_data$malignant), positive = "TRUE")
    
    return(data.frame(Accuracy = cm$overall['Accuracy'], 
                      Sensitivity = cm$byClass['Sensitivity'], 
                      Specificity = cm$byClass['Specificity'], 
                      Precision = cm$byClass['Precision'], 
                      Recall = cm$byClass['Recall'], 
                      F1 = cm$byClass['F1'],
                      `Balanced Accuracy` = cm$byClass['Balanced Accuracy']))
  }
  
  bootstrap_samples <- replicate(n_bootstrap, calculate_single_sample(), simplify = FALSE) %>% 
    bind_rows()
  
  metrics_with_CI <- bootstrap_samples %>%
    summarise(across(everything(), list(mean = mean, lower = ~quantile(., 0.025), upper = ~quantile(., 0.975))))
  
  return(metrics_with_CI)
}

# Lists of datasets and their names
datasets <- list(g2 = g2, g4 = g4, g6 = g6)
datasets <- list(g2 = g2 %>% filter(Filename %in% common_filenames$Filename), 
                 g4 = g4 %>% filter(Filename %in% common_filenames$Filename), 
                 g6 = g6 %>% filter(Filename %in% common_filenames$Filename))
dataset_names <- names(datasets)
skin_tones <- c('12', '34', '56')
columns <- c('one_word_expert_simple', 'one_word_standard_simple', 'one_word_expert_eng', 'one_word_standard_eng')


results <- expand.grid(dataset_name = dataset_names, FST = skin_tones, column = columns) %>%
  mutate(metrics = pmap(list(dataset_name, FST, column), 
                        ~calculate_metrics_with_CI(datasets[[..1]], ..2, ..3))) %>%
  unnest(metrics)


# BOOTSTRAPPED METRICS
# Function to calculate metrics with bootstrapped CIs
calculate_metrics_with_CI <- function(data, FST, column_name, n_bootstrap = 1000) {
  # Function to calculate metrics for a single bootstrap sample
  calculate_single_sample <- function() {
    sample_data <- data %>% 
      filter(skin_tone == FST) %>% 
      drop_na({{column_name}}) %>%
      sample_frac(1, replace = TRUE)
    
    cm <- confusionMatrix(as.factor(sample_data[[column_name]]), as.factor(sample_data$malignant), positive = "TRUE")
    
    return(data.frame(Accuracy = cm$overall['Accuracy'], 
                      Sensitivity = cm$byClass['Sensitivity'], 
                      Specificity = cm$byClass['Specificity'], 
                      Precision = cm$byClass['Precision'], 
                      Recall = cm$byClass['Recall'], 
                      F1 = cm$byClass['F1'],
                      `Balanced Accuracy` = cm$byClass['Balanced Accuracy']))
  }
  
  # Generate bootstrap samples and calculate metrics
  bootstrap_samples <- replicate(n_bootstrap, calculate_single_sample(), simplify = FALSE) %>% 
    bind_rows()
  
  # Calculate means and CIs for each metric
  metrics_with_CI <- bootstrap_samples %>%
    summarise(across(everything(), list(mean = mean, lower = ~quantile(., 0.025), upper = ~quantile(., 0.975))))
  
  return(metrics_with_CI)
}

# Lists of datasets and their names
datasets <- list(g2 = g2, g4 = g4, g6 = g6)
dataset_names <- names(datasets)
skin_tones <- c('12', '34', '56')
columns <- c('one_word_expert_simple', 'one_word_standard_simple', 'one_word_expert_eng', 'one_word_standard_eng')

# Iterating over combinations and storing results with CIs
results <- expand.grid(dataset_name = dataset_names, FST = skin_tones, column = columns) %>%
  mutate(metrics = pmap(list(dataset_name, FST, column), 
                        ~calculate_metrics_with_CI(datasets[[..1]], ..2, ..3))) %>%
  unnest(metrics)


results <- results %>% 
  mutate(Model = case_when(dataset_name == "g2" ~ "Gemini Pro Vision",
                           dataset_name == "g6" ~ "Gemini Pro Vision",
                           dataset_name == "g4" ~ "GPT-4 Turbo Vision")) %>% 
  select(-dataset_name) %>% 
  drop_na(Accuracy_mean)

#### Write Results ####
# write_csv(results, "bootstrapped_metrics_all.csv")
# write_csv(results, "bootstrapped_metrics_intersection.csv")
results <- read_csv("classification_results/bootstrapped_metrics_all.csv")
results <- read_csv("classification_results/bootstrapped_metrics_intersection.csv")

derm_ensemble <- data.frame(FST = c('I-II', 'V-VI'), Sensitivity = c(.84, 0.4), Specificity = c(0.6, 0.79))
derm_ensemble <- derm_ensemble %>% 
  mutate(TPR = Sensitivity, FPR = 1-Specificity) %>% 
  select(FST, TPR, FPR)

results %>%
  mutate(FST = case_when(FST == "12" ~ "I-II", FST == "34" ~ "III-IV", FST == "56" ~ "V-VI")) %>% 
  mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
  mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
         FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
  mutate(Prompt = case_when(column == "one_word_expert_simple" ~ "P2 - Expert Simple",
                            column == "one_word_standard_simple" ~ "P1 - Non-Expert Simple",
                            column == "one_word_expert_eng" ~ "P4 - Expert Engineered",
                            column == "one_word_standard_eng" ~ "P3 - Non-Expert Engineered")) %>% 
  ggplot() +
  geom_errorbar(aes(x = FPR, y = TPR, ymin = TPR_lower, ymax = TPR_upper), alpha = 0.7) +
  geom_errorbar(aes(x = FPR, y = TPR, xmin = FPR_lower, xmax = FPR_upper), alpha = 0.7) +
  geom_point(aes(x = FPR, y = TPR, color = Model, shape = Prompt), size = 2) +
  geom_point(data = derm_ensemble, aes(x = FPR, y = TPR), shape = 4, size = 2) +
  geom_point(aes(x = -1, y = -1, shape = "Dermatologist Ensemble"), color = "black", size = 2) +  # Dummy point for legend
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~FST) +
  xlim(0, 1) + 
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Gemini vs. GPT-4 Malignant/Benign Classification Performance by FST", x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", 
       color = "Model", shape = "Prompt") +  # Updated legend title
  scale_shape_manual(values = c(`P2 - Expert Simple` = 16, `P1 - Non-Expert Simple` = 1, `P4 - Expert Engineered` = 15, `P3 - Non-Expert Engineered` = 0, "Dermatologist Ensemble" = 4)) +  # Manual shape mapping
  scale_color_brewer(palette = "Set1") +
  guides(shape = guide_legend(override.aes = list(color = "black")))  # Ensure color consistency in legend


results %>%
  mutate(FST = case_when(FST == "12" ~ "I-II", FST == "34" ~ "III-IV", FST == "56" ~ "V-VI")) %>% 
  mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
  mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
         FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
  mutate(Prompt = case_when(column == "one_word_expert_simple" ~ "P2 - Expert Simple",
                            column == "one_word_standard_simple" ~ "P1 - Non-Expert Simple",
                            column == "one_word_expert_eng" ~ "P4 - Expert Engineered",
                            column == "one_word_standard_eng" ~ "P3 - Non-Expert Engineered")) %>% 
  ggplot() +
  geom_errorbar(aes(x = FPR, y = TPR, ymin = TPR_lower, ymax = TPR_upper), alpha = 0.4) +
  geom_errorbar(aes(x = FPR, y = TPR, xmin = FPR_lower, xmax = FPR_upper), alpha = 0.4) +
  geom_point(aes(x = FPR, y = TPR, color = Model, shape = Prompt), size = 3) +
  geom_point(data = derm_ensemble, aes(x = FPR, y = TPR), shape = 4, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~FST) +
  xlim(0, 1) + 
  ylim(0, 1) +
  theme_minimal(base_size = 14) +  # Adjust base font size
  theme(legend.title = element_text(size = 12),  # Adjust legend title font size
        legend.text = element_text(size = 10),  # Adjust legend text font size
        axis.title = element_text(size = 12),  # Adjust axis title font size
        axis.text = element_text(size = 10),  # Adjust axis text font size
        strip.text = element_text(size = 12), 
        legend.position = "bottom") +  # Adjust facet label font size
  labs(title = "Gemini Pro Vision vs. GPT-4V Classification Performance by FST", 
       x = "False Positive Rate (FPR)", 
       y = "True Positive Rate (TPR)", 
       color = "Model", 
       shape = "Prompt") +
  scale_shape_manual(values = c(`P2 - Expert Simple` = 16, `P1 - Non-Expert Simple` = 1, `P4 - Expert Engineered` = 15, `P3 - Non-Expert Engineered` = 0, "Dermatologist Ensemble" = 4)) +  # Manual shape mapping
  scale_color_brewer(palette = "Set1") +
  guides(shape = guide_legend(override.aes = list(color = "black")))+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),  # Adjust the number of rows and orientation for the color legend
         shape = guide_legend(nrow = 2, byrow = TRUE))

wide_results <- results %>%
  mutate(Model_Prompt = case_when(
    str_detect(column, "standard_simple") & str_detect(Model, "GPT") ~ "GPT-4 - P1",
    str_detect(column, "expert_simple") & str_detect(Model, "GPT") ~ "GPT-4 - P2",
    str_detect(column, "standard_eng") & str_detect(Model, "GPT") ~ "GPT-4 - P3",
    str_detect(column, "expert_eng") & str_detect(Model, "GPT") ~ "GPT-4 - P4",
    str_detect(column, "standard_simple") & str_detect(Model, "Gemini") ~ "Gemini Pro - P1",
    str_detect(column, "expert_simple") & str_detect(Model, "Gemini") ~ "Gemini Pro - P2",
    str_detect(column, "standard_eng") & str_detect(Model, "Gemini") ~ "Gemini Pro - P3",
    str_detect(column, "expert_eng") & str_detect(Model, "Gemini") ~ "Gemini Pro - P4")) %>% 
  # Create a new identifier for each metric and FST combination
  mutate(metric_FST = paste(column, FST, sep = "_")) %>%
  # Select the columns we want to spread and the identifier we just created
  select(Model_Prompt, FST, ends_with("mean"), ends_with("lower"), ends_with("upper")) %>%
  drop_na(Accuracy_mean) %>% 
  # Spread the data to a wide format
  pivot_wider(names_from = FST, values_from = c(ends_with("mean"), ends_with("lower"), ends_with("upper"))) %>%
  # Optionally, reorder the columns as per your needs
  select(, sort(tidyselect::peek_vars()))

formatted_results <- wide_results %>%
  # Round the mean and CI columns to three digits
  mutate(across(contains("_mean_") | contains("_upper_") | contains("_lower_"), round, digits = 3)) %>% 
  # Iterate over each set of metric columns
  mutate(across(contains("_mean_"), .fns = list(mean_ci = ~str_c(
    round(., 3), " (+/-", round(. - get(str_replace(cur_column(), "mean", "lower")), 2), ")"
  )), .names = "{str_replace(.col, '_mean_', '_')}")) %>% 
  select(!contains(c("upper", "lower", "mean")))


# View the table
View(formatted_results)

# write.csv(formatted_results, "Summarized_Results/Metric_tables_intersection.csv")

overall_results <- results %>%
  mutate(Model_Prompt = case_when(
    str_detect(column, "standard_simple") & str_detect(Model, "GPT") ~ "GPT-4 - P1",
    str_detect(column, "expert_simple") & str_detect(Model, "GPT") ~ "GPT-4 - P2",
    str_detect(column, "standard_eng") & str_detect(Model, "GPT") ~ "GPT-4 - P3",
    str_detect(column, "expert_eng") & str_detect(Model, "GPT") ~ "GPT-4 - P4",
    str_detect(column, "standard_simple") & str_detect(Model, "Gemini") ~ "Gemini Pro - P1",
    str_detect(column, "expert_simple") & str_detect(Model, "Gemini") ~ "Gemini Pro - P2",
    str_detect(column, "standard_eng") & str_detect(Model, "Gemini") ~ "Gemini Pro - P3",
    str_detect(column, "expert_eng") & str_detect(Model, "Gemini") ~ "Gemini Pro - P4")) %>%
  select(Model_Prompt, starts_with("Sensitivity_"), starts_with("Specificity_"), starts_with("Balanced.Accuracy_")) %>%
  group_by(Model_Prompt) %>%
  summarise(
    `Overall Sensitivity` = str_c(round(mean(Sensitivity_mean), 3), "(+/-", round(mean(Sensitivity_mean) - mean(Sensitivity_lower), 2), ")"),
    `Overall Specificity` = str_c(round(mean(Specificity_mean), 3), "(+/-", round(mean(Specificity_mean) - mean(Specificity_lower), 2), ")"),
    `Overall Balanced Accuracy` = str_c(round(mean(Balanced.Accuracy_mean), 3), "(+/-", round(mean(Balanced.Accuracy_mean) - mean(Balanced.Accuracy_lower), 2), ")")
  ) %>%
  ungroup()

# write.csv(overall_results, "Summarized_Results/Overall_metric_results_intersection.csv")
