library(tidyverse)
library(caret)
gemini_res <- read_csv("gemini_ddi_results_3.csv")
ddi <- read_csv("../DDI/ddi_metadata.csv")

g2 <- gemini_res %>% 
  mutate(prompt_version = if_else(str_detect(TextPrompt, "expert dermatologist"), 
                                  "expert", 
                                  "standard")) %>% 
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert = case_when(expert == "malignant" |
                                     expert == "This lesion is malignant." |
                                     expert == "The lesion is malignant."
                                     ~ TRUE,
                                     expert == "benign" |
                                       expert == "This lesion is benign." |
                                       expert == "The lesion is benign." 
                                     ~ FALSE,
                                     expert == TRUE ~ NA)) %>% 
  mutate(one_word_standard = case_when(standard == "malignant" |
                                         standard == "This lesion is malignant." |
                                         standard == "The lesion is malignant."
                                       ~ TRUE,
                                       standard == "benign" |
                                         standard == "This lesion is benign." |
                                         standard == "The lesion is benign." 
                                       ~ FALSE,
                                       expert == TRUE ~ NA))
  
  

g2 <- inner_join(g2, ddi, by = c("Filename" = "DDI_file"))

g2 %>% 
  drop_na(one_word_expert) %>% 
  mutate(expert_correct = if_else(one_word_expert == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(expert_correct), N = n())

g2 %>% 
  drop_na(one_word_standard) %>% 
  mutate(standard_correct = if_else(one_word_standard == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(standard_correct), N = n())




# Read the CSV files
df2 <- read_csv("gemini_ddi_results_3.csv")
df3 <- read_csv("gemini_ddi_results_missing_3v1.csv")
df4 <- read_csv("gemini_ddi_results_missing_3v2.csv")
df5 <- read_csv("gemini_ddi_results_missing_3v3.csv")
df6 <- read_csv("gemini_ddi_results_missing_3v4.csv")
df7 <- read_csv("gemini_ddi_results_missing_3v5.csv")

# Merge the data frames
result_df <- df2 %>%
  left_join(df3, by = c("Filename", "TextPrompt"), suffix = c("", ".3")) %>%
  left_join(df4, by = c("Filename", "TextPrompt"), suffix = c("", ".4")) %>%
  left_join(df5, by = c("Filename", "TextPrompt"), suffix = c("", ".5")) %>% 
  left_join(df6, by = c("Filename", "TextPrompt"), suffix = c("", ".6")) %>%
  left_join(df7, by = c("Filename", "TextPrompt"), suffix = c("", ".7"))

# Read the CSV files
df2 <- read_csv("gemini_ddi_results_4.csv")
df3 <- read_csv("gemini_ddi_results_missing_4v1.csv")
df4 <- read_csv("gemini_ddi_results_missing_4v2.csv")
df5 <- read_csv("gemini_ddi_results_missing_4v3.csv")

# Merge the data frames
result_df <- df2 %>%
  left_join(df3, by = c("Filename", "TextPrompt"), suffix = c("", ".3")) %>%
  left_join(df4, by = c("Filename", "TextPrompt"), suffix = c("", ".4")) %>%
  left_join(df5, by = c("Filename", "TextPrompt"), suffix = c("", ".5")) %>% 
  left_join(df6, by = c("Filename", "TextPrompt"), suffix = c("", ".6")) %>%
  left_join(df7, by = c("Filename", "TextPrompt"), suffix = c("", ".7"))

# Read the CSV files
df2 <- read_csv("gemini_ddi_results_2.csv")
df3 <- read_csv("gemini_ddi_results_missing_2v1.csv")
df4 <- read_csv("gemini_ddi_results_missing_2v2.csv")
df5 <- read_csv("gemini_ddi_results_missing_2v3.csv")

# Merge the data frames
result_df <- df2 %>%
  left_join(df3, by = c("Filename", "TextPrompt"), suffix = c("", ".3")) %>%
  left_join(df4, by = c("Filename", "TextPrompt"), suffix = c("", ".4")) %>%
  left_join(df5, by = c("Filename", "TextPrompt"), suffix = c("", ".5")) 

# Fill NA values in 'Response' column
result_df <- result_df %>%
  mutate(Response = coalesce(Response, Response.3, Response.4, Response.5, Response.6, Response.7))


# Fill NA values in 'Response' column
result_df <- result_df %>%
  mutate(Response = coalesce(Response, Response.3, Response.4, Response.5))

# Optionally, remove the additional Response columns if not needed
result_df <- select(result_df, -starts_with("Response."))

result_df %>%
  filter(Response != "malignant", Response != "malignant.", Response != "benign", Response != "benign.", Response != "Malignant", Response != "Malignant.", Response != "Benign", Response != "Benign.") %>% 
  filter(Response != "The lesion is malignant.", Response != "This lesion is malignant.",
         Response != "The lesion is benign.", Response != "This lesion is benign.") %>% 
  filter(!grepl("cannot|not possible", Response, ignore.case = TRUE)) %>% View()

g2 <- result_df %>% 
  mutate(prompt_version = if_else(str_detect(TextPrompt, "expert dermatologist"), 
                                  "expert", 
                                  "standard")) %>% 
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert = case_when(expert == "malignant" |
                                       expert == "This lesion is malignant." |
                                       expert == "The lesion is malignant."
                                     ~ TRUE,
                                     expert == "benign" |
                                       expert == "This lesion is benign." |
                                       expert == "The lesion is benign." 
                                     ~ FALSE,
                                     expert == TRUE ~ NA)) %>% 
  mutate(one_word_standard = case_when(standard == "malignant" |
                                         standard == "This lesion is malignant." |
                                         standard == "The lesion is malignant."
                                       ~ TRUE,
                                       standard == "benign" |
                                         standard == "This lesion is benign." |
                                         standard == "The lesion is benign." 
                                       ~ FALSE,
                                       expert == TRUE ~ NA))



g2 <- inner_join(g2, ddi, by = c("Filename" = "DDI_file"))

g2 %>% 
  drop_na(one_word_expert) %>% 
  mutate(expert_correct = if_else(one_word_expert == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(expert_correct), N = n())

g2 %>% 
  drop_na(one_word_standard) %>% 
  mutate(standard_correct = if_else(one_word_standard == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(standard_correct), N = n())


gpt4 <- read_csv("gpt4_responses.csv")
gpt42 <- read_csv("gpt4_responses_2.csv")

g4 <- gpt4 %>% 
  mutate(prompt_version = if_else(str_detect(TextPrompt, "expert dermatologist"), 
                                  "expert", 
                                  "standard")) %>%
  select(-TextPrompt) %>%
  pivot_wider(names_from = prompt_version, values_from = Response) %>% 
  mutate(one_word_expert = case_when(
    str_detect(expert, regex("malignant", ignore_case = TRUE)) ~ TRUE,
    str_detect(expert, regex("benign", ignore_case = TRUE)) ~ FALSE,
    TRUE ~ NA  # NA_real_ is used here to ensure the column is of type logical
  ))%>% 
  mutate(one_word_standard = case_when(
    str_detect(standard, regex("malignant", ignore_case = TRUE)) ~ TRUE,
    str_detect(standard, regex("benign", ignore_case = TRUE)) ~ FALSE,
    TRUE ~ NA  # NA_real_ is used here to ensure the column is of type logical
  ))

g4 <- inner_join(g4, ddi, by = c("Filename" = "DDI_file"))

g4 %>% 
  mutate(standard_refusal = if_else(str_detect(standard, "sorry"), 1, 0)) %>% 
  mutate(expert_refusal = if_else(str_detect(expert, "sorry"), 1, 0)) %>% 
  summarise(sum_std = sum(standard_refusal, na.rm = T), sum_exp = sum(expert_refusal, na.rm = T))
  

g4 %>% 
  drop_na(one_word_expert) %>% 
  mutate(expert_correct = if_else(one_word_expert == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(expert_correct), N = n())

g4 %>% 
  drop_na(one_word_standard) %>% 
  mutate(standard_correct = if_else(one_word_standard == malignant, TRUE, FALSE)) %>% 
  group_by(skin_tone) %>% 
  summarise(mean_acc = mean(standard_correct), N = n())


cm_dat <- g2 %>% 
  filter(skin_tone=='12') %>% 
  drop_na(one_word_expert) %>% 
  select(one_word_expert, malignant)

calculate_fpr_tpr <- function(predictions, actual) {
  # Create a confusion matrix
  conf_matrix <- table(Predicted = predictions, Actual = actual)
  
  # Calculate TPR and FPR
  TPR <- conf_matrix["TRUE", "TRUE"] / sum(conf_matrix[, "TRUE"])
  FPR <- conf_matrix["TRUE", "FALSE"] / sum(conf_matrix[, "FALSE"])
  
  list(FPR = FPR, TPR = TPR)
}

calculate_fpr_tpr(as.factor(cm_dat$one_word_expert), as.factor(cm_dat$malignant))

confusionMatrix(as.factor(cm_dat$one_word_expert), as.factor(cm_dat$malignant), positive = "TRUE") 
        
cm_dat <- g4 %>% 
  drop_na(one_word_standard) %>% 
  select(one_word_standard, malignant)

confusionMatrix(as.factor(cm_dat$one_word_standard), as.factor(cm_dat$malignant)) 



# Define a function to calculate metrics from confusion matrix
calculate_metrics <- function(data, FST, column_name) {
  cm_dat <- data %>%
    filter(skin_tone == FST) %>%
    drop_na({{column_name}}) %>%
    select({{column_name}}, malignant)
  
  
  cm <- confusionMatrix(as.factor(cm_dat[[column_name]]), as.factor(cm_dat$malignant), positive = "TRUE")
  return(data.frame(Accuracy = cm$overall['Accuracy'], 
                    Sensitivity = cm$byClass['Sensitivity'], 
                    Specificity = cm$byClass['Specificity'], 
                    Precision = cm$byClass['Precision'], 
                    Recall = cm$byClass['Recall'], 
                    F1 = cm$byClass['F1'],
                    `Balanced Accuracy` = cm$byClass['Balanced Accuracy']))
}

# Lists of datasets and their names
datasets <- list(g2 = g2, g4 = g4)
dataset_names <- names(datasets)
skin_tones <- c('12', '34', '56')
columns <- c('one_word_expert', 'one_word_standard')

# Iterating over combinations and storing results
# Iterating over combinations and storing results
results <- expand.grid(dataset_name = dataset_names, FST = skin_tones, column = columns) %>%
  mutate(metrics = pmap(list(dataset_name, FST, column), 
                        ~calculate_metrics(datasets[[..1]], ..2, ..3))) %>%
  unnest(metrics)




# BOOTSTRAPPED METRICS
# Function to calculate metrics with bootstrapped CIs
calculate_metrics_with_CI <- function(data, FST, column_name, n_bootstrap = 500) {
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
datasets <- list(g2 = g2, g4 = g4)
dataset_names <- names(datasets)
skin_tones <- c('12', '34', '56')
columns <- c('one_word_expert', 'one_word_standard')

# Iterating over combinations and storing results with CIs
results <- expand.grid(dataset_name = dataset_names, FST = skin_tones, column = columns) %>%
  mutate(metrics = pmap(list(dataset_name, FST, column), 
                        ~calculate_metrics_with_CI(datasets[[..1]], ..2, ..3))) %>%
  unnest(metrics)






results <- results %>% 
  mutate(Model = case_when(dataset_name == "g2" ~ "Gemini Pro Vision",
                           dataset_name == "g4" ~ "GPT-4 Turbo Vision")) %>% 
  select(-dataset_name)

write.csv(results, "bootstrapped_metrics.csv")
results <- read_csv("bootstrapped_metrics.csv")
derm_ensemble <- data.frame(FST = c('I-II', 'V-VI'), Sensitivity = c(.84, 0.4), Specificity = c(0.6, 0.79))
derm_ensemble <- derm_ensemble %>% 
  mutate(TPR = Sensitivity, FPR = 1-Specificity) %>% 
  select(FST, TPR, FPR)

results %>%
  select(Model, column, FST, Sensitivity_mean, Sensitivity_lower, Sensitivity_upper, Specificity_mean, Specificity_lower, Specificity_upper) %>%
  filter(FST != "34") %>% 
  mutate(FST = case_when(FST == "12" ~ "I-II", FST == "56" ~ "V-VI")) %>% 
  mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
  mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
         FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
  ggplot() +
  geom_point(aes(x = FPR, y = TPR, color = Model, shape = column)) +
  facet_wrap(~FST) +
  xlim(0, 1) + 
  ylim(0, 1) +
  theme_minimal() +
  labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", 
       color = "Model", shape = "Column") +
  scale_color_brewer(palette = "Set1") + 
  geom_point(data = derm_ensemble, aes(x = FPR, y = TPR), shape = 4, size = 2)

results %>%
  mutate(FST = case_when(FST == "12" ~ "I-II", FST == "34" ~ "III-IV", FST == "56" ~ "V-VI")) %>% 
  mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
  mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
         FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
  mutate(Prompt = case_when(column == "one_word_expert" ~ "Expert Prompt",
                            column == "one_word_standard" ~ "Standard Prompt")) %>% 
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
  scale_shape_manual(values = c(`Expert Prompt` = 16, `Standard Prompt` = 17, "Dermatologist Ensemble" = 4)) +  # Manual shape mapping
  scale_color_brewer(palette = "Set1") +
  guides(shape = guide_legend(override.aes = list(color = "black"))) +  # Ensure color consistency in legend
  theme(legend.position = "bottom")

#### Main figure ####

results %>%
  mutate(FST = case_when(FST == "12" ~ "I-II", FST == "34" ~ "III-IV", FST == "56" ~ "V-VI")) %>% 
  mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
  mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
         FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
  mutate(Prompt = case_when(column == "one_word_expert" ~ "Expert Prompt",
                            column == "one_word_standard" ~ "Standard Prompt")) %>% 
  ggplot() +
  geom_errorbar(aes(x = FPR, y = TPR, ymin = TPR_lower, ymax = TPR_upper), width=0.01, alpha = 0.7) +
  geom_errorbar(aes(x = FPR, y = TPR, xmin = FPR_lower, xmax = FPR_upper), height=0.01, alpha = 0.7) +
  geom_point(aes(x = FPR, y = TPR, color = Model, shape = Prompt), size = 3) +
  geom_point(data = derm_ensemble, aes(x = FPR, y = TPR), shape = 4, size = 3) +
  geom_point(aes(x = -1, y = -1, shape = "Dermatologist Ensemble"), color = "black", size = 3) +  # Dummy point for legend
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~FST) +
  xlim(0, 1) + 
  ylim(0, 1) +
  theme_minimal() +
  theme(text = element_text(size = 12), legend.position = "bottom", legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  labs(title = "Gemini vs. GPT-4V Malignant/Benign Classification Performance by FST",
       x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", 
       color = "Model", shape = "Prompt") +
  scale_shape_manual(values = c(`Expert Prompt` = 16, `Standard Prompt` = 17, "Dermatologist Ensemble" = 4)) +
  scale_color_brewer(palette = "Set1", type = "qual") +
  guides(shape = guide_legend(override.aes = list(color = "black")))

ggsave("fig1.png", width = 8.5, height = 5, dpi = 300)
#### end figure ####


results %>% 
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>% 
  select(Model_Prompt, FST, Sensitivity) %>% 
  pivot_wider(names_from = c(FST), values_from = Sensitivity)  %>% 
  write_csv("Summarized_Results/sensitivity_fst.csv")

results %>% 
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>% 
  select(Model_Prompt, Sensitivity) %>%
  group_by(Model_Prompt) %>% 
  summarise(`Overall Sensitivity` = mean(Sensitivity)) %>% 
  write_csv("Summarized_Results/sensitivity_overall.csv")
  

results %>% 
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>% 
  select(Model_Prompt, FST, Specificity) %>% 
  pivot_wider(names_from = c(FST), values_from = Specificity) %>% 
  write_csv("Summarized_Results/specificity_fst.csv")

results %>% 
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>% 
  select(Model_Prompt, Specificity) %>%
  group_by(Model_Prompt) %>% 
  summarise(`Overall Specificity` = mean(Specificity)) %>% 
  write_csv("Summarized_Results/specificity_overall.csv")

results %>% 
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>% 
  select(Model_Prompt, FST, Balanced.Accuracy) %>% 
  pivot_wider(names_from = c(FST), values_from = Balanced.Accuracy) %>% 
  write_csv("Summarized_Results/balanced_acc_fst.csv")

results %>% 
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>% 
  select(Model_Prompt, Balanced.Accuracy) %>%
  group_by(Model_Prompt) %>% 
  summarise(`Overall Balanced.Accuracy` = mean(Balanced.Accuracy)) %>% 
  write_csv("Summarized_Results/balanced_acc_overall.csv")


library(gt)
# Create the Sensitivity comparison table
sensitivity_table <- results %>%
  mutate(Model_Prompt = ifelse(grepl("expert", column), paste(Model, "- Expert"), paste(Model, "- Standard"))) %>%
  select(Model_Prompt, FST, Sensitivity) %>%
  mutate(Sensitivity = round(Sensitivity, 3)) %>% 
  pivot_wider(names_from = FST, values_from = Sensitivity) %>%
  gt()

# Style the table
sensitivity_table <- sensitivity_table %>%
  cols_label(
    `Model_Prompt` = "Model - Prompt",
    `12` = "I-II",
    `34` = "III-IV",
    `56` = "V-VI"
  ) %>% 
  

# Export the table
gtsave(sensitivity_table, "sensitivity_table.png")

