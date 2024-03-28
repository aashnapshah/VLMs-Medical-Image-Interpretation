library(tidyverse)
library(caret)

# Read the DDI ground truth file
ddi <- read_csv("../../data/dermatology/DDI/ddi_metadata.csv")

gemini_results <- read_csv("../../data/dermatology/apiResults/gemini_ddi_results_20240326_single_word.csv")
gpt4_results <- read_csv("../../data/dermatology/apiResults/gpt4v_dermatology_results_20240318_single_word.csv")

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
      grepl("cannot|not possible|impossible to|too blurry|onion", !!sym(prompt_version), ignore.case = TRUE) ~ "Refused/Undetermined",
      is.na(!!sym(prompt_version)) ~ "Blocked",
      !is.na(!!sym(one_word_col)) ~ "Satisfactory",
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
      grepl("cannot|not possible|sorry|As an AI|I'm unable to provide this information|I'm sorry,|unable to|Unable to", !!sym(prompt_version), ignore.case = TRUE) ~ "Refused/Undetermined",
      is.na(!!sym(prompt_version)) ~ "Blocked",
      !is.na(!!sym(one_word_col)) ~ "Satisfactory",
      TRUE ~ "Other"  # Default case
    ))
  
  return(df)
}

#### Gemini ####
gemini_results <- gemini_results %>% 
  mutate(PromptID = paste0("P",PromptID+1)) 

gemini_wide <- gemini_results %>% 
  mutate(model = "Gemini-Pro-Vision") %>% 
  pivot_wider(names_from = PromptID, values_from = Response)

for (version in unique(gemini_results$PromptID)) {
  gemini_wide <- analyze_prompt_version_gemini(gemini_wide, version)
}

gemini_wide <- inner_join(gemini_wide, ddi, by = c("Filename" = "DDI_file"))

#### GPT4 ####
gpt4_results <- gpt4_results %>% 
  mutate(model = "GPT-4V") %>% 
  mutate(PromptID = paste0("P",PromptID+1))

gpt4_wide <- gpt4_results %>% 
  pivot_wider(names_from = PromptID, values_from = Response)

for (version in unique(gpt4_results$PromptID)) {
  gpt4_wide <- analyze_prompt_version_gpt4(gpt4_wide, version)
}

gpt4_wide <- inner_join(gpt4_wide, ddi,  by = c("Filename" = "DDI_file"))


#### Combine results ####
combined_wide <- bind_rows(gemini_wide, gpt4_wide) %>% 
  select(!contains("..."))

#### Bootstrap Metrics Functions w/CIs ####
calculate_metrics_with_CI <- function(data, model, FST, column_name, n_bootstrap = 1000) {
  if (!column_name %in% names(data)) {
    print("column name not found")
    return(data.frame(Accuracy = NA, Sensitivity = NA, Specificity = NA, Precision = NA, Recall = NA, F1 = NA, `Balanced Accuracy` = NA))
  }
  
  calculate_single_sample <- function() {
    sample_data <- data %>% 
      filter(skin_tone == FST) %>% 
      filter(model == model) %>% 
      drop_na({{column_name}}, malignant) %>%
      sample_frac(1, replace = TRUE)
    
    # Ensure that both TRUE and FALSE levels exist
    sample_data[[column_name]] <- factor(sample_data[[column_name]], levels = c("FALSE", "TRUE"))
    sample_data$malignant <- factor(sample_data$malignant, levels = c("FALSE", "TRUE"))
    
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
    summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        lower = ~quantile(., 0.025, na.rm = TRUE), 
                                        upper = ~quantile(., 0.975, na.rm = TRUE))))
  
  return(metrics_with_CI)
}

calculate_metrics_overall <- function(data, model, column_name, n_bootstrap = 1000) {
  if (!column_name %in% names(data)) {
    return(data.frame(Accuracy = NA, Sensitivity = NA, Specificity = NA, Precision = NA, Recall = NA, F1 = NA, `Balanced Accuracy` = NA))
  }
  
  calculate_single_sample <- function() {
    sample_data <- data %>% 
      filter(model == model) %>% 
      drop_na({{column_name}}, malignant) %>%
      sample_frac(1, replace = TRUE)
    
    # Ensure that both TRUE and FALSE levels exist
    sample_data[[column_name]] <- factor(sample_data[[column_name]], levels = c("FALSE", "TRUE"))
    sample_data$malignant <- factor(sample_data$malignant, levels = c("FALSE", "TRUE"))
    
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
    summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        lower = ~quantile(., 0.025, na.rm = TRUE), 
                                        upper = ~quantile(., 0.975, na.rm = TRUE))))
  
  return(metrics_with_CI)
}

#### Create eval datasets ####
model_names <- c("Gemini-Pro-Vision", "GPT-4V")
skin_tones <- c('12', '34', '56')
columns <- c('one_word_P1', 'one_word_P2', 'one_word_P3', 'one_word_P4', 'one_word_P5', 'one_word_P6', 'one_word_P7', 'one_word_P8')

results <- expand.grid(model_name = model_names, FST = skin_tones, column = columns) %>%
  mutate(metrics = pmap(list(model_name, FST, column), 
                        ~calculate_metrics_with_CI(combined_wide,..1, ..2, ..3))) %>%
  unnest(metrics)

results <- results %>% 
  mutate(Model = case_when(
    grepl("gemini", model_name, ignore.case = TRUE) ~ "Gemini Pro Vision",
    grepl("gpt", model_name, ignore.case = TRUE) ~ "GPT-4 with Vision",
    TRUE ~ NA_character_ # Default case if neither pattern is found
  )) %>% 
  mutate(Prompt = str_replace(column, "one_word_", "")) %>%
  drop_na(Accuracy_mean)


results_overall <- expand.grid(model_name = model_names, column = columns) %>%
  mutate(metrics = pmap(list(model_name, column), 
                        ~calculate_metrics_overall(combined_wide, ..1, ..2))) %>%
  unnest(metrics)

results_overall <- results_overall %>% 
  mutate(Model = case_when(
    grepl("gemini", model_name, ignore.case = TRUE) ~ "Gemini Pro Vision",
    grepl("gpt", model_name, ignore.case = TRUE) ~ "GPT-4 with Vision",
    TRUE ~ NA_character_ # Default case if neither pattern is found
  )) %>% 
  mutate(Prompt = str_replace(column, "one_word_", "")) %>%
  drop_na(Accuracy_mean)

# Format and write summary results
results_overall <- results_overall %>%
  select(Model, Prompt, starts_with("Sensitivity_"), starts_with("Specificity_"), starts_with("Balanced.Accuracy_")) %>%
  group_by(Model, Prompt) %>%
  summarise(
    `Overall Sensitivity` = str_c(round((Sensitivity_mean), 2), " (+/-", round((Sensitivity_mean) - (Sensitivity_lower), 2), ")"),
    `Overall Specificity` = str_c(round((Specificity_mean), 2), " (+/-", round((Specificity_mean) - (Specificity_lower), 2), ")"),
    `Overall Balanced Accuracy` = str_c(round((Balanced.Accuracy_mean), 2), " (+/-", round((Balanced.Accuracy_mean) - (Balanced.Accuracy_lower), 2), ")")
  ) %>%
  ungroup()

wide_results <- results %>%
  mutate(Model_Prompt = paste(Model, "-", Prompt)) %>% 
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
  mutate(across(contains("_mean_") | contains("_upper_") | contains("_lower_"), round, digits = 2)) %>% 
  # Iterate over each set of metric columns
  mutate(across(contains("_mean_"), .fns = list(mean_ci = ~str_c(
    round(., 3), " (+/-", round(. - get(str_replace(cur_column(), "mean", "lower")), 2), ")"
  )), .names = "{str_replace(.col, '_mean_', '_')}")) %>% 
  select(!contains(c("upper", "lower", "mean")))


# View the table
View(formatted_results)

# Dermatologist results from the Science Advances paper (Daneshjou et al 2022)
derm_ensemble <- data.frame(FST = c('I-II', 'V-VI'), Sensitivity = c(.84, 0.4), Specificity = c(0.6, 0.79))
derm_ensemble <- derm_ensemble %>% 
  mutate(TPR = Sensitivity, FPR = 1-Specificity) %>% 
  select(FST, TPR, FPR)

##### TPR x FPR plot ####
p <- results %>%
  filter(!(Model == "GPT-4 with Vision" & Prompt %in% c("P1", "P2"))) %>% 
  mutate(FST = case_when(FST == "12" ~ "I-II", FST == "34" ~ "III-IV", FST == "56" ~ "V-VI")) %>% 
  mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
  mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
         FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
  mutate(Prompt = factor(Prompt, levels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8"))) %>%
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
  scale_shape_manual(values = c("P1" = 16, "P2" = 1, "P3" = 15, "P4" = 0, "P5" = 17, "P6" = 2, "P7" = 18, "P8" = 5, "Dermatologist Ensemble" = 4)) +  # Manual shape mapping
  scale_color_brewer(palette = "Set1") +
  guides(shape = guide_legend(override.aes = list(color = "black")))+
  guides(color = guide_legend(nrow = 2, byrow = TRUE),  # Adjust the number of rows and orientation for the color legend
         shape = guide_legend(nrow = 2, byrow = TRUE))

p

write_csv(results, paste0("../../data/dermatology/classification_results/bootstrapped_fst_metrics_all.csv"))
write_csv(results_overall, paste0("../../data/dermatology/classification_results/overall_metrics_all.csv"))
write_csv(formatted_results, paste0("../../data/dermatology/classification_results/metric_tables_all.csv"))
ggsave("figures/TPR_x_FPR.png", plot = p, width = 10, height = 6)

# Response rates ####
combined_wide %>% 
  select(Filename, model, skin_tone, contains("responded"))

gemini_wide %>% count(responded_P8)

gemini_wide %>%
  pivot_longer(cols = starts_with("responded_P"), 
               names_to = "responded", 
               values_to = "value") %>%
  group_by(model, responded, value) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = value, values_from = count)

gpt4_wide %>%
  pivot_longer(cols = starts_with("responded_P"), 
               names_to = "responded", 
               values_to = "value") %>%
  group_by(model, responded, value) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = value, values_from = count)

