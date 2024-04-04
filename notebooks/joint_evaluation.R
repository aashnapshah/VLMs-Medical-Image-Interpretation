library(tidyverse)

base_dir <- "/Users/aashnashah/Dropbox/Research/derm-gemini-vs-gpt4/"
source(paste0(base_dir, 'notebooks/cleaning_functions.R'))
source(paste0(base_dir, 'notebooks/plot_functions.R'))
source(paste0(base_dir, 'notebooks/metric_functions.R'))

# Define data directories
radiology_data_dir <- "data/radiology/"
dermatology_data_dir <- "data/dermatology/"

dermatology_api_results <- read_results(dermatology_data_dir, 'Dermatology')
radiology_api_results <- read_results(radiology_data_dir, 'Radiology')

# Clean the data
# for loop the dataframes

plot <- refusal_plot(radiology_api_results)
print(plot)

plot <- refusal_plot(dermatology_api_results)
print(plot)

demographic_groups <- c("Overall", "skin_tone") #Age", "Race", "GENDER")
main_df <- calculate_group_metrics(dermatology_api_results, 'malignant', demographic_groups)

t <- main_df %>%
  mutate(
    Balanced.Accuracy_CI = paste0(round(Balanced.Accuracy_mean, 2), " (+/- ",
                                  round(Balanced.Accuracy_mean - Balanced.Accuracy_lower, 2), ")"),
    Sensitivity_CI = paste0(round(Sensitivity_mean, 2), " (+/- ",
                            round(Sensitivity_mean - Sensitivity_lower, 2), ")"),
    Specificity_CI = paste0(round(Specificity_mean, 2), " (+/- ",
                            round(Specificity_mean - Specificity_lower, 2), ")")
  )

table_df <- t %>%
  select(Model, PromptID, Size, skin_tone, Balanced.Accuracy_CI, Sensitivity_CI, Specificity_CI) %>%
  pivot_wider(
    id_cols = c("Model", "PromptID"),
    names_from = skin_tone,
    values_from = c(ends_with("CI"))
  )

demo_df <- main_df %>% filter(Category == "skin_tone")
#demo_df <- demo_df[!(demo_df$Model == 'GPT-4 Vision' & demo_df$PromptID < 3), ]

mean_df <- demo_df %>%
  group_by(Model, PromptID) %>%
  summarize(mean_accuracy = mean(Balanced.Accuracy_mean))

# Merge mean data with filtered data
filtered_demo_df <- merge(demo_df, mean_df, by = c("PromptID", "Model"))
filtered_demo_df$PromptID <- as.factor(filtered_demo_df$PromptID)

plot <- plot_balanced_acc(filtered_demo_df)
print(plot)