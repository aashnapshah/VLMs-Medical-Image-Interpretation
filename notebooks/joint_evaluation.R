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

cm <- calculate_metrics_with_CI(dermatology_api_results, 'malignant', 2)

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
# 
# # Create the table
table_df <- t %>%
  select(Model, PromptID, Size, skin_tone, Balanced.Accuracy_CI, Sensitivity_CI, Specificity_CI) %>%
  pivot_wider(
    id_cols = c("Model", "PromptID"),
    names_from = skin_tone,
    values_from = c(ends_with("CI"))
  )

