library(tidyverse)
library(reshape2)
library(caret)

base_dir <- "/Users/aashnashah/Dropbox/Research/derm-gemini-vs-gpt4/"

source(paste0(base_dir, 'notebooks/cleaning_functions.R'))
source(paste0(base_dir, 'notebooks/plot_functions.R'))
source(paste0(base_dir, 'notebooks/metric_functions.R'))

get_overlap_df <- function(df, accept_prompts) {
  filtered_df <- df %>%
    mutate(ID = as.numeric(gsub("\\D", "", PromptID))) %>%
    filter((Model == "GPT-4 Vision" & ID %in% accept_prompts | Model == "Gemini Pro Vision")  & !is.na(PredictedDiagnosis))
  
  filenames_list <- filtered_df$Filename
  filename_counts <- table(filenames_list)
  
  len_expected = length(accept_prompts) + 8
  print(len_expected)
  filenames_at_least_12 <- names(filename_counts[filename_counts >= len_expected])
  overlap_filtered_df <- filtered_df %>% filter(Filename %in% filenames_at_least_12)
  
  return(overlap_filtered_df)
}
  
# Define data directories
radiology_data_dir <- "data/radiology/"
dermatology_data_dir <- "data/dermatology/"
gastrology_data_dir <- "data/gastrology/"

dermatology_api_results <- read_results(dermatology_data_dir, 'Dermatology')
radiology_api_results <- read_results(radiology_data_dir, 'Radiology')
gastrology_api_results <- read_results(gastrology_data_dir, 'Gastrology')

### REFUSAL PLOTS ###
refusal_rate <- refusal_rates(radiology_api_results)
plot <- refusal_plot(refusal_rate)
ggsave(paste0(base_dir, "figures/radiology_refusal_plot.png"), plot, width = 10, dpi=300, height = 3.1)

refusal_rate <- refusal_rates(dermatology_api_results)
plot <- refusal_plot(refusal_rate)
ggsave(paste0(base_dir, "figures/dermatology_refusal_plot.png"), plot, width = 10, dpi=300, height = 3.1)

refusal_rate <- refusal_rates(gastrology_api_results)
plot <- refusal_plot(refusal_rate)
ggsave(paste0(base_dir, "figures/gastrology_refusal_plot.png"), plot, width = 10, dpi=300, height = 3.1)

# Get the overlap between the two models
dermatology_api_results_overlap <- get_overlap_df(dermatology_api_results, c(4, 7, 8))
radiology_api_results_overlap <- get_overlap_df(radiology_api_results, c(3, 4, 5, 6, 7, 8))
gastrology_api_results_overlap <- get_overlap_df(gastrology_api_results, c(3, 4, 5, 6, 7, 8))

unique_combinations_count <- radiology_api_results_overlap %>%
  select(PromptID, Model) %>%  # Select the columns of interest
  distinct() %>%  # Get unique rows
  count() 

dermatology_data_frames <- list(
  all = dermatology_api_results,
  overlap = dermatology_api_results_overlap
)

for (subset in names(dermatology_data_frames)) {
  df <- dermatology_data_frames[[subset]]
  print(paste("Analyzing subset:", subset))

  ### SAVE TABLE WITH ALL RESULTS ### 
  demographic_groups <- c("Overall", "skin_tone") #Age", "Race", "GENDER")
  outfile_name <- paste0(base_dir, "tables/dermatology_balanced_acc_group_", subset, ".csv")
  if (file.exists(outfile_name)){
    print("Reading from file")
    derm_main_df <- read.csv(outfile_name)
  } else {
    derm_main_df <- calculate_group_metrics(df, 'malignant', demographic_groups)
    write.csv(derm_main_df, file = outfile_name)
  }
  
  ### SAVE THE FORMATTED TABLE ###
  category <- "skin_tone"
  formatted_main_df <- format_metrics(derm_main_df, category)
  outfile_name <- paste0(base_dir, "tables/dermatology_balanced_acc_", category, "_", subset, "_formatted.csv")
  write.csv(formatted_main_df, file = outfile_name)
  
  plot <- plot_fpr_tpr(derm_main_df, "skin_tone")
  ggsave(paste0(base_dir, "figures/dermatology_fpr_tpr_skin_tone_", subset, ".png"), plot, width = 12, height = 5,  dpi=300)
  
  ### PLOT THE BALANCED ACCURACY ###
  plot <- plot_balanced_acc(derm_main_df, "skin_tone")
  ggsave(paste0(base_dir, "figures/dermatology_balanced_acc_skin_tone_", subset, ".png"), plot, width = 12, height = 5,  dpi=300)
}


### RADIOLOGY ANALYSIS ###
radiology_data_frames <- list(
  all = radiology_api_results,
  overlap = radiology_api_results_overlap
)

for (subset in names(radiology_data_frames)) {
  df <- radiology_data_frames[[subset]]
  print(paste("Analyzing subset:", subset))
  
  demographic_groups <- c("Overall", "Age", "Race", "GENDER")
  outfile_name <- paste0(base_dir, "tables/radiology_balanced_acc_group_", subset, ".csv")
  if (file.exists(outfile_name)){
    rad_main_df <- read.csv(outfile_name)
  } else {
    rad_main_df <- calculate_group_metrics(df, 'abnormal', demographic_groups)
    write.csv(rad_main_df, file = outfile_name)
  }

  category <- c("Age")
  formatted_main_df <- lapply(category, function(x) format_metrics(rad_main_df, x))
  outfile_name <- paste0(base_dir, "tables/radiology_balanced_acc_", category, "_", subset, "_formatted.csv")
  write.csv(formatted_main_df, file = outfile_name)

  plot <- plot_balanced_acc(rad_main_df, "Age")
  ggsave(paste0(base_dir, "figures/radiology_balanced_acc_age_", subset, ".png"), plot, width = 12, height = 5, dpi=300)
  
  plot <- plot_fpr_tpr(rad_main_df, "Age")
  ggsave(paste0(base_dir, "figures/radiology_fpr_tpr_age_", subset, ".png"), plot, width = 12, height = 5, dpi=300)

}

### RADIOLOGY ANALYSIS ###
gastrology_data_frames <- list(
  all = gastrology_api_results,
  overlap = gastrology_api_results_overlap
)

for (subset in names(gastrology_data_frames)) {
  df <- gastrology_data_frames[[subset]]
  print(paste("Analyzing subset:", subset))
  
  demographic_groups <- c("Overall", "Brightness")
  outfile_name <- paste0(base_dir, "tables/gastrology_balanced_acc_group_", subset, ".csv")
  if (file.exists(outfile_name)){
    gastro_main_df <- read.csv(outfile_name)
  } else {
    gastro_main_df <- calculate_group_metrics(df, 'label', demographic_groups)
    write.csv(gastro_main_df, file = outfile_name)
  }
  
  category <- c("Brightness")
  formatted_main_df <- lapply(category, function(x) format_metrics(gastro_main_df, x))
  outfile_name <- paste0(base_dir, "tables/gastro_balanced_acc_", category, "_", subset, "_formatted.csv")
  write.csv(formatted_main_df, file = outfile_name)
  
  plot <- plot_balanced_acc(gastro_main_df, "Brightness")
  ggsave(paste0(base_dir, "figures/gastro_balanced_acc_age_", subset, ".png"), plot, width = 12, height = 5, dpi=300)
  
  plot <- plot_fpr_tpr(gastro_main_df, "Brightness")
  ggsave(paste0(base_dir, "figures/gastro_fpr_tpr_age_", subset, ".png"), plot, width = 12, height = 5, dpi=300)
  
}



