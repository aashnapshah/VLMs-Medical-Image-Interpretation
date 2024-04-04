calculate_metrics_with_CI <- function(data, y_true_col, n_bootstrap = 1000) {
  column_name <- "PredictedDiagnosis"
  
  if (!column_name %in% names(data)) {
    return(data.frame(Accuracy = NA, Sensitivity = NA, Specificity = NA, Precision = NA, Recall = NA, F1 = NA, `Balanced Accuracy` = NA))
  }
  
  calculate_single_sample <- function(prompt_value) {
    sample_data <- data %>%
      filter(PromptID == prompt_value) %>%
      drop_na({{ column_name }}, {{ y_true_col }}) %>%
      sample_frac(1, replace = TRUE)
    
    sample_data[[column_name]] <- factor(sample_data[[column_name]], levels = c(FALSE, TRUE))
    sample_data[[y_true_col]] <- factor(sample_data[[y_true_col]], levels = c(FALSE, TRUE))
    
    cm <- confusionMatrix(as.factor(sample_data[[column_name]]), as.factor(sample_data[[y_true_col]]), positive = "TRUE")
    
    return(data.frame(PromptID = prompt_value,
                      Size = nrow(sample_data),
                      Accuracy = cm$overall["Accuracy"], 
                      Sensitivity = cm$byClass["Sensitivity"], 
                      Specificity = cm$byClass["Specificity"], 
                      Precision = cm$byClass["Precision"], 
                      Recall = cm$byClass["Recall"], 
                      F1 = cm$byClass["F1"],
                      `Balanced Accuracy` = cm$byClass["Balanced Accuracy"]))
  }
  
  prompts <- unique(data$PromptID)
  bootstrap_samples <- replicate(n_bootstrap, lapply(prompts, calculate_single_sample), simplify = FALSE) %>%
    bind_rows()
  
  metrics_with_CI <- bootstrap_samples %>%
    group_by(PromptID, Size) %>%
    summarise(across(everything(), list(mean = ~mean(., na.rm = TRUE), 
                                        lower = ~quantile(., 0.025, na.rm = TRUE), 
                                        upper = ~quantile(., 0.975, na.rm = TRUE))))
  return(metrics_with_CI)
}

calculate_group_metrics <- function(df, y_label, demographic_groups) {
  master_df <- data.frame()
  for (group in demographic_groups) {
    if (group == "Overall") {
      metrics <- df %>%
        group_by(Model, .groups = "drop") %>% 
        do({
          calculate_metrics_with_CI(., y_label, 2) %>%
            as.data.frame(metrics_result)
        }) %>% 
        ungroup()
      metrics$skin_tone <- "Overall"
      
    } else {
      unique_values <- unique(df[[group]])
      
      metrics <- df %>%
        group_by(Model, !!sym(group), .groups = "drop") %>%
        do({
          metrics_result <- calculate_metrics_with_CI(., y_label, 2)
          as.data.frame(metrics_result)
        }) %>%
        ungroup()
    }
    
    master_df <- rbind(master_df, metrics)
  }
  return(master_df)
}