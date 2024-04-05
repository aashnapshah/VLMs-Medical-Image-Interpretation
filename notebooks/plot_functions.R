
refusal_plot <- function(refusal_rates){

  plot <- ggplot(refusal_rates, aes(x = Model, y = Rate, fill = Model, shape = PromptID)) +
    geom_point(size = 4, position = position_dodge(width = 0.1), aes(color = Model), stroke = 1.0, alpha = 1) +
    scale_fill_brewer(palette = "Set1") +
    labs(x = "", y = "Fraction") +
    scale_shape_manual(values = c(16, 1, 15, 0, 17, 2, 18, 5, 4)) +  # Manual shape mapping
    scale_color_brewer(palette = "Set1") +
    facet_grid(RateType ~ PromptID, scales = "fixed") + 
    theme_minimal() +# Adjusted facet_grid layout
    theme(
      axis.ticks.x = element_blank(),  # Hide x-axis ticks
      axis.text.x = element_blank(),  # Hide x-axis ticks
      legend.text = element_text(size = 10), 
      panel.background = element_rect(fill = "white"),  # Set background color to white
      panel.grid.major = element_line(color = "gray93"), # Customize major grid lines color and transparency
      panel.border = element_rect(color = "white", fill = NA, size = 0.5),
      strip.background = element_blank(),  # Remove background color for facet titles
      strip.text = element_text(color = "black", face = "bold", angle = 0, hjust = 0.5, vjust = 0.5),  # Customize facet title appearance
      legend.position = "bottom", 
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),  
           shape = guide_legend(nrow = 2, byrow = TRUE))
  
  return(plot)
}

plot_balanced_acc <- function(data, category = "skin_tone") {
  demo_df <- data %>% 
    filter(Category == category)
  
  mean_df <- data %>% 
    filter(Category == category)  %>%
    group_by(Model, PromptID) %>%
    summarize(mean_accuracy = mean(Balanced.Accuracy_mean))
  
  # Merge mean data with filtered data
  filtered_demo_df <- merge(demo_df, mean_df, by = c("PromptID", "Model"))
  filtered_demo_df$PromptID <- as.factor(filtered_demo_df$PromptID)
  
  plot <- ggplot(filtered_demo_df, aes(x = UniqueValue, y = Balanced.Accuracy_mean, color = Model, shape = PromptID)) +
  geom_errorbar(aes(ymin = Balanced.Accuracy_lower, ymax = Balanced.Accuracy_upper), width = 0.2, alpha = 0.3) +
  geom_point(size = 3) +
  geom_hline(data = mean_df, aes(yintercept = mean_accuracy, color = Model), linetype='dashed', alpha = 0.6) +  # Use transparent mean lines
  scale_shape_manual(values = c(15, 0, 16, 1, 17, 2, 18, 5)) +  # Manual shape mapping 
  # theme_minimal() +
  facet_grid(~ PromptID) +
  scale_color_brewer(palette = "Set1") + 
  theme(
    legend.text = element_text(size = 10), 
    panel.background = element_rect(fill = "white"),  # Set background color to white
    panel.grid.major = element_line(color = "gray93"), # Customize major grid lines color and transparency
    panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
    strip.background = element_blank(),  # Remove background color for facet titles
    strip.text = element_text(color = "black", face = "bold", angle = 0, hjust = 0.5, vjust = 0.5),  # Customize facet title appearance
    legend.position = "bottom"  # Move legend to the bottom
  ) + 
  labs(y = "Balanced Accuracy", x=category, shape = "Prompt", color = "Model") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE),  
         shape = guide_legend(nrow = 2, byrow = TRUE)) +  
  guides(shape = guide_legend(override.aes = list(color = "black")))
  return(plot)
}

plot_fpr_tpr <- function(data, category) {
  data <- data %>% filter(Category == category)
  plot <- data %>%
    mutate(TPR = Sensitivity_mean, FPR = 1 - Specificity_mean) %>%
    mutate(TPR_upper = Sensitivity_upper, TPR_lower = Sensitivity_lower,
           FPR_lower = 1 - Specificity_upper, FPR_upper = 1 - Specificity_lower) %>% 
    ggplot() +
    geom_errorbar(aes(x = FPR, y = TPR, ymin = TPR_lower, ymax = TPR_upper), alpha = 0.4) +
    geom_errorbar(aes(x = FPR, y = TPR, xmin = FPR_lower, xmax = FPR_upper), alpha = 0.4) +
    geom_point(aes(x = FPR, y = TPR, color = Model, shape = PromptID), size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
    scale_shape_manual(values = c(15, 0, 16, 1, 17, 2, 18, 5)) +   # Manual shape mapping
    facet_wrap(~ UniqueValue) + #!!sym(category)) +
    xlim(0, 1.0) + 
    ylim(0, 1.0) +
    theme_minimal() +  
    theme(legend.title = element_text(size = 12), 
          legend.text = element_text(size = 10), 
          axis.title = element_text(size = 12),  
          axis.text = element_text(size = 10),  
          strip.text = element_text(color = "black", face = "bold", angle = 0, hjust = 0.5, vjust = 0.5),  # Customize facet title appearance
          legend.position = "bottom") +  
    labs(x = "1-Specificity", 
         y = "Sensitivity", 
         color = "Model",
         shape = "Prompt") +
    scale_color_brewer(palette = "Set1") +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE)) 
  guides(shape = guide_legend(override.aes = list(color = "black")))
return(plot)
}
