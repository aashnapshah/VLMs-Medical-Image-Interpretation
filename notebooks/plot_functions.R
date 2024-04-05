
refusal_plot <- function(df_cleaned){
  rates_data <- df_cleaned %>%
    group_by(PromptID, Model, CategorizedResponse, .groups = "drop") %>%
    summarise(count = n()) %>%
    pivot_wider(names_from = CategorizedResponse, values_from = count, values_fill = 0) %>%
    mutate(
      total_responses = Diagnosed + Refused, #+ Blocked,
      Diagnosis = Diagnosed / total_responses,
      Refusal = Refused / total_responses,
      #Blocked = Blocked / total_responses
    ) %>% select(Diagnosis, Refusal) #, Blocked)
  
  melted_data <- melt(rates_data, id.vars = c("PromptID", "Model"), variable.name = "RateType", value.name = "Rate")
  
  # Calculate the fractions of refusals and blocks for each race and each Model
  fraction_data <- aggregate(Rate ~ PromptID + Model + RateType, melted_data, sum) %>% filter(RateType != 'Diagnosis')
  fraction_data$Model_Prompt <- paste(fraction_data$PromptID, fraction_data$Model, sep = " - ")
  set.seed(123)
  
  fraction_data$PromptID <- as.factor(fraction_data$PromptID)
  
  plot <- ggplot(fraction_data, aes(x = Model, y = Rate, fill = Model, shape = PromptID)) +
    geom_point(size = 4, position = position_dodge(width = 1.0), aes(color = Model), stroke = 1.0, alpha = 1) +
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
      legend.position = "bottom"  # Move legend to the bottom
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),  
           shape = guide_legend(nrow = 2, byrow = TRUE))
  
  return(plot)
}

plot_balanced_acc <- function(data, category = "skin_tone") {
  plot <- ggplot(data, aes(x = !!sym(category), y = Balanced.Accuracy_mean, color = Model, shape = PromptID)) +
  geom_errorbar(aes(ymin = Balanced.Accuracy_lower, ymax = Balanced.Accuracy_upper), width = 0.2, alpha = 0.3) +
  geom_point(size = 3) +
  geom_hline(data = mean_df, aes(yintercept = mean_accuracy, color = Model), alpha = 0.6) +  # Use transparent mean lines
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

