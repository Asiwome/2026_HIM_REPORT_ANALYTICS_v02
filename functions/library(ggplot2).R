library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Input data
forest_data <- tibble::tribble(
  ~Variable, ~Category, ~cPR, ~cPR_low, ~cPR_high, ~aPR, ~aPR_low, ~aPR_high,
  "Child's Gender", "Male vs. Female (Ref)", 2.73, 1.51, 4.95, 2.20, 1.23, 3.94,
  "Child's Education", "Preschool vs. None (Ref)", 8.77, 2.01, 38.27, 4.89, 1.25, 19.03,
  "Child's Education", "Basic vs. None (Ref)", 5.17, 1.32, 20.33, 4.54, 1.34, 15.42,
  "Child's Education", "Secondary vs. None (Ref)", 2.76, 0.63, 12.12, 3.58, 0.86, 14.90,
  "Child's Education", "Tertiary vs. None (Ref)", 3.80, 0.76, 19.03, 3.51, 0.62, 19.95,
  "Caregiver's Gender", "Male vs. Female (Ref)", 1.44, 0.89, 2.34, 1.72, 1.07, 2.76,
  "Caregiver's Relation", "Non-biological vs. Bio (Ref)", 0.85, 0.49, 1.48, 0.52, 0.27, 1.03,
  "Caregiver's Religion", "Others vs. Christian (Ref)", 0.65, 0.37, 1.13, 1.10, 0.63, 1.91,
  "Caregiver's Marital Status", "Unmarried vs. Married (Ref)", 0.69, 0.40, 1.18, 0.56, 0.30, 1.06,
  "Caregiver's Employment", "Unemployed vs. Employed (Ref)", 0.76, 0.48, 1.18, 0.73, 0.47, 1.13
)

# 2. Reshape ensuring 'PR', 'low', and 'high' are explicitly created
plot_df <- forest_data |> 
  pivot_longer(
    cols = matches("^(cPR|aPR)"),
    names_to = c("Model", ".value"),
    names_pattern = "(cPR|aPR)_?(.*)"
  ) |> 
  # Fix column naming if empty string was produced for point estimates
  rename_with(~ ifelse(.x == "", "PR", .x)) |> 
  mutate(
    Model = if_else(Model == "cPR", "Crude PR (cPR)", "Adjusted PR (aPR)"),
    Category = factor(Category, levels = rev(unique(forest_data$Category)))
  )

# 3. Plotting
ggplot(plot_df, aes(y = Category, x = PR, xmin = low, xmax = high, color = Model, shape = Model)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.6, linewidth = 0.7) +
  scale_x_log10(
    breaks = c(0.25, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0),
    labels = c("0.25", "0.50", "1.00", "2.00", "5.00", "10.00", "20.00")
  ) +
  scale_color_manual(values = c("Crude PR (cPR)" = "#D95F02", "Adjusted PR (aPR)" = "#1B9E77")) +
  facet_grid(Variable ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Determinants of Medication Non-Adherence",
    subtitle = "Modified Poisson Regression: Crude vs. Adjusted Prevalence Ratios (95% CI)",
    x = "Prevalence Ratio (Log Scale)",
    y = NULL,
    color = "Model",
    shape = "Model"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(angle = 0, face = "bold", hjust = 0),
    strip.background = element_rect(fill = "gray95"),
    plot.title = element_text(face = "bold", size = 13)
  )