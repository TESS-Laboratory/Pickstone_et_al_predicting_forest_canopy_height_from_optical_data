##This code is used to create a visualisation of the performance of all 
##four data sources and machine learning algorithms using RMSE, MAE, R2 and time

library(tidyverse)
library(patchwork)


# RMSE Plot ---------------------------------------------------------------

RMSE <- data.frame(
  Data_Source = c("PS (3m)", "PS (10m)", "S2", "Combined"),
  MLR = c(5.76, 4.40, 3.70, 3.73),
  RF = c(5.50, 3.97, 3.52, 3.47),
  CNN = c(5.53, 3.95, 3.47, 3.50)
)

RMSE <- pivot_longer(RMSE, cols = c("MLR", "RF", "CNN"),
                          names_to = "Model", values_to = "RMSE")

# Create the bar chart
theme_set(theme_minimal() +
            theme(text = element_text(family = "Times New Roman")))

(RMSE.plot <- ggplot(RMSE, aes(x = Data_Source, y = RMSE, fill = factor(Model, levels = c("MLR", "RF", "CNN")))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Data Source",
         y = "RMSE",
         fill = "Models") +  # Change the legend title here
    scale_fill_manual(values = c("MLR" = "#3a5e8cff", "RF" = "#ffcf20ff", "CNN" = "#2f9aa0ff"),
                      breaks = c("MLR", "RF", "CNN")) +
    scale_x_discrete(limits = c("PS (3m)", "PS (10m)", "S2", "Combined")) +
    guides(fill = guide_legend(order = 1)) +
    theme_minimal() +
    theme(text = element_text(family = "Times New Roman")))


# R2 Plot ----------------------------------------------------------------

# Create a new tibble for R2
r2_data <- tibble(
  Data_Source = c("PS (3m)", "PS (10m)", "S2", "Combined"),
  MLR = c(0.37, 0.50, 0.65, 0.64),
  RF = c(0.43, 0.6, 0.68, 0.69),
  CNN = c(0.42, 0.60, 0.69, 0.68)
)

# Reshape the data
r2_long <- pivot_longer(r2_data, cols = c("MLR", "RF", "CNN"),
                        names_to = "Model", values_to = "Value")

# Create the bar chart for R2
(r2_plot <- ggplot(r2_long, aes(x = Data_Source, y = Value, fill = factor(Model, levels = c("MLR", "RF", "CNN"))))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Data Source",
       y = expression(R^2))+
  scale_fill_manual(values = c("MLR" = "#3a5e8cff", "RF" = "#ffcf20ff", "CNN" = "#2f9aa0ff"),
                                   breaks = c("MLR", "RF", "CNN")) +  # Set the order of legend
  scale_x_discrete(limits = c("PS (3m)", "PS (10m)", "S2", "Combined")) +  # Set the order of x-axis
  guides(fill = guide_legend(order = 1)) +  # Set the order of legend
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"), 
        legend.position = "none"))

# bar chart for MAE -------------------------------------------------------


# Create a new tibble for TIME
MAE_data <- tibble(
  Data_Source = c("PS (3m)", "PS (10m)", "S2", "Combined"),
  MLR = c(4.61, 3.48, 2.89, 2.90),
  RF = c(4.27, 3.01, 2.63, 2.58),
  CNN = c(4.32, 3.05, 2.64, 2.68)
)

# Reshape the data
MAE_long <- pivot_longer(MAE_data, cols = c("MLR", "RF", "CNN"),
                          names_to = "Model", values_to = "Value")

# Create the bar chart for TIME
(MAE_plot <- ggplot(MAE_long, aes(x = Data_Source, y = Value, fill = factor(Model, levels = c("MLR", "RF", "CNN"))))+
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Data Source",
         y = "MAE") +
    scale_fill_manual(values = c("MLR" = "#3a5e8cff", "RF" = "#ffcf20ff", "CNN" = "#2f9aa0ff"),
                      breaks = c("MLR", "RF", "CNN")) +  # Set the order of legend
    scale_x_discrete(limits = c("PS (3m)", "PS (10m)", "S2", "Combined")) +  # Set the order of x-axis
    guides(fill = guide_legend(order = 1)) +  # Set the order of legend
    theme_minimal() +
    theme(text = element_text(family = "Times New Roman"), 
          legend.position = "none"))

# bar chart for Time ------------------------------------------------------

# Create a new tibble for TIME
time_data <- tibble(
  Data_Source = c("PS (3m)", "PS (10m)", "S2", "Combined"),
  MLR = c(34.41, 3.99, 3.82, 5.25),
  RF = c(450.35, 31.48, 34.23, 33.53),
  CNN = c(2723.23, 126.86, 77.35, 155.65)
)

# Reshape the data
time_long <- pivot_longer(time_data, cols = c("MLR", "RF", "CNN"),
                          names_to = "Model", values_to = "Value")

# Create the bar chart for TIME
(time_plot <- ggplot(time_long, aes(x = Data_Source, y = Value, fill = factor(Model, levels = c("MLR", "RF", "CNN"))))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Data Source",
       y = "Time (minutes)") +
  scale_fill_manual(values = c("MLR" = "#3a5e8cff", "RF" = "#ffcf20ff", "CNN" = "#2f9aa0ff"),
                    breaks = c("MLR", "RF", "CNN")) +  # Set the order of legend
  scale_x_discrete(limits = c("PS (3m)", "PS (10m)", "S2", "Combined")) +  # Set the order of x-axis
  guides(fill = guide_legend(order = 1)) +  # Set the order of legend
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"), 
        legend.position = "none"))

MAE_plot + RMSE.plot + r2_plot + time_plot

ggsave(file="Performance_models.png", width = 7.10, height = 5.6, dpi = 600)

