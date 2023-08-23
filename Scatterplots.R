library(tidyverse)
library(patchwork)

#load in data for PlanetScope 3m resolution 
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

#read in dataframes for canopy height density plots 
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m


#read in PlanetScope 3m files 
P3_MLR <-read_csv(file.path(data_path, "PlanetScope 3m/dt_MLR_PS3.csv"))
P3_RF <- read_csv(file.path(data_path, "PlanetScope 3m/dt_RF_P3.csv"))
P3_CNN <- read_csv(file.path(data_path, "PlanetScope 3m/CNN_3m.csv"))
P3_CNN <- P3_CNN %>%
  select(truth = truth_renorm, response = response_renorm)


#read in PlanetScope 10m files
P10_MLR <- read_csv(file.path(data_path, "PlanetScope 10m/dt_MLR_PS10.csv"))
P10_RF <- read_csv(file.path(data_path, "PlanetScope 10m/dt_RF_PS10.csv"))
P10_CNN <- read_csv(file.path(data_path, "PlanetScope 10m/CNN_PS10.csv"))

#read in Sentinel-2 files
S2_MLR <- read_csv(file.path(data_path, "Sentinel-2/dt_MLR_S2.csv"))
S2_RF <- read_csv(file.path(data_path, "Sentinel-2/dt_RF_s2.csv"))
S2_CNN <- read_csv(file.path(data_path, "Sentinel-2/CNN_S2.csv"))


#read in combined files
comb_MLR <- read_csv(file.path(data_path, "Combined/dt_MLR_combined.csv"))
comb_RF <- read_csv(file.path(data_path, "Combined/dt_RF_comb.csv"))
comb_CNN <- read_csv(file.path(data_path, "Combined/CNN_comb.csv"))



# define function ---------------------------------------------------------
#create function to create the model plot: 
create_plot <- function(data, breaks) {
  ggplot(data, aes(x = truth, y = response)) +
    geom_bin_2d(binwidth = 0.3) +
    scale_fill_viridis_c(
      trans = scales::yj_trans(0.1),
      option = "G",
      direction = -1,
      breaks = breaks
    ) +
    geom_abline(slope = 1, linewidth = 0.2) +
    theme_light() +
    theme(
      text = element_text(family = "Times New Roman"),  # Set font to Times New Roman
      axis.text = element_text(size = 10), 
      legend.text = element_text(size = 10), 
      legend.title = element_text(size = 10), 
      axis.ticks = element_line(linewidth = 0.5)
    ) +
    labs(x = "LiDAR Canopy Height (m)", 
         y = "Predicted Canopy Height (m)", 
         fill = "Count")+ 
    scale_x_continuous(breaks = seq(0, 40, by = 10)) +  # Set x-axis tick marks to go up in 10s
    scale_y_continuous(breaks = seq(-10, 40, by = 10), limits = c(-10, 40))
}

# Plot PlanetScope 3m -----------------------------------------------------
P3_MLR_breaks = c(100, 1000, 10000, 40000)
P3_RF_breaks <- c(100, 1000, 10000, 80000)
P3_CNN_breaks <- c(100, 1000, 10000, 70000)

P3_MLR_Plot <- create_plot(P3_MLR, P3_MLR_breaks)
P3_RF_Plot <- create_plot(P3_RF, P3_RF_breaks)
P3_CNN_Plot <- create_plot(P3_CNN, P3_CNN_breaks)

#plot 3 m CHM plot 

(CHM.plot.3m <- ggplot() +
    geom_density(data = df_PS.3m, aes(x = CHM), fill = "#2f9aa0ff", alpha = 0.2) +
    theme_classic() +
    labs(x = "LiDAR Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 8),
      axis.title = element_text(family = "Times New Roman", size = 8),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.position = c(0.85, 0.85),  
      legend.justification = c(1, 1),  # Top-right corner
      legend.title = element_text(family = "Times New Roman", size = 8),  # Legend title font
      legend.text = element_text(family = "Times New Roman", size = 6),
      legend.key.size = unit(0.6, "lines"),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ))

# Arrange the plots using patchwork
(combined_plot <- (P3_MLR_Plot + P3_RF_Plot + P3_CNN_Plot + CHM.plot.3m) + 
  plot_layout(nrow = 2))

ggsave(file = "P3_scatterplots.png", width = 7.10, height = 5.6, dpi = 600)

# Plot PlanetScope 10m  ---------------------------------------------------
P10_MLR_breaks = c(100, 1000, 10000, 40000)
P10_RF_breaks <- c(100, 1000, 10000, 50000)
P10_CNN_breaks <- c(100, 1000, 10000, 80000)

P10_MLR_Plot <- create_plot(P3_MLR, P10_MLR_breaks)
P10_RF_Plot <- create_plot(P10_RF, P10_RF_breaks)
P10_CNN_Plot <- create_plot(P10_CNN, P10_CNN_breaks)

# Plot Sentinel-2 ---------------------------------------------------------


# Plot Combined -----------------------------------------------------------


# Create a function to generate the heatmap
create_heatmap <- function(data, title) {
  ggplot(data, aes(x = truth, y = response)) +
    geom_bin2d(binwidth = c(1, 1), bins = 100) +
    scale_fill_viridis_c(option = "G") +
    labs(title = title, fill = "Count") +
    theme_minimal() +
    theme(legend.position = "right")
}

# Create individual heatmaps for each model
mlr_heatmap <- create_heatmap(P3_MLR, "MLR Model")
rf_heatmap <- create_heatmap(P3_RF, "Random Forest Model")
cnn_heatmap <- create_heatmap(P3_CNN, "CNN Model")

combined_plot <- plot_grid(mlr_heatmap, rf_heatmap, cnn_heatmap, ncol = 3, 
                           align = "hv", labels = "AUTO")

