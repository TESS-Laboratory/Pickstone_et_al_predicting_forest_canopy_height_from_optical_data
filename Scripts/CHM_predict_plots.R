##this script is for the use of visualisation of the predicted canopy height models 
##and the difference between the LiDAR derived CHM 
library(raster)
library(tidyverse)
library(patchwork)
library(viridisLite)
library(raster)

#load in data cubes for each data source
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
comb_cube <- rast(file.path(data_path, "comb_cube.tif"))

#load in predicted cube for each data source (best performing)
PS3_predict <- rast(file.path(data_path, "Canopy Height Prediction/PS3_RF_predict.tif"))
PS10_predict <- rast(file.path(data_path, "Canopy Height Prediction/PS10_RF_predict.tif"))
s2_predict <- rast(file.path(data_path, "Canopy Height Prediction/S2_CNN_predict.tif"))
comb_predict  <- rast(file.path(data_path, "Canopy Height Prediction/comb_RF_predict.tif"))

#read in data tables of actual lidar canopy height 
df_s2 <- read_csv(file.path(data_path, "df_S2.10m_final.csv")) #sentinel 2 - 10m
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m
df_comb <- read_csv(file.path(data_path, "combined_df.csv")) #combined data - 10m 


#determine difference of each data source
difference_PS3m <-  PS3_predict - PScope_3m.cube$CHM
difference_PS10m <- PS10_predict - PScope_10m.cube$CHM
difference_S2 <- s2_predict - s2.cube$CHM
difference_comb <- comb_predict - comb_cube$CHM

#create function for canopy height plot
create_CHM_plot <- function(p, color_scale_limits) {
  plot <- ggplot() +
    theme_bw() +
    geom_spatraster(data = p) +
    theme(
      axis.text = element_text(size = 10, family = "Times New Roman"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.text = element_text(size = 8, family = "Times New Roman"),
      legend.title = element_text(size = 10, family = "Times New Roman"),
      axis.ticks = element_line(linewidth = 0.1)
    ) +
    scale_fill_gradientn(
      name = "Canopy Height (m)",
      colors = viridisLite::viridis(n = 100),
      na.value = 'transparent',
      limits = color_scale_limits,  # Update the limits here
    )
  
  return(plot)
}

# #Plot PlanetScope 3m and difference plot --------------------------------
#plot the canopy height models 
PS3.CHM_Predict_plot <- create_CHM_plot(PS3_predict, c(0,50))
PS3.diff_plot <- create_CHM_plot(difference_PS3m, c(-45, 25))

#plot the difference on a density plot
df_3m_predict <- as.data.frame(PS3_predict, xy = TRUE)

(PS3_density <- ggplot() +
    geom_density(data = df_PS.3m, aes(x = CHM, fill = "LiDAR"), alpha = 0.2) +
    geom_density(data = df_3m_predict, aes(x = lyr1, fill = "Predict"), alpha = 0.2) +
    scale_fill_manual(values = c('LiDAR' = '#ffcf20ff', 'Predict' = '#2f9aa0ff'), 
                      breaks = c("LiDAR", "Predict"), 
                      name = NULL) +
    theme_classic() +
    labs(x = "Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.text = element_text(family = "Times New Roman", size = 10),
      panel.grid.major = element_blank(),
      legend.position = "bottom", 
      panel.grid.minor = element_blank()
    ))


PS3.CHM_Predict_plot + PS3.diff_plot + PS3_density

ggsave(file = "CHM_P3.png", width = 10.3, height = 5, dpi = 600)

# Plot the 10 m PlanetScope CHM and difference plot -----------------------
PS10.CHM_plot <- create_CHM_plot(PS10_predict, c(0,50))
PS10.diff_plot <- create_CHM_plot(difference_PS10m, c(-35, 20))


#create density plot of the difference between LiDAR and predicted
df_10m_predict <- as.data.frame(PS10_predict, xy = TRUE)

(PS10_density <- ggplot() +
    geom_density(data = df_PS.10m, aes(x = CHM, fill = "LiDAR"), alpha = 0.2) +
    geom_density(data = df_10m_predict, aes(x = lyr1, fill = "Predict"), alpha = 0.2) +
    scale_fill_manual(values = c('LiDAR' = '#ffcf20ff', 'Predict' = '#2f9aa0ff'), 
                      breaks = c("LiDAR", "Predict"), 
                      name = NULL) +
    theme_classic() +
    labs(x = "Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.title = element_text(family = "Times New Roman", size = 10),  # Legend title font
      legend.text = element_text(family = "Times New Roman", size = 10),
      panel.grid.major = element_blank(),
      legend.position = "bottom", 
      panel.grid.minor = element_blank()
    ))

#combine the plots together 
PS10.CHM_plot + PS10.diff_plot + PS10_density

#save the plot
ggsave(file = "CHM_P10.png", width = 10.30, height = 4, dpi = 600)

# Plot the S2  CHM and difference plot -----------------------
S2.CHM_plot <- create_CHM_plot(s2_predict, c(0,50))
S2.diff_plot <- create_CHM_plot(difference_S2, c(-25, 20))

#create density plot of the difference between LiDAR and predicted
df_S2_predict <- as.data.frame(s2_predict, xy = TRUE)

(S2_density <- ggplot() +
    geom_density(data = df_s2, aes(x = CHM, fill = "LiDAR"), alpha = 0.2) +
    geom_density(data = df_S2_predict, aes(x = lyr1, fill = "Predict"), alpha = 0.2) +
    scale_fill_manual(values = c('LiDAR' = '#ffcf20ff', 'Predict' = '#2f9aa0ff'), 
                      breaks = c("LiDAR", "Predict"), 
                      name = NULL) +
    theme_classic() +
    labs(x = "Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.title = element_text(family = "Times New Roman", size = 10),  # Legend title font
      legend.text = element_text(family = "Times New Roman", size = 10),
      panel.grid.major = element_blank(),
      legend.position = "bottom", 
      panel.grid.minor = element_blank()
    ))

S2.CHM_plot + S2.diff_plot + S2_density

ggsave(file = "CHM_S2.png", width = 10.30, height = 5, dpi = 600)

# Plot the combined CHM and difference plot -----------------------
comb.CHM_plot <- create_CHM_plot(comb_predict, c(0,50))
comb.diff_plot <- create_CHM_plot(difference_comb, c(-30, 20))

#create density plot of the difference between LiDAR and predicted
df_comb_predict <- as.data.frame(comb_predict, xy = TRUE)

(comb_density <- ggplot() +
    geom_density(data = df_comb, aes(x = CHM, fill = "LiDAR"), alpha = 0.2) +
    geom_density(data = df_comb_predict, aes(x = lyr1, fill = "Predict"), alpha = 0.2) +
    scale_fill_manual(values = c('LiDAR' = '#ffcf20ff', 'Predict' = '#2f9aa0ff'), 
                      breaks = c("LiDAR", "Predict"), 
                      name = NULL) +
    theme_classic() +
    labs(x = "Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.title = element_text(family = "Times New Roman", size = 10),  # Legend title font
      legend.text = element_text(family = "Times New Roman", size = 10),
      panel.grid.major = element_blank(),
      legend.position = "bottom", 
      panel.grid.minor = element_blank()
    ))


comb.CHM_plot + comb.diff_plot + comb_density

ggsave(file = "CHM_comb.png", width = 10.30, height = 5, dpi = 600)
