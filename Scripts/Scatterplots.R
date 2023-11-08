##This code is written to display the predicted vs truth (LiDAR canopy height)
##of each datasource and machine learning algorithm 
library(tidyverse)
library(ggpubr)

#load in data for PlanetScope 3m resolution 
data_path <- "/raid/home/bp424/Documents/MTHM603/Data"

#read in dataframes for canopy height density plots 
df_PS.3m <- read_csv(file.path(data_path, "PScope_3m_df.csv")) #PlanetScope - 3m
df_PS.10m <- read_csv(file.path(data_path, "df_PScope_10m.csv")) #PlanetScope - 10m

#read in PlanetScope 3m files 
P3_MLR <-read_csv(file.path(data_path, "PlanetScope 3m/dt_MLR_PS3.csv"))
P3_RF <- read_csv(file.path(data_path, "PlanetScope 3m/dt_RF_P3.csv"))
P3_CNN <- read_csv(file.path(data_path, "PlanetScope 3m/CNN_3m.csv"))%>%
  select(truth = truth_renorm, response = response_renorm)


#read in PlanetScope 10m files
P10_MLR <- read_csv(file.path(data_path, "PlanetScope 10m/dt_MLR_PS10.csv"))
P10_RF <- read_csv(file.path(data_path, "PlanetScope 10m/dt_RF_P10.csv"))
P10_CNN <- read_csv(file.path(data_path, "PlanetScope 10m/CNN_PS10.csv"))%>%
  select(truth = truth_renorm, response = response_renorm)

#read in Sentinel-2 files
S2_MLR <- read_csv(file.path(data_path, "Sentinel-2/dt_MLR_S2.csv"))
S2_RF <- read_csv(file.path(data_path, "Sentinel-2/dt_RF_S2.csv"))
S2_CNN <- read_csv(file.path(data_path, "Sentinel-2/CNN_S2.csv")) %>%
  select(truth = truth_renorm, response = response_renorm)

#read in combined files
comb_MLR <- read_csv(file.path(data_path, "Combined/dt_MLR_combined.csv"))
comb_RF <- read_csv(file.path(data_path, "Combined/dt_RF_comb.csv"))
comb_CNN <- read_csv(file.path(data_path, "Combined/CNN_comb.csv"))%>%
  select(truth = truth_renorm, response = response_renorm)

#load in data cubes for each data source
PScope_3m.cube <- rast(file.path(data_path,"PScope_3m.tif"))
PScope_10m.cube <- rast(file.path(data_path,"PScope_10m.tif"))
s2.cube <- rast(file.path(data_path,"S2_comb_data.tif"))
comb_cube <- rast(file.path(data_path, "comb_cube.tif"))

#load in predicted cube for each data source (best performing)
PS3_predict <- rast(file.path(data_path, "Canopy Height Prediction/PS3_RF_predict.tif"))
PS10_predict <- rast(file.path(data_path, "Canopy Height Prediction/PS10_RF_predict.tif"))
s2_predict <- rast(file.path(data_path, "Canopy Height Prediction/S2_CNN_predict.tif"))
s2_predict <- project(s2_predict, s2.cube)
comb_predict  <- rast(file.path(data_path, "Canopy Height Prediction/comb_RF_predict.tif"))


# determine difference between predicted and actual -----------------------
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

# define function ---------------------------------------------------------
create_plot <- function(data, breaks, r_squared) {
  p <- ggplot(data, aes(x = truth, y = response)) +
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
      text = element_text(family = "Times New Roman"),  
      axis.text = element_text(size = 10), 
      legend.text = element_text(size = 10), 
      legend.title = element_text(size = 10), 
      axis.ticks = element_line(linewidth = 0.5)
    ) +
    labs(x = "LiDAR Canopy Height (m)", 
         y = "Predicted Canopy Height (m)", 
         fill = "Count") + 
    scale_x_continuous(breaks = seq(0, 40, by = 10)) +  
    scale_y_continuous(breaks = seq(-10, 40, by = 10), limits = c(-10, 40)) +
    annotate("text", x = 10, y = 37, label =bquote(R^2 == .(r_squared)), size = 5, 
             family = "Times New Roman",fontface = "bold")  
  
  return(p)
}

# Plot PlanetScope 3m -----------------------------------------------------
P3_MLR_breaks <- c(100, 1000, 10000, 40000)
P3_RF_breaks <- c(100, 1000, 10000, 80000)
P3_CNN_breaks <- c(100, 1000, 10000, 70000)

P3_MLR_R2 <- "0.37"
P3_RF_R2 <- "0.43"
P3_CNN_R2 <- "0.42"

P3_MLR_Plot <- create_plot(P3_MLR, P3_MLR_breaks, P3_MLR_R2)
P3_RF_Plot <- create_plot(P3_RF, P3_RF_breaks, P3_RF_R2)
P3_CNN_Plot <- create_plot(P3_CNN, P3_CNN_breaks, P3_CNN_R2)

# #Plot PlanetScope 3m and difference plot --------------------------------
#plot the canopy height models 
PS3.CHM_Predict_plot <- create_CHM_plot(PS3_predict, c(0,50))
PS3.diff_plot <- create_CHM_plot(difference_PS3m, c(-45, 25))

#plot the difference on a density plot
df_3m_predict <- as.data.frame(PS3_predict, xy = TRUE)

PS3_density <- ggplot() +
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
    )

ggarrange(P3_MLR_Plot, P3_RF_Plot,
            P3_CNN_Plot, PS3_density, 
          PS3.CHM_Predict_plot, PS3.diff_plot, 
          ncol = 2, nrow = 3, labels="auto", 
          font.label = list(color = "black", face = "bold", family = "Times New Roman"))

ggsave(file = "P3_scatterplots.png", width = 9.50, height = 11.3, dpi = 600)


# Plot PlanetScope 10m  ---------------------------------------------------
P10_MLR_breaks = c(100, 1000, 5000)
P10_RF_breaks <- c(100, 1000, 10000)
P10_CNN_breaks <- c(100, 1000, 5000)

P10_MLR_R2 <- "0.50"
P10_RF_R2 <- "0.60"
P10_CNN_R2 <- "0.60"

(P10_MLR_Plot <- create_plot(P10_MLR, P10_MLR_breaks, P10_MLR_R2))
(P10_RF_Plot <- create_plot(P10_RF, P10_RF_breaks, P10_RF_R2))
(P10_CNN_Plot <- create_plot(P10_CNN, P10_CNN_breaks, P10_CNN_R2))


# Plot the 10 m PlanetScope CHM and difference plot -----------------------
PS10.CHM_plot <- create_CHM_plot(PS10_predict, c(0,50))
PS10.diff_plot <- create_CHM_plot(difference_PS10m, c(-35, 20))


#create density plot of the difference between LiDAR and predicted
df_10m_predict <- as.data.frame(PS10_predict, xy = TRUE)

PS10_density <- ggplot() +
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
    )


ggarrange(P10_MLR_Plot, P10_RF_Plot,
          P10_CNN_Plot, PS10_density, 
          PS10.CHM_plot, PS10.diff_plot, 
          ncol = 2, nrow = 3, labels="auto", 
          font.label = list(color = "black", face = "bold", family = "Times New Roman"), 
          heights = c(3, 3, 3))


ggsave(file = "P10_scatterplots.png", width = 9.50, height = 11.3, dpi = 600)

# Plot Sentinel-2 ---------------------------------------------------------
S2_MLR_breaks = c(100, 1000, 4000)
S2_RF_breaks <- c(100, 1000, 10000)
S2_CNN_breaks <- c(100, 1000, 5000)

S2_MLR_R2 <- "0.65"
S2_RF_R2 <- "0.68"
S2_CNN_R2 <- "0.69"


(S2_MLR_Plot <- create_plot(S2_MLR, S2_MLR_breaks, S2_MLR_R2))
(S2_RF_Plot <- create_plot(S2_RF, S2_RF_breaks, S2_RF_R2))
(S2_CNN_Plot <- create_plot(S2_CNN, S2_CNN_breaks, S2_CNN_R2))


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

ggarrange(S2_MLR_Plot, S2_RF_Plot,
          S2_CNN_Plot, S2_density, 
          S2.CHM_plot, S2.diff_plot, 
          ncol = 2, nrow = 3, labels="auto", 
          font.label = list(color = "black", face = "bold", family = "Times New Roman"), 
          heights = c(3, 3, 3))



ggsave(file = "S2_scatterplots.png", width = 9.50, height = 11.3, dpi = 600)


# Plot Combined -----------------------------------------------------------
comb_MLR_breaks = c(100, 1000, 4000)
comb_RF_breaks <- c(100, 1000, 10000)
comb_CNN_breaks <- c(100, 1000, 5000)

comb_MLR_R2 <- "0.64"
comb_RF_R2 <- "0.69"
comb_CNN_R2 <- "0.68"

(comb_MLR_Plot <- create_plot(comb_MLR, comb_MLR_breaks, comb_MLR_R2))
(comb_RF_Plot <- create_plot(comb_RF, comb_RF_breaks, comb_RF_R2))
(comb_CNN_Plot <- create_plot(comb_CNN, comb_CNN_breaks, comb_CNN_R2))

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

ggarrange(comb_MLR_Plot, comb_RF_Plot,
          comb_CNN_Plot, comb_density, 
          comb.CHM_plot, comb.diff_plot, 
          ncol = 2, nrow = 3, labels="auto", 
          font.label = list(color = "black", face = "bold", family = "Times New Roman"), 
          heights = c(3, 3, 3))


ggsave(file = "comb_scatterplots.png", width = 9.50, height = 11.3, dpi = 600)

