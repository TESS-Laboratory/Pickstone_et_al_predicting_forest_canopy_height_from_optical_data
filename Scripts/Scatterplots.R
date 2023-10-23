##This code is written to display the predicted vs truth (LiDAR canopy height)
##of each datasource and machine learning algorithm 
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

#plot 3 m CHM plot 

(CHM.plot.3m <- ggplot() +
    geom_density(data = df_PS.3m, aes(x = CHM), fill = "#2f9aa0ff", alpha = 0.2) +
    theme_classic() +
    labs(x = "LiDAR Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.key.size = unit(0.6, "lines"),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ))

# Arrange the plots using patchwork
(P3_MLR_Plot + P3_RF_Plot + P3_CNN_Plot + CHM.plot.3m) + 
  plot_layout(nrow = 2) + 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = "bold", 
                                family = "Times New Roman"))

ggsave(file = "P3_scatterplots.png", width = 8.10, height = 6.3, dpi = 600)
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

#plot 10 m CHM 
(CHM.plot.10m <- ggplot() +
    geom_density(data = df_PS.10m, aes(x = CHM), fill = "#2f9aa0ff", alpha = 0.2) +
    theme_classic() +
    labs(x = "LiDAR Canopy Height (m)", 
         y = "Frequency") +
    theme(
      axis.text = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.line = element_line(linewidth = 0.2),
      axis.ticks = element_line(linewidth = 0.2),
      legend.key.size = unit(0.6, "lines"),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ))


(combined_plot.P10 <- (P10_MLR_Plot + P10_RF_Plot + P10_CNN_Plot + CHM.plot.10m) + 
    plot_layout(nrow = 2)+ 
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = "bold", 
                                  family = "Times New Roman")))
  

ggsave(file = "P10_scatterplots.png", width = 8.10, height = 6.3, dpi = 600)

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


(combined_plot.S2 <- (S2_MLR_Plot + S2_RF_Plot + S2_CNN_Plot + CHM.plot.10m) + 
    plot_layout(nrow = 2)+ 
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = "bold", 
                                  family = "Times New Roman")))

ggsave(file = "S2_scatterplots.png", width = 8.10, height = 6.3, dpi = 600)


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


(combined_plot.comb <- (comb_MLR_Plot + comb_RF_Plot + comb_CNN_Plot + CHM.plot.10m) + 
    plot_layout(nrow = 2)+ 
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = "bold", 
                                  family = "Times New Roman")))

ggsave(file = "comb_scatterplots.png", width = 8.10, height = 6.3, dpi = 600)

