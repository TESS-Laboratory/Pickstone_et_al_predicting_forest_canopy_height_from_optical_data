---
editor_options: 
  markdown: 
    wrap: 72
---

# Pickstone-development

This repository contains R code for processing, analysing and
visualising the data presented in "Comparison of Multi-Spectral Optical
PlanetScope Data with Sentinel-2 Data for Estimating Airborne LiDAR
derived Canopy Height in Tropical Forests using Machine Learning
Techniques" by Brianna Pickstone under the supervision of Dr Andrew
Cunliffe and Dr Hugh Graham for her MSc in Applied Data Science
(Environment and Sustainability) at the University of Exeter in
partnership with Permian Global.

**Data Source:** This study uses a combination of data:

1.  The Airborne LiDAR data from Permian Global, supplied by Dr Hugh
    Graham
2.  The PlanetScope optical data from Planet Labs, supplied by Dr Hugh
    Graham
3.  Sentinel-2 Data, downloaded from the Sentinel-2-Cloud Optimised
    GeoTIFFs (COG)
4.  Digital Terrain Model from Copernicus, downloaded from the Microsoft
    Planetary Computer STAC

**Contact**: [bp424\@exeter.ac.uk](mailto:bp424@exeter.ac.uk){.email}

**This repository contains the following scripts:**

*Importing_S2.R* This script contains code for downloading the
Sentinel-2 Data from the COG. This also includes the generation of the
canopy height model from LiDAR, accessing the digital terrain model,
calculation of topographic metrics, calculation of vegetation spectral
indices, and the creation of the 10 m resolution data cube and table.

*Preprocessing.R* This script contains code for pre-processing both the
LiDAR data and optical Planet Labs data. This includes generation of the
canopy height model, calculation of topographic metrics and calculations
for the vegetation spectral indices from the optical data.

*Combining_Data_S2_PS.R* This script contains the code for combining the
Sentinel-2 Data at 10 m resolution with the PlanetScope data at 10 m
resolution

*Intial_Plots.R* This script contains code for generating a true colour
image of the PlanetLabs data and Sentinel-2 data as well as the Canopy
Height Model from LiDAR.

*Spatial Autocorrelation.R* This script contains code for assessing
spatial autocorrelation within the data, this includes the calculation
of Moran's I metric and visualisation through a variogram. The code also
provides the generation of the plots to visualise the resampling plan
for spatial cross-validation.

*MLR.R* main script for undertaking multiple linear regression for
prediction of the canopy height model

*RF.R* main script for undertaking Random Forest for prediction of the
canopy height model

*CNN_2D.R* main script for undertaking a Convolutional Neural Network
for prediction of the canopy height model
