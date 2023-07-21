---
editor_options: 
  markdown: 
    wrap: 72
---

# Pickstone-development

This repository contains R code for processing, analysing and
visualising the data presented in "Improving Predictions of Canopy
Height in Tropical Forests with Machine Learning" by Brianna Pickstone
under the supervision of Dr Andrew Cunliffe and Dr Hugh Graham for her
MSc in Applied Data Science (Environment and Sustainability) at the
University of Exeter in partnership with Permian Global.

**Contact**: [bp424\@exeter.ac.uk](mailto:bp424@exeter.ac.uk){.email}

**This repository contains the following scripts:**

*Preprocessing.R* This script contains code for pre-processing both the
LiDAR data and optical Planet Labs data. This includes generation of the
canopy height model, calculation of topographic metrics and calculations
for the vegetation spectral indices from the optical data.

*Intial_Plots.R* This script contains code for generating a true colour
image of the Planet Labs data as well as the Canopy Height Model from
LiDAR.

*Spatial Autocorrelation.R* This script contains code for assessing
spatial autocorrelation within the data, this includes the calculation
of Moran's I metric and visualisation through a variogram. The code also
provides the generation of the plots to visualise the resampling plan
for spatial cross-validation.

*MLR.R* main script for undertaking multiple linear regression for
prediction of the canopy height model

*RF.R* main script for undertaking multiple linear regression for
prediction of the canopy height model

*CNN.R* main script for undertaking multiple linear regression for
prediction of the canopy height model

*Minimum LiDAR.R* main script for downsampling the dataset to test how
changes to the amount of data affects the predictive performance of each
model and generation of the canopy height model.
