##this code was written by Dr Hugh Graham as an example of how to calculate 
#vegetation spectral indices 

library(terra)
library(viridisLite)

kat_planet <- rast("Katingan-Planet-Composites/Katingan-Comp-22-median.tif")

print(names(kat_planet))

# Do the band math
kat_planet$NDVI <- (kat_planet$nir - kat_planet$red)/(kat_planet$nir +kat_planet$red)
kat_planet$NDRE <- (kat_planet$nir - kat_planet$rededge)/(kat_planet$nir +kat_planet$rededge)
kat_planet$EVI <- 2.5 * ((kat_planet$nir/10000) - (kat_planet$red/10000)) /
  ((kat_planet$nir/10000) + 6 * (kat_planet$red/10000) - 7.5 * (kat_planet$blue/10000) + 1)

#plot the maps...
{par(mfrow=c(1,3))
plot(kat_planet$NDVI, main="NDVI", col=viridisLite::mako(256))
plot(kat_planet$NDRE, main="NDRE", col=viridisLite::inferno(256))
plot(kat_planet$EVI, main="EVI", col=viridisLite::viridis(256))
par(mfrow=c(1,1))}
