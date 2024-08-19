# Process Corine Landcover downloaded from Cop
library(terra)
library(dplyr)
library(ggplot2)
library(sf)


corine <- rast("E:/EAGLE/Forest_Project/Data/GEE_export/corine/U2018_CLC2018_V2020_20u1.tif")
ger <- rnaturalearth::ne_countries(country = "Germany", returnclass = "sf", scale = "large") %>% st_transform(3035)

corine.crpd <- crop(corine, ger)
corine.ger <- mask(corine.crpd, ger)
plot(corine.ger)
# 311, 312, 313

corine.forest <- mask(corine.ger, corine.ger, maskvalues = c(23, 24, 25), inverse = TRUE)
plot(corine.forest)
writeRaster(corine.forest, "E:/EAGLE/Forest_Project/Data/GEE_export/corine/Corine_forestmask-2018.tif")
