## Steckbriefe pro Wuchsgebiet
library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(tidyterra)
library(RColorBrewer)
library(ggrepel)
library(ggspatial)
library(raster)
library(tidyr)

setwd("E:/EAGLE/Forest_Project/")

ger <- rnaturalearth::ne_countries(country = "Germany", returnclass = "sf", scale = "large") %>% st_transform(3035)

# Trend Rasters entire Period ----------

mask.forest <- function(rst, forest.raster){
  rst <- rst %>% project("EPSG:3035") %>%  crop(forest.raster)
  rst.res <- resample(rst, forest.raster, method = 'bilinear')
  rst.masked <- mask(rst.res, forest.raster)
  return(rst.masked)
}

frst <- rast("Data/GEE_export/corine/Corine_forestmask-2018.tif")
growregion <- read_sf("Data/vector_data/wgwb_wg_2020.shp") %>% st_transform(3035)
bb <- st_bbox(frst)

# Temperature
Temp.rast <- rast("Data/GEE_export/Trend_ObsPeriod/slope_temperature_2m.tif")
Temp.rast <- mask.forest(Temp.rast, frst)
T_rst <- Temp.rast * 120

# Evaporation
evapo.rast <- rast("Data/GEE_export/Trend_ObsPeriod/slope_total_evaporation_sum.tif") %>% mask.forest(frst)
ET_rst <- evapo.rast * 1000 * 120
climate.stack <- rast(list(T_rst$slope, ET_rst$slope))

# calculate mean and median Trend per Growing Region ----

results <- list()
growing_areas_agg <- aggregate(growregion, by = list(growregion$bez_bu_wg), FUN = mean)
names(growing_areas_agg)[names(growing_areas_agg) == "Group.1"] <- "Name"
growing_areas_agg$ID <- c(1:82)

for (forest_type in 23:25) {
  masked_rasters <- mask(climate.stack, frst == forest_type, maskvalue = FALSE)
  means <- extract(masked_rasters, vect(growing_areas_agg), fun=mean, na.rm=TRUE)
  meds <- extract(masked_rasters, vect(growing_areas_agg), fun=median, na.rm=TRUE)
  
  # Add the forest type column to the result
  means$forest_type <- forest_type
  means$name <- growing_areas_agg$name
  means$med <- meds
  
  results[[as.character(forest_type)]] <- means
}

combined_results <- do.call(rbind, results)
growing_areas_with_means <- st_as_sf(merge(growing_areas_agg, combined_results, by="ID", all.x=TRUE))

growing_areas <- cb <- cbind(growing_areas_with_means, growing_areas_with_means$med)
growing_areas$med <- NULL

colnames(growing_areas)[5:10] <- c('T_mean_slope', 'ET_mean_slope', 'forest_type',
                                              'ID.2', 'T_med_slope', 'ET_med_slope')

df_wide <- growing_areas %>% 
  pivot_wider(names_from = forest_type, 
              values_from = c(T_mean_slope, ET_mean_slope, T_med_slope, ET_med_slope))

trend.perRegion <- df_wide %>% st_drop_geometry()
write.csv(trend.perRegion, file = "Data/tables/Trend_perGrowingRegion.csv", row.names = F)

# Calculate Area per Forest Type and Entire Forest Area per GrowingRegion
results <- list()
pixel_area <- res(frst)[1] * res(frst)[2]

for (i in 1:nrow(growing_areas_agg)) {
  current_area <- growing_areas_agg[i, ]
  
  forest_crop <- crop(frst, vect(current_area))
  forest_masked <- mask(forest_crop, vect(current_area))
  forest_areas <- terra::freq(forest_masked)
  
  # Calculate total area of the growing area
  total_area <- sum(forest_areas$count)
  
  # Calculate the proportional area of each forest type
  prop_forest_type <- forest_areas
  prop_forest_type$proportion <- forest_areas$count / total_area
  
  # Calculate total proportional forest area (sum of all forest types)
  total_forest_prop <- (total_area * pixel_area)/ st_area(current_area)
  

  # Create a data frame with the results
  area_data <- data.frame(
    name = rep(current_area$Name,3),
    forest_type = forest_areas$value,
    proportional_area = prop_forest_type$proportion,
    total_forest_proportion = rep(total_forest_prop,3) # zählt nur Waldgebiete zusammen
  )
  
  # Append the results to the list
  results[[i]] <- area_data
}

final_results <- do.call(rbind, results)

frst_area_perRegion <- final_results %>% pivot_wider(names_from = forest_type,
                              values_from = proportional_area) %>% st_drop_geometry()

write.csv(frst_area_perRegion, file = "Data/tables/Forest_area_perRegion.csv", row.names = F)


df.steckbriefe <- merge(frst_area_perRegion, trend.perRegion, by.x = "name", by.y = "Name")
df.steckbriefe <- df.steckbriefe[-c(6,7,8,9)]

colnames(df.steckbriefe)[1:5] <- c('Name', 'Total_forest_prop', 'Prop_Broadleaved', 'Prop_Decidious',
                              'Prop_Mixed')
write.csv(df.steckbriefe, file = "Data/tables/Steckbriefe_perRegion.csv", row.names = F)
