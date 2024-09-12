# ERA5 trends for entire Germany
library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(tidyterra)
library(RColorBrewer)
library(ggrepel)
library(ggspatial)
library(raster)

setwd("E:/EAGLE/Forest_Project/")
# per Thünen Wuchsgebiet and Forest type -----
# see Python Scripts

ger <- rnaturalearth::ne_countries(country = "Germany", returnclass = "sf", scale = "large") %>% st_transform(3035)

# Trend Rasters entire Period ----------

mask.forest <- function(rst, forest.raster){
  rst <- rst %>% project("EPSG:3035") %>%  crop(forest.raster)
  rst.res <- resample(rst, forest.raster, method = 'bilinear')
  rst.masked <- mask(rst.res, forest.raster)
  return(rst.masked)
}

theme <- theme(panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  #panel.background = element_blank(),
  #strip.background = element_blank(),
  legend.position = "bottom", legend.key.width = unit(0.5, "inches"))


plot_trend_map <- function(rst, climate_param, palette, lims, labs, unit){
  gg <- ggplot()+
    geom_spatraster(data = rst)+
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), crs = 3035)+
    scale_fill_gradientn(name = unit, 
                         colours = palette, 
                         na.value = 'white', limits = lims)+
    #new_scale_color()+
    #geom_sf(data = grow_slope_tr, fill = NA, color = "black")+
    #scale_colour_manual(name = "", values = c("black"),label = c("Naturparke"))+
    #geom_sf(data = bayern, fill = "white", color = NA)+
    geom_sf(data = ger, fill = "transparent", color = 'black')+
    scale_y_continuous(breaks = seq(30,65, by = 1))+
    scale_x_continuous(breaks = seq(-15,40, by = 1))+ 
    xlab("Longitude")+
    ylab("Latitude")+
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.4, "in"), pad_y = unit(0.75, "in"),
                           style = north_arrow_fancy_orienteering)+
    ggtitle(paste0(climate_param," increase in German Forests from 1950 until 2024"),
            subtitle = "ERA5 Climate Reanalysis Data over 6 Decades")
  
  ggsave(paste0("Maps/", climate_param, "trend-obsPeriod.png"), width = 20, height = 20, units = "cm"
         )
  return(gg)
}

mean_slope <- function(vector, raster.stack){
  n <- ncol(vector)
  vector$T_slope <- rep(NA, nrow(vector))
  vector$prec_slope <- rep(NA, nrow(vector))
  vector$evapo_slope <- rep(NA, nrow(vector))
  #vector$water_def <- rep(NA, nrow(vector))
  
  for (i in 1:nrow(vector)){
    for (lyr in 1:3){
      rst <- raster.stack[[lyr]] %>% mask(vector[i,])
      mn <- global(rst, "mean", na.rm = T)$mean
      vector[i, (n +lyr)] <- mn
    }
  }
  return(vector)
}

frst <- rast("Data/GEE_export/corine/Corine_forestmask-2018.tif")
growregion <- read_sf("Data/vector_data/wgwb_wg_2020.shp") %>% st_transform(3035)
bb <- st_bbox(frst)

# Temperature
Temp.rast <- rast("Data/GEE_export/Trend_ObsPeriod/slope_temperature_2m.tif")
Temp.rast <- mask.forest(Temp.rast, frst)
T_rst <- Temp.rast * 120
#writeRaster(T_rst, "Data/Final-Results/trend_raster/slope_temperature_2m.tif")

# Evaporation
evapo.rast <- rast("Data/GEE_export/Trend_ObsPeriod/slope_total_evaporation_sum.tif") %>% mask.forest(frst)
ET_rst <- evapo.rast * 1000 * 120
#writeRaster(ET_rst, "Data/Final-Results/trend_raster/slope_total_evaporation_sum.tif")

# Precipitation
prec.rast <- rast("Data/GEE_export/Trend_ObsPeriod/slope_total_precipitation_sum.tif") %>% 
  mask.forest(frst)
prec_rst <- prec.rast * 1000 * 120
#writeRaster(prec_rst, "Data/Final-Results/trend_raster/slope_total_precipitation_sum.tif")

# Plotting ----
plot_trend_map(T_rst$slope, "Temperature", brewer.pal(9, 'YlOrRd'), c(0.25,0.35), 
               round(grow_slope_tr$T_slope,3), "K/decade")
plot_trend_map(ET_rst$slope, "Evapotranspiration", rev(brewer.pal(9, 'YlGnBu')), c(-1.3,-0.5), 
               "mm/decade")
plot_trend_map(prec_rst$slope, "Precipitation", brewer.pal(9, 'Purples'),  c(0, 3), 
               round(grow_slope_tr$prec_slope,2), "mm/decade")


#slope_growregion <- mean_slope(growregion, climate.stack)


# Trend Rasters Monthly -----------