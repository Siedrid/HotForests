# final GEE plots - HotForest Project


library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(tidyterra)
library(RColorBrewer)
library(ggrepel)
library(ggspatial)
library(raster)

ger <- rnaturalearth::ne_states(country = "Germany", returnclass = "sf")
bayern <- ger[ger$name == "Bayern",]
bayern.proj <- st_transform(bayern, "EPSG:3035")
bb <- st_bbox(bayern)
background <- st_difference(st_as_sfc(bb), bayern)

setwd("C:/Users/laura/Documents/Eagle/CloudComputing/")
frst <- rast("corine/forest_mask.tif")
# mask with corine forest
mask.forest <- function(rst, forest.raster){
  rst <- rst %>% project("EPSG:3035") %>%  crop(forest.raster)
  rst.res <- resample(rst, forest.raster, method = 'bilinear')
  rst.masked <- mask(rst.res, forest.raster)
  return(rst.masked)
}

min_max_norm <- function(x) {
  min <- global(x, "min", na.rm=TRUE)$min
  max <- global(x, "max", na.rm = TRUE)$max
  norm <- (x - min) / (max - min)
  #(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
  return(norm)
}

# Climate Params Stack - ERA5 ----
# Temperature
rst <- rast("GEE_out/trend_raster/T_all_lineartrend_slope.tif")
rst <- mask.forest(rst, frst)
T_rst <- rst *120 # from months to decades
T_norm <- min_max_norm(T_rst)

# precipitation
rst <- rast("GEE_out/trend_raster/total_precipitation_sum_all_lineartrend_slope-v3.tif")
rst <- mask.forest(rst, frst)
prec_rst <- (rst / 1000000000) * 1000 *120 # m to mm
prec_norm <- min_max_norm(prec_rst)

# evapotranspiration
rst <- rast("GEE_out/trend_raster/total_evaporation_sum_all_lineartrend_slope-v3.tif")
rst <- mask.forest(rst, frst)
evapo_rst <- (rst / 1000000000) * 1000 * 120 # apply scale factor, m to mm, from months to decades
evapo_norm <- min_max_norm(evapo_rst) # betrag nehmen

# Precipitation - ET
rst <- rast("GEE_out/trend_raster/ET-Prec_lineartrend_slope.tif")
rst <- mask.forest(rst, frst)
dif <- rst/100 # apply scale factor

# stack the layers
climat_stack <- rast(list(T_rst, prec_rst, evapo_rst, dif)) # RGB of the three
names(climat_stack) <- c("Temperature", "Precipitation", "Evapotranspiration", "Water-Deficit")
climat_stack.proj <- project(climat_stack, "EPSG:25832")

## Plot of trend per Growing Area -----
grow_slope <- st_read("vector_data/grow_climate_vars-slope-v2.gpkg")
nudgx <- 0
#c(100000, 100000,200000,-100000,-100000, 50000,0, -150000, -150000, -50000, -50000, 10000,0, 100000, 0, 100000)
nudgy <- 0
#c(rep(0,5), 50000,0, -20000, 0,-100000,50000,100000, 50000, 0, -30000, 0)
grow_slope_tr <- st_transform(grow_slope, 3035)

#library(gghighlight)

plot_trend_map <- function(climate_param, lyr, palette, lims, labs, unit){
  gg <- ggplot()+
    #geom_sf(data = bayern, fill = "white", color = NA)+
    geom_spatraster(data = climat_stack[[lyr]])+
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]), crs = 3035)+
    scale_fill_gradientn(name = unit, 
                         colours = palette, 
                         na.value = NA, limits = lims)+
    #new_scale_color()+
    geom_sf(data = grow_slope_tr, fill = NA, color = "black")+
    #scale_colour_manual(name = "", values = c("black"),label = c("Naturparke"))+
    #geom_sf(data = bayern, fill = "white", color = NA)+
    geom_label_repel(aes(label = paste(labs, unit),
                         geometry = grow_slope_tr$geom), stat = "sf_coordinates",
                     size = 3, segment.curvature = -1e-20, hjust = 1,
                     nudge_x = nudgx, nudge_y = nudgy, box.padding = 0.5, alpha = 0.7)+
    #gghighlight(bez_bu_wg == "Schwaebisch-Bayerische Schotterplatten- und Altmoraenenlandschaft") +
    scale_y_continuous(breaks = seq(30,65, by = 1))+
    scale_x_continuous(breaks = seq(-15,40, by = 1))+ 
    theme(panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          #panel.background = element_blank(),
          #strip.background = element_blank(),
          legend.position = "bottom", legend.key.width = unit(0.5, "inches"))+
    xlab("Longitude")+
    ylab("Latitude")+
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0.4, "in"), pad_y = unit(0.75, "in"),
                           style = north_arrow_fancy_orienteering)+
    ggtitle(paste0(climate_param," increase in Bavarian Forests from 1950 until 2024"),
            subtitle = "ERA5 Climate Reanalysis Data over 6 Decades")

  ggsave(paste0("Plots/", climate_param, "trend-v4.png"), width = 20, height = 20, units = "cm",
         bg = 'transparent')
  return(gg)
}

temperature_plot <- plot_trend_map("Temperature", 1, brewer.pal(9, 'YlOrRd'), c(0.25,0.35), 
                                   round(grow_slope_tr$T_slope,3), "K/decade")
evapo_plot <- plot_trend_map("Evapotranspiration", 3, rev(brewer.pal(9, 'YlGnBu')), c(-0.044,-0.021), 
                             round(grow_slope_tr$evapo_slope,3), "mm/decade")
prec_plot <- plot_trend_map("Precipitation", 2, brewer.pal(11, 'Purples'),  c(0.5,2.6), 
                            round(grow_slope_tr$prec_slope,2), "mm/decade")

plot_trend_map("Water-Deficit", 4, brewer.pal(9, "PRGn"), c(-0.7, 1),
               round(grow_slope_tr$water_def,3), "mm/decade")

era_grid <- grid.arrange(temperature_plot, evapo_plot, prec_plot, ncol = 3, nrow = 1)
ggsave(plot = era_grid, filename = "Plots/erafive-grid.png", width = 30, height = 20, units = "cm")

# RGB Plots ----

file.list <- list.files("GEE_out/precipitation_trend_raster/", full.names = T)
library(stringr)
rgb.list <- list()
for (m in 1:12){
  month <- str_pad(m, 2, pad = "0")
  prec <- rast(file.list[grep(month, file.list)][3]) %>% mask.forest(frst)
  prec_norm <- prec *1000 * 10 # from m per year to mm per decade
  prec_norm <- min_max_norm(prec_norm)
  
  t <- rast(file.list[grep(month, file.list)][1]) %>% mask.forest(frst)
  t_norm <- t * 10 # from m per year to mm per decade
  t_norm <- min_max_norm(t_norm)
  
  evapo <- rast(file.list[grep(month, file.list)][2]) %>% mask.forest(frst)
  evapo_norm <- evapo *1000 * 10
  evapo_norm <- abs(evapo_norm) %>% min_max_norm()
  
  rgb <- rast(list(t_norm, prec_norm, evapo_norm))

  gg <- ggplot()+
    geom_sf(data = bayern, fill = "white", color = "black")+
    geom_spatraster_rgb(data = rgb, max_col_value = 1)+
    #geom_sf(data = background, fill = "grey", color = NA)+
    ggtitle(month.abb[m])+
    theme(panel.background = element_rect(fill = "white"),
          strip.background = element_rect(fill = NA))
  assign(paste0("RGB_", m), gg)
}

library(ggpubr)
monthly_rgbs <- ggarrange(RGB_1, RGB_2, RGB_3, RGB_4, RGB_5, RGB_6, RGB_7, RGB_8, RGB_9, RGB_10, RGB_11, RGB_12, 
          ncol = 4, nrow = 3)
rgb.grid <- grid.arrange(RGB_1, RGB_2, RGB_3, RGB_4, RGB_5, RGB_6, RGB_7, RGB_8, RGB_9, RGB_10, RGB_11, RGB_12, 
             ncol = 3, nrow = 4)

ggsave(plot = rgb.grid, filename = "Plots/monthlyRGBs-gridv2.png", width = 30, height = 40, units = "cm")

# Pie Charts / Spider Web Plot Forest Types