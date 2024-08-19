# Trend per Growing Region and Forest Type

library(ggpubr)

climate.stack <- rast(list(T_rst$slope, ET_rst$slope))

# calculate mean per Growing Region

results <- list()
growing_areas_agg <- aggregate(growregion, by = list(growregion$bez_bu_wg), FUN = mean)
names(growing_areas_agg)[names(growing_areas_agg) == "Group.1"] <- "Name"
growing_areas_agg$ID <- c(1:82)

for (forest_type in 23:25) {
  masked_rasters <- mask(climate.stack, frst == forest_type, maskvalue = FALSE)
  means <- extract(masked_rasters, vect(growing_areas_agg), fun=mean, na.rm=TRUE)
  
  # Add the forest type column to the result
  means$forest_type <- forest_type
  means$name <- growing_areas_agg$name
  
  results[[as.character(forest_type)]] <- means
}

combined_results <- do.call(rbind, results)
growing_areas_with_means <- st_as_sf(merge(growing_areas_agg, combined_results, by="ID", all.x=TRUE))

colnames(growing_areas_with_means)[5:6] <- c("slope_Temperature", "slope_evaporation"
                                             )

# Plot Results

# create a grid columns are forest types

frst_types <- c("Broad-Leaved", "Decidious", "Mixed")
i <- 1
for (frst_type in 23:25){
  
  gg_T <- growing_areas_with_means[growing_areas_with_means$forest_type == frst_type,] %>% 
    ggplot()+
      geom_sf(aes(fill = slope_Temperature))+
      scale_fill_gradientn(name = "Trend per Growing Area \n[K/decade]",
                          colours = brewer.pal(9, 'YlOrRd'), limits = c(0.25,0.35),
                          na.value = "white")+
      theme+
      scale_y_continuous(breaks = seq(30,65, by = 1))+
      scale_x_continuous(breaks = seq(-15,40, by = 1))+ 
      xlab("Longitude")+
      ylab("Latitude")+
      ggtitle(paste0(frst_types[i], " Forest"), subtitle = "Temperature")
 
  
  assign(paste0("Temperature_", frst_type), gg_T)

  gg_evapo <- growing_areas_with_means[growing_areas_with_means$forest_type == frst_type,] %>% 
    ggplot()+
    geom_sf(aes(fill = slope_evaporation))+
    scale_fill_gradientn(name = "Trend per Growing Area \n[mm/decade]",
                         colours = rev(brewer.pal(9, 'YlGnBu')), limits = c(-1.3,-0.5),
                         na.value = "white")+
    theme+
    scale_y_continuous(breaks = seq(30,65, by = 1))+
    scale_x_continuous(breaks = seq(-15,40, by = 1))+ 
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle(label = "", subtitle = "Evapotranspiration")
  
  
  assign(paste0("Evapo", frst_type), gg_evapo)
  
  i <- i+1
}

grid <- ggarrange(Temperature_23, Temperature_24, Temperature_25,
          Evapo23, Evapo24, Evapo25,
          ncol = 3, nrow = 2)
ggsave(plot = grid, filename = "Maps/Temperature_evapo_perForestType.png", width = 30, height = 40, units = "cm")

#ggsave(paste0("Maps/", 'Temperature_slope_perGrowRegion.png'), width = 20, height = 20, units = "cm")
