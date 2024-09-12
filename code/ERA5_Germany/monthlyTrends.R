# Monthly Trends

library(ggnewscale)

monthly_ET <- rast("Data/GEE_export/Trend_ObsPeriod/total_evaporation_sum_Monthly_SensSlope.tif") %>% 
  mask.forest(frst)
names(monthly_ET) <- month.abb
writeRaster(monthly_ET, "Data/Final-Results/monthly_trend_raster/total_evaporation_sum_Monthly_SensSlope.tif")

monthly_prec <- rast("Data/GEE_export/Trend_ObsPeriod/total_precipitation_sum_Monthly_SensSlope.tif") %>% 
  mask.forest(frst)
names(monthly_prec) <- month.abb
writeRaster(monthly_prec, "Data/Final-Results/monthly_trend_raster/total_precipitation_sum_Monthly_SensSlope.tif")

monthly_T <- rast("Data/GEE_export/Trend_ObsPeriod/temperature_2m_Monthly_SensSlope.tif") %>% 
  mask.forest(frst)
names(monthly_T) <- month.abb
writeRaster(monthly_T, "Data/Final-Results/monthly_trend_raster/temperature_2m_Monthly_SensSlope.tif")


# Plotting
dif <- st_difference(st_as_sfc(bb), ger)
names(monthly_T) <- month.abb

ggT <- ggplot()+
  
  geom_spatraster(data = monthly_T)+
  facet_wrap(~lyr, ncol = 4)+
  xlim(c(bb[1], bb[3]))+
  ylim(c(bb[2], bb[4]))+
  scale_fill_gradientn(name = 'K/month', colors = brewer.pal(9, 'YlOrRd'), limits = c(0.025,0.06),
                       na.value = "grey")+
  #new_scale_fill()+
  geom_sf(data = dif, fill = 'white', color = 'black')+
  theme+
  scale_y_continuous(breaks = seq(30,65, by = 2))+
  scale_x_continuous(breaks = seq(-15,40, by = 2))+ 
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Monthly Senns Slope of ERA5 Temperature (2m)")

ggsave(plot = ggT, filename = "Maps/Temp_monthlySlope.png", width = 20, height = 20, units = 'cm')

# Evapotranspiration
names(monthly_ET) <- month.abb

ggET <- ggplot()+
  geom_spatraster(data = monthly_ET)+
  facet_wrap(~lyr, ncol = 4)+
  xlim(c(bb[1], bb[3]))+
  ylim(c(bb[2], bb[4]))+
  scale_fill_gradientn(name = 'mm/month', colors = rev(brewer.pal(9, 'YlGnBu')), limits = c(-0.0009, -0.00001),
                       na.value = "grey")+
  #new_scale_fill()+
  geom_sf(data = dif, fill = 'white', color = 'black')+
  theme+
  scale_y_continuous(breaks = seq(30,65, by = 2))+
  scale_x_continuous(breaks = seq(-15,40, by = 2))+ 
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("Monthly Senns Slope of ERA5 Evaporation Sum")

ggsave(plot = ggET, filename = "Maps/ET_monthlySlope.png", width = 20, height = 20, units = 'cm')
