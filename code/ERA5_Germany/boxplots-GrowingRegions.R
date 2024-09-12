# Boxplots per Wuchsgebiet

# forest types
mixed.forest <- frst
mixed.forest[!values(mixed.forest) == 23] <- NA
coni <- frst
coni[!values(coni) == 24] <- NA
broad <- frst
broad[!values(broad) == 25] <- NA

# extract values from Raster and covnert it to long table format
box.vals <- function(climat.rst, grow.vect, grow.ID){
  
  crp <- crop(climat.rst, grow.vect[grow.vect$ID == grow.ID,])
  msk <- mask(crp, grow.vect[grow.vect$ID == grow.ID,])
  msk.slope <- msk$slope
  
  # extract values
  vals1 <- mask(msk.slope, crop(mixed.forest, grow.vect[grow.vect$ID == grow.ID,])) %>% values()
  vals2 <- mask(msk.slope, crop(broad, grow.vect[grow.vect$ID == grow.ID,])) %>% values() 
  vals3 <- mask(msk.slope, crop(coni, grow.vect[grow.vect$ID == grow.ID,])) %>% values() 
  
  df <- data.frame('mixed' = vals1, 'coni' = vals2, 'broad' = vals3)
  colnames(df) <- c('Mixed Forest', 'Coniferous', 'Broad-leaved')
  df.long <- tidyr::pivot_longer(df, cols = c('Mixed Forest', 'Broad-leaved', 'Coniferous'))

  return(df.long)
}

# Function for Plotting
plt.boxplot <- function(df.long, var.name, grow.ID){
  
  #grow.name <- growing_areas_agg[growing_areas_agg$ID == grow.ID,]$Name
  tplot <- ggplot(df.long, aes(x = name, y = value, fill = name)) + 
    ## add half-violin from {ggdist} package
    ggdist::stat_halfeye(
      ## custom bandwidth
      adjust = .5, 
      ## adjust height
      width = .6, 
      ## move geom to the right
      justification = -.2, 
      ## remove slab interval
      .width = 0, 
      point_colour = NA
    ) + 
    scale_fill_manual(values = c("#80FF00", "#00A600", "#44AA99"))+
    geom_boxplot(
      width = .15, 
      ## remove outliers
      outlier.color = NA ## `outlier.shape = NA` or `outlier.alpha = 0` works as well
    )+
    xlab("")+
    ylab(paste0(var.name, " Slope"))+

    theme(panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          legend.position = "none")
    #ggtitle(grow.name)
  
  tplot <- tplot+ theme(text=element_text(size=20), #change font size of all text
               axis.text=element_text(size=20), #change font size of axis text
               axis.title=element_text(size=20))
  
  ggsave(plot = tplot, filename = paste0("Maps/Boxplots/", grow.ID, "_", var.name, "-Violinplot.png"), 
         width = 20, height = 15, units = "cm", bg = "transparent")

  return(tplot)
}

# Iterate over the Growing Regions
growing_areas_agg <- aggregate(growregion, by = list(growregion$bez_bu_wg), FUN = mean)
names(growing_areas_agg)[names(growing_areas_agg) == "Group.1"] <- "Name"
growing_areas_agg$ID <- c(1:82)

for (id in c(1:82)){
  df <- box.vals(T_rst, growing_areas_agg, id)
  plt.boxplot(df, "Temperature", id)
  
  df.et <- box.vals(ET_rst, growing_areas_agg, id)
  plt.boxplot(df.et, "Evapotranspiration", id)
}


# For entire Germany -----
# extract values

results_list <- list()
for (class.id in 23:25){
  frst.mask <- frst
  frst.mask[!values(frst.mask) == class.id] <- NA
  
  extr <- mask(T_rst$slope, frst.mask) %>% values()
  extr$class.id <- class.id
  results_list[[class.id]] <- extr
}
df <- bind_rows(results_list)

plt.boxplot(df, 'Temperature', 0)
box.vals(ET_rst$slope) %>% plt.boxplot(., 'Evaporation', 0)