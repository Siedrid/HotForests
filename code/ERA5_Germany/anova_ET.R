# ANOVA Test for Forest Types

rst_stack <- c(frst, ET_rst$slope, T_rst$slope)
forest_data <- as.data.frame(rst_stack, xy = TRUE, na.rm = TRUE)
colnames(forest_data) <- c('X', 'Y', 'Forest_type', 'ET', 'Temp')
forest_data$Forest_type <- as.factor(forest_data$Forest_type)

hist(forest_data$Temp, breaks = 20, probability = TRUE, main = "Histogram of Temperature", xlab = "Temperature")
lines(density(forest_data$Temp), col = "blue", lwd = 2)

anova_T <- aov(Temp ~ Forest_type, data = forest_data)
s <- summary(anova_T)
anova_T$qr
# TukeyHSD(anova_T)

anova_ET <- aov(ET ~ Forest_type, data = forest_data)
summary(anova_ET)

# T-Test

with(forest_data, t.test(ET[Forest_type == '23'], ET[Forest_type == '24']))
with(forest_data, t.test(ET[Forest_type == '23'], ET[Forest_type == '25']))
with(forest_data, t.test(ET[Forest_type == '25'], ET[Forest_type == '24']))

with(forest_data, t.test(Temp[Forest_type == '23'], Temp[Forest_type == '24']))
with(forest_data, t.test(Temp[Forest_type == '23'], Temp[Forest_type == '25']))
with(forest_data, t.test(Temp[Forest_type == '25'], Temp[Forest_type == '24']))


# Plotting

# Calculate means and standard errors
library(dplyr)
library(ggplot2)
library(ggpubr)

# Set the random seed for reproducibility
set.seed(42)

# Number of rows for the random data
n_rows <- 50

# Generate the random dataset
random_forest_data <- data.frame(
  Forest_type = sample(c("23", "24", "25"), n_rows, replace = TRUE),
  Temp = rnorm(n_rows, mean = 25, sd = 5),
  Evaporation = rnorm(n_rows, mean = 200, sd = 50)
)

# Function for Plotting
# https://r-charts.com/distribution/violin-plot-mean-ggplot2/
# higher transparency of violin
# whiskers for the standard deviation
# p value of Anova

plt.violin_with_mean_sd <- function(df.long, var.name){
  
  anova_T <- aov(var.name ~ Forest_type, data = df.long)
  anova_p <- summary(anova_T)[[1]][["Pr(>F)"]][1]
  
  tplot <- ggplot(df.long, aes(x = Forest_type, y = var.name, fill = Forest_type)) + 
    ## add half-violin from {ggdist} package
    geom_violin(
      width = .6, 
      adjust = 1,   # Smoothing adjustment
      trim = TRUE,  # Trim the violins to the data range
      colour = NA,  # Remove the outline
      alpha = 0.6   # Set transparency
    ) + 
    scale_fill_manual(values = c("#80FF00", "#00A600", "#44AA99")) +
    
    # Add mean and SD
    stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",
                 colour = "black") +
    
    # Add the p-value as a text annotation
    annotate("text", 
             x = 2.5,                     # X-coordinate for placement
             y = max(var.name) * 1.1, # Y-coordinate for placement (adjust for space above violins)
             label = paste0("ANOVA p = ", anova_p), 
             size = 5,                    # Text size
             hjust = 0,                   # Align to the left
             color = "black") +           # Text color
    
    xlab("") +
    ylim(c(0.27, 0.32))+
    scale_x_discrete(
      labels = c("23" = "Mixed", "24" = "Coniferous", "25" = "Broad-Leaved")
    )+
    ylab(paste0(" Slope")) +
    theme_minimal()+
    
    theme(legend.position = "none")
  # ggtitle(grow.name)
  
  tplot <- tplot + theme(text=element_text(size=20), # change font size of all text
                         axis.text=element_text(size=20), # change font size of axis text
                         axis.title=element_text(size=20))
  
  # Save the plot
  #ggsave(plot = tplot, filename = paste0("Maps/Boxplots/", grow.ID, "_", var.name, "-Violinplot.png"), 
   #      width = 20, height = 15, units = "cm", bg = "transparent")
  
  return(tplot)
}
gc()
# plt.violin_with_mean_sd(random_forest_data, random_forest_data$Temp)
plt.violin_with_mean_sd(forest_data, forest_data$Temp)


# very basic plot
library(dplyr)

#calculate mean and sd of points by team
df_mean_std <- forest_data %>%
  group_by(Forest_type) %>%
  summarise_at(vars(ET), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

#view results
df_mean_std
ggplot(df_mean_std , aes(x=Forest_type, y=mean)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.3) +
  geom_point(size=2)

ggplot(forest_data, aes(x = Temp, colour = Forest_type)) +
  geom_density()
