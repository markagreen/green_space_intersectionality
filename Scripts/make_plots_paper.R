#############################
### Make graphs for paper ###
#############################

## Purpose: To create the plots for insertion into the paper.

# Libraries
library(ggplot2)


## Figures 1 and 2 ##

# Load and tidy data
model1 <- read.csv("./sde_outputs_24062026/intersectional_group_pred_prob_cmd_post.csv") # Load
model1$group <- factor(model1$group, levels = c("1", "2", "3", "4", "5")) # Set IMD order correct

 
# Figure 1
plot1 <- ggplot(model1[model1$panel == "F",], aes(x = x, y = predicted, group = group, color = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, width = 0) + # Plot error bars (width gets rid of the whiskers, alpha is just for presentation so not overbearing)
  geom_point() + # Plot points (plot second so placed on top of the lines above)
  facet_wrap(~facet) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  ylim(0, 0.6) + # Define y-axis range
  labs(x = "Age group", y = "Probability of a common mental health diagnosis", color = "IMD Quintile", title = "Females", caption = "IMD = Index of Multiple Deprivation. Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot1) # Plot in viewer
# ggsave(plot = plot1, filename = "./Plots/figure1.jpeg", dpi = 300) # Quick save
ggsave(plot = plot1, filename = "./Plots/figure1.jpeg", width = 10.5, height = 6.5, units = "in", dpi = 300) # To match landscape in word format

# Figure 2
plot2 <- ggplot(model1[model1$panel == "M",], aes(x = x, y = predicted, group = group, color = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, width = 0) + # Plot error bars (width gets rid of the whiskers, alpha is just for presentation so not overbearing)
  geom_point() + # Plot points (plot second so placed on top of the lines above)
  facet_wrap(~facet) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  ylim(0, 0.6) + # Define y-axis range
  labs(x = "Age group", y = "Probability of a common mental health condition", color = "IMD Quintile", title = "Males", caption = "IMD = Index of Multiple Deprivation. Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot2) # Plot in viewer 
# ggsave(plot = plot2, filename = "./Plots/figure2.jpeg", dpi = 300) # Quick save
ggsave(plot = plot2, filename = "./Plots/figure2.jpeg", width = 10.5, height = 6.5, units = "in", dpi = 300) # To match landscape in word format

# Tidy
rm(model1, plot1, plot2)
gc()


## Figures 3 and 4 ##

# Load and tidy data
model2 <- read.csv("./sde_outputs_24062026/pred_prob_cmd_post_model2.csv") # Load
model2$imd <- factor(model2$imd, levels = c("1", "2", "3", "4", "5")) # Set IMD order correct


# Plot the variance by strata - females
plot3 <- ggplot(model2[model2$sex == "F",], aes(x = age, y = distance_gs_mid_km, group = imd, color = imd)) + 
  geom_hline(yintercept = 0, linetype = 2) + # Add dotted line for 0
  geom_point() + # Plot points)
  facet_wrap(~ethnicity) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  labs(x = "Age group", y = "Random slope", color = "IMD Quintile", title = "Females", caption = "IMD = Index of Multiple Deprivation. Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  ylim(-0.06, 0.06) + # Set limits for y-axis for consistency
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Make x-axis labels easier to see by placing at 45 degree angle
print(plot3) # Plot in viewer
ggsave(plot = plot3, filename = "./Plots/figure3.jpeg", width = 10.5, height = 6.5, units = "in", dpi = 300) # Save

# Plot the variance by strata - males
plot4 <- ggplot(model2[model2$sex == "M",], aes(x = age, y = distance_gs_mid_km, group = imd, color = imd)) + 
  geom_hline(yintercept = 0, linetype = 2) + # Add dotted line for 0
  geom_point() + # Plot points
  facet_wrap(~ethnicity) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  labs(x = "Age group", y = "Random slope", color = "IMD Quintile", title = "Males", caption = "IMD = Index of Multiple Deprivation. Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  ylim(-0.06, 0.06) + # Set limits for y-axis for consistency
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Make x-axis labels easier to see by placing at 45 degree angle
print(plot4) # Plot in viewer
ggsave(plot = plot4, filename = "./Plots/figure4.jpeg", width = 10.5, height = 6.5, units = "in", dpi = 300) # Save

# Tidy
rm(model2, plot3, plot4)
gc()

