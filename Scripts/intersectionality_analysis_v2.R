###############################
### Intersectionality model ###
###############################

## Aim: To explore how mental health outcomes are inequitable in who they affect

# Libraries
library(performance) # Helps tidy up the regression output
library(viridisLite) # Colour-blind friendly colours
library(parameters) # Gives model fit statistics
library(data.table) # For faster data wrangling
library(ggeffects) # For estimating the predictive and marginal effects of models
library(parallel) # To allow the multi-level model to be parallelised
library(glmmTMB) # Multi-level model
library(ggplot2) # For plotting data
library(insight) # Helps with extracting information from models
cores <- detectCores() # Identify how many cores we have access to for parallelisation
cores # Print


## Get data ready for analysis ##

# Load electronic health records datasets
spine <- fread("./Data/population_spine.csv") # Population spine
ethnicity <- fread("./Data/ethnicity.csv") # Ethnicity
ethnicity2 <- fread("./Data/ethnicity2.csv") # Chris' ethnicity records
cmd_gp <- fread("./Data/common_mental_health_diagnoses.csv") # Common mental health conditions (GP events)
cmd_med <- fread("./Data/common_mental_health_meds.csv") # Common mental health conditions (medications)

# Remove column as repeated throughout and causes issues with merge
spine$V1 <- NULL # Remove column as repeated throughout and causes issues with merge
ethnicity$V1 <- NULL
ethnicity2$V1 <- NULL
cmd_gp$V1 <- NULL
cmd_med$V1 <- NULL

# Create deprivation quintiles
spine$imd_quintiles <- NA # Create blank variable
spine$imd_quintiles[spine$IMD_Score >= 1 & spine$IMD_Score < 6570] <- 1 # Most deprived quintile
spine$imd_quintiles[spine$IMD_Score >= 6570 & spine$IMD_Score < 13139] <- 2 # Quintile 2
spine$imd_quintiles[spine$IMD_Score >= 13139 & spine$IMD_Score < 19707] <- 3 # Quintile 3
spine$imd_quintiles[spine$IMD_Score >= 19707 & spine$IMD_Score < 26276] <- 4 # Quintile 4
spine$imd_quintiles[spine$IMD_Score >= 26276 & spine$IMD_Score <= 32844] <- 5 # Least deprived

# Join all data together
all_data <- merge(spine, cmd_gp, by.x = "PK_Patient_ID", by.y = "FK_Patient_ID", all.x = TRUE) # Join on mental health GP events to spine
all_data <- merge(all_data, cmd_med, by.x = "PK_Patient_ID", by.y = "FK_Patient_ID", all.x = TRUE) # Join on mental health medications to spine
all_data <- merge(all_data, ethnicity, by.x = "FK_Patient_Link_ID", by.y = "PK_Patient_Link_ID", all.x = TRUE) # Join on various ethnicity data
all_data <- merge(all_data, ethnicity2, by.x = "NWSDE_Pseudo_Number", by.y = "nwid", all.x = TRUE) # Join on various ethnicity data
rm(spine, cmd_gp, cmd_med, ethnicity, ethnicity2) # Tidy
gc()

# Create outcome measures
all_data$any_cmd_pre <- 0 # Any with a NA means that are not in the system therefore 0
all_data$any_cmd_pre[all_data$cmd_gp_event_pre >= 1 | all_data$cmd_med_pre >= 1] <- 1 # Either a diagnosis or medication two years pre-exposure
all_data$any_cmd_post <- 0 # Any with a NA means that are not in the system therefore 0
all_data$any_cmd_post[all_data$cmd_gp_event_post >= 1 | all_data$cmd_med_post >= 1] <- 1 # Either a diagnosis or medication 18 months post-exposure
all_data$any_cmd_new <- 0 # Any with a NA means that are not in the system therefore 0
all_data$any_cmd_new[all_data$cmd_gp_event_new_diag >= 1 | all_data$cmd_gp_event_first_med >= 1] <- 1 # Either a first diagnosis or first medication 18 months post-exposure

# Deal with missing data
nrow(all_data) # n = 2 974 672 (this is all registered with a GP in C&M)
all_data <- all_data[all_data$Deceased == "N"] # Drop people who had died (n = 141 121 dropped)
all_data <- all_data[!is.na(all_data$pseudo_uprn)] # Drop missing UPRN (n = 213 063)
all_data <- all_data[!is.na(all_data$imd_quintiles)] # Drop missing IMD (n = 2444)
all_data <- all_data[all_data$Sex != "U"] # Drop missing Sex (n = 197)
# I did look at dropping missing age here but there were none
all_data <- all_data[!is.na(all_data$distance_local_greenspace_description)] # Missing distance to nearest green space (n = 208 074)
nrow(all_data) # n = 2 409 771

# Create new age groups
breaks <- c(0, 17, 24, 34, 44, 54, 64, 74, 121) # Define age group cut points
labels <- c("0-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+") # Define labels for age group
all_data$age_group <- cut(all_data$age, breaks = breaks, labels = labels, right = FALSE) # Calculate which age group each person belongs to based on age
all_data <- all_data[all_data$age >= 18] # Drop people aged <18 (n = 447 540)
rm(labels, breaks)

# Create new ethic measure
all_data$EthnicSubGroup <- all_data$eth_fine # Use Chris' measure
all_data$EthnicSubGroup[is.na(all_data$EthnicSubGroup)] <- "Unknown" # Add to missing records
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Discordant"] <- "Unknown" # Collapse some groups
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Arab"] <- "Other ethnic group" # Collapse some groups
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Asian Bangladeshi"] <- "Bangladeshi" # Rename to shorter names
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Asian Indian"] <- "Indian"
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Asian Pakistani"] <- "Pakistani"
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Black Caribbean"] <- "Black Other"
all_data$EthnicSubGroup[all_data$EthnicSubGroup == "Mixed Other" | all_data$EthnicSubGroup == "Mixed White and Asian" | all_data$EthnicSubGroup == "Mixed White and Black African" | all_data$EthnicSubGroup == "Mixed White and Black Caribbean"] <- "Mixed"

# Set measures to factors to help modelling later
all_data$Sex <- as.factor(all_data$Sex)
all_data$age_group <- as.factor(all_data$age_group)
all_data$EthnicSubGroup <- as.factor(all_data$EthnicSubGroup)
all_data$imd_quintiles <- as.factor(all_data$imd_quintiles)

# Set reference group in factors
all_data <- within(all_data, age_group <- relevel(age_group, ref = "18-24"))
all_data <- within(all_data, Sex <- relevel(Sex, ref = "M"))
all_data <- within(all_data, EthnicSubGroup <- relevel(EthnicSubGroup, ref = "White British"))
all_data <- within(all_data, imd_quintiles <- relevel(imd_quintiles, ref = "5"))

# Create efficient data object to save memory in the environment (saves ~60% memory)
all_data <- all_data[, c("NWSDE_Pseudo_Number", "age_group", "Sex", "EthnicSubGroup", "imd_quintiles", "distance_local_greenspace_description", "any_cmd_pre", "any_cmd_post", "any_cmd_new")]
gc()


### Descriptives ###

# Outcomes
# table(all_data$any_cmd_pre)
table(all_data$any_cmd_post)

# Covariates
table(all_data$age_group) # Age
table(all_data$Sex) # Sex
table(all_data$EthnicSubGroup) # Ethnicity
table(all_data$imd_quintiles) # IMD
table(all_data$distance_local_greenspace_description) # Distance to green space

# Adjust exposure to km
parts <- do.call(rbind, strsplit(gsub("m|km", "", all_data$distance_local_greenspace_description), "_")) # Split out the band into numeric values
all_data$distance_gs_mid <- rowMeans(apply(parts, 2, as.numeric), na.rm = TRUE) # Calculate mid point in m
all_data$distance_gs_mid_km <- all_data$distance_gs_mid / 1000 # Adjust to km
mean(all_data$distance_gs_mid_km)


### Multi-level models ###


## Null model ##

# Note: This is used as a baseline to assess how much variation is accounted for by the intersectional groups and for comparing to later models.

# Fit model (~1 min)
null_model <- glmmTMB(
  any_cmd_post ~ (1 | Sex:age_group:EthnicSubGroup:imd_quintiles), # This is same as above
  data = all_data, # Dataset
  # data = all_data[1:5000], # Set to just n=5000 for testing purposes
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

model_parameters(null_model) # Model coefficients (fixed and random effects) - takes ~1 minute
model_performance(null_model) # Model fit statistics - takes ~2 minutes
# icc(null_model) # If just want the ICC but is given in the model fit above

## Comparing the different effects of each social measure ##

# Note: Here we assess the differences in model performance in accounting for each social category. We could have more granular groups at this stage too (i.e., define the intersections on broader ethnic groups and then use more granular groups as fixed effects).

# Adjusting for sex only
sex_model <- glmmTMB(
  any_cmd_post ~ factor(Sex) + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles),
  data = all_data,
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Adjusting for age only
age_model <- glmmTMB(
  any_cmd_post ~ factor(age_group) + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles),
  data = all_data,
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Adjusting for ethnicity only
ethnicity_model <- glmmTMB(
  any_cmd_post ~ factor(EthnicSubGroup) + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles),
  data = all_data,
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Adjusting for deprivation only
imd_model <- glmmTMB(
  any_cmd_post ~ factor(imd_quintiles) + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles),
  data = all_data,
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Fit fully adjusted model
full_model <- glmmTMB(
  any_cmd_post ~ factor(Sex) + factor(age_group) + factor(EthnicSubGroup) + factor(imd_quintiles) + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles),
  data = all_data,
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Store the unadjusted fixed effects
results1 <- as.data.frame(compare_parameters(sex_model, age_model, ethnicity_model, imd_model, full_model, exponentiate = TRUE))
write.csv(results1, "./Outputs/Tables/cmd_post_results_model1.csv") # Save

# Store all model fit statistics
models <- list(m1 = null_model, m2 = sex_model, m3 = age_model, m4 = ethnicity_model, m5 = imd_model, m6 = full_model) # Create list of all models
icc_vars <- sapply(models, icc) # Get ICC values
var_vals <- sapply(models, function(m) {
  get_variance(m)$var.random
}) # Get variance of model random effect
sd_vals <- sqrt(var_vals) # Get standard deviation of random effect
null_var <- get_variance(null_model)$var.random # Get null model variance for PCV
pcv_vals <- sapply(models, function(m){
  v <- get_variance(m)$var.random
  (null_var - v) / null_var
}) # Calculate PCV in relation to null model
mf_res <- data.frame(model = names(models), unadjusted_icc = as.numeric(icc_vars[3,]), adjusted_icc = as.numeric(icc_vars[1,]), variance = as.numeric(var_vals), std_dev = as.numeric(sd_vals), pcv = c(pcv_vals)) # Store in table
write.csv(mf_res, "./Outputs/Tables/model_fit_cmd_post_model1.csv")
rm(models, icc_vars, var_vals, sd_vals, null_var, pcv_vals, mf_res) # Tidy

# Make predictions 
pred <- predict_response( # Takes 80 seconds to fit
  null_model,
  c("age_group", "imd_quintiles", "EthnicSubGroup", "Sex"), # Play around with the order helps with the next step - the first one is the x-axis ('x' in data frame), the second one is groups on the plot ('group' on data frame), third one is the facets ('facet' in data frame), and fourth separates out the plots ('panel' in the data frame). I like this one
  #c("age_group", "EthnicSubGroup", "imd_quintiles", "Sex"), # Alternative #1
  #c("imd_quintiles", "EthnicSubGroup", "age_group", "Sex"), # Alternative #2
  type = "random",
  interval = "confidence"
)

# Statistical Disclosure Checks (SDC) - so that outputs can be safe to output
# all_data <- within(all_data, imd_quintiles <- relevel(imd_quintiles, ref = "1")) # For plotting purposes
all_data[, intersection := interaction(Sex, age_group, EthnicSubGroup, imd_quintiles, drop = TRUE)] # Create intersection strata variable
pred_dt <- as.data.table(pred) # Convert predictions to data.table for next step
pred_dt[, intersection := interaction(panel, x, facet, group, drop = TRUE)] # Create same intersectional strata variable
nrow(pred_dt) # Get total number of intersectional strata overall in model (n = 1190)
grp_count <- all_data[, .N, by = intersection] # Frequency count for all intersectional strata
filter_predictions <- pred_dt[intersection %in% grp_count[N >= 10, intersection]] # Keep only those who have n>=10 counts for SDC reasons
nrow(filter_predictions) # Check how many strata remain - n = 1136 (95.46%)
grp_count <- grp_count[grp_count$N >= 10] # Save list of intersectional strata included in the plot
fwrite(grp_count, "./Outputs/Tables/intersectional_group_frequencies_cmd_post.csv") # Save to share in SDC process
fwrite(filter_predictions, "./Outputs/Tables/intersectional_group_pred_prob_cmd_post.csv") # Save to share in SDC process
rm(grp_count, pred_dt, pred) # Tidy

# Recreate in ggplot2 - females
filter_predictions$group <- factor(filter_predictions$group, levels = c("1", "2", "3", "4", "5")) # Set IMD order correct
plot1f <- ggplot(filter_predictions[filter_predictions$panel == "F",], aes(x = x, y = predicted, group = group, color = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, width = 0) + # Plot error bars (width gets rid of the whiskers, alpha is just for presentation so not overbearing)
  geom_point() + # Plot points (plot second so placed on top of the lines above)
  facet_wrap(~facet) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  ylim(0, 0.6) + # Define y-axis range
  labs(x = "Age group", y = "Probability of a common mental health condition", color = "IMD Quintile", title = "Females", caption = "IMD = Index of Multiple Deprivation (2019 version). Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot1f) # Plot in viewer
ggsave(plot = plot1f, filename = "./Outputs/Plots/cmd_post_females.jpeg", dpi = 300)

# Males plot
plot1m <- ggplot(filter_predictions[filter_predictions$panel == "M",], aes(x = x, y = predicted, group = group, color = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.4, width = 0) + # Plot error bars (width gets rid of the whiskers, alpha is just for presentation so not overbearing)
  geom_point() + # Plot points (plot second so placed on top of the lines above)
  facet_wrap(~facet) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  ylim(0, 0.6) + # Define y-axis range
  labs(x = "Age group", y = "Probability of a common mental health condition", color = "IMD Quintile", title = "Males", caption = "IMD = Index of Multiple Deprivation (2019 version). Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot1m) # Plot in viewer 
ggsave(plot = plot1m, filename = "./Outputs/Plots/cmd_post_males.jpeg", dpi = 300)


### Bringing in access to green space ###

# Adjust exposure to km
parts <- do.call(rbind, strsplit(gsub("m|km", "", all_data$distance_local_greenspace_description), "_")) # Split out the band into numeric values
all_data$distance_gs_mid <- rowMeans(apply(parts, 2, as.numeric), na.rm = TRUE) # Calculate mid point in m
all_data$distance_gs_mid_km <- all_data$distance_gs_mid / 1000 # Adjust to km

# Fit unadjusted model (takes ~1 min)
unajd_model_gs <- glmmTMB(
  any_cmd_post ~ distance_gs_mid_km + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles), 
  data = all_data, # Dataset
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Fit fully adjusted model (takes ~2 mins)
adj_model_gs <- glmmTMB(
  any_cmd_post ~ distance_gs_mid_km + factor(Sex) + factor(age_group) + factor(EthnicSubGroup) + factor(imd_quintiles) + (1 | Sex:age_group:EthnicSubGroup:imd_quintiles),
  data = all_data,
  family = "binomial",
  control = glmmTMBControl(parallel = cores)
)

# Give random slopes based on distance to nearest green space (group specific slopes)
# This allows the association between green space and mental health to vary for each intersectional group
# Here the fixed effect gives the average association across everyone (independent of strata)
# The random slope (variance) then tells you how much this association varies across all strata
# Then plot the slopes for each strata
# Is slow to fit 
slopes_model_gs <- glmmTMB(
  any_cmd_post ~ distance_gs_mid_km + (distance_gs_mid_km | Sex:age_group:EthnicSubGroup:imd_quintiles), 
  data = all_data, 
  family = "binomial",
  control = glmmTMBControl(parallel = 2)
)

# Compare model summaries of fixed effects across all models
results2 <- as.data.frame(compare_parameters(unajd_model_gs, adj_model_gs, slopes_model_gs, exponentiate = TRUE))
write.csv(results2, "./Outputs/Tables/cmd_post_results_model2.csv") # Save

# Store all model fit statistics
models <- list(m1 = null_model, m2 = unajd_model_gs, m3 = adj_model_gs, m4 = slopes_model_gs) # Create list of all models
icc_vars <- sapply(models, icc) # Get ICC values
var_vals <- sapply(models, function(m) {
  get_variance(m)$var.random
}) # Get variance of model random effect
sd_vals <- sqrt(var_vals) # Get standard deviation of random effect
null_var <- get_variance(null_model)$var.random # Get null model variance for PCV
pcv_vals <- sapply(models, function(m){
  v <- get_variance(m)$var.random
  (null_var - v) / null_var
}) # Calculate PCV in relation to null model
mf_res <- data.frame(model = names(models), unadjusted_icc = as.numeric(icc_vars[3,]), adjusted_icc = as.numeric(icc_vars[1,]), variance = as.numeric(var_vals), std_dev = as.numeric(sd_vals), pcv = c(pcv_vals)) # Store in table
write.csv(mf_res, "./Outputs/Tables/model_fit_cmd_post_model2.csv")
rm(models, icc_vars, var_vals, sd_vals, null_var, pcv_vals, mf_res) # Tidy

# Extract variance across strata to examine the extent of the random effect
randomslopes <- ranef(slopes_model_gs) # Get random effects for each strata
re_model <- data.frame(randomslopes$cond$`Sex:age_group:EthnicSubGroup:imd_quintiles`) # Store as data.frame
re_model$intersection <- rownames(re_model) # Store names as variable
rm(randomslopes)

# Tidy up the strata for plotting purposes
strings <- strsplit(re_model$intersection, ":")  # Split the strata into individual components (stores as a list)
strings_mat <- data.frame(do.call(rbind, strings)) # Convert the list into a data.frame
re_model$sex <- strings_mat$X1 # Store each variable separately - sex
re_model$age <- strings_mat$X2 # repeat again - age band
re_model$ethnicity <- strings_mat$X3 # Ethnicity
re_model$imd <- strings_mat$X4 # IMD

# Remove the small counts for SDC reasons
re_model$intersection <- gsub(":", ".", re_model$intersection) # Change colons to full stops to match below
grp_count <- fread("./Outputs/Tables/intersectional_group_frequencies_cmd_post.csv") # Load strata counts if not already present
re_model <- data.table(re_model) # Convert format for next step
filter_re_model <- re_model[intersection %in% grp_count[N >= 10, intersection]] # Keep only those who have n>=10 counts for SDC reason
nrow(filter_re_model) # How many strata left - n=1136
write.csv(filter_re_model, "./Outputs/Tables/pred_prob_cmd_post_model2.csv") # Save values

# How to interpret these values:
# The random slope model gives a unique slope to each strata - this value shows how much the effect differs for each strata compared to the overall average effect. A positive value suggests that the effect for the distance to nearest greenspace (in this example) is stronger (more positive) than the average effect for all groups (i.e., greenspace matters more). Conversely a negative effect means that the effect is weaker for that group - or even more negative.
# The values here are small though - so suggest that the variation is not a big effect. This is especially so as the odds ratio of the coefficient suggests that it is 0.999 - so little association in the general population. 

# Plot the variance by strata - females
plot2f <- ggplot(filter_re_model[filter_re_model$sex == "F",], aes(x = age, y = distance_gs_mid_km, group = imd, color = imd)) + 
  geom_hline(yintercept = 0, linetype = 2) + # Add dotted line for 0
  geom_point() + # Plot points)
  facet_wrap(~ethnicity) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  labs(x = "Age group", y = "Random slope", color = "IMD Quintile", title = "Females", caption = "IMD = Index of Multiple Deprivation (2019 version). Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  ylim(-0.2, 0.2) + # Set limits for y-axis for consistency
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Make x-axis labels easier to see by placing at 45 degree angle
print(plot2f) # Plot in viewer
ggsave(plot = plot2f, filename = "./Outputs/Plots/re_females_cmd_post.jpeg") # Save

# Plot the variance by strata - males
plot2m <- ggplot(filter_re_model[filter_re_model$sex == "M",], aes(x = age, y = distance_gs_mid_km, group = imd, color = imd)) + 
  geom_hline(yintercept = 0, linetype = 2) + # Add dotted line for 0
  geom_point() + # Plot points
  facet_wrap(~ethnicity) + # Plot each panel
  scale_colour_viridis_d() + # Make plot colour blind friendly
  labs(x = "Age group", y = "Random slope", color = "IMD Quintile", title = "Males", caption = "IMD = Index of Multiple Deprivation (2019 version). Quintile 1 = most deprived areas, quintile 5 = least deprived areas.") + # Add labels to plot
  ylim(-0.2, 0.2) + # Set limits for y-axis for consistency
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Make x-axis labels easier to see by placing at 45 degree angle
print(plot2m) # Plot in viewer
ggsave(plot = plot2m, filename = "./Outputs/Plots/re_males_cmd_post.jpeg") # Save

# Delete all objects bar main data
rm(list = setdiff(ls(), c("all_data", "cores")))
gc()



