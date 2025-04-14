# preamble - required packages----
# list required packages
package.list <- (c("ggplot2", "dplyr", "ggpmisc", "tidyr",
                   "ordinal", "MASS", "systemfit"))
# load packages
lapply(package.list, require, character.only = TRUE)



# import and merge data=============
# Download the full ASHRAE Global Thermal Comfort Database II dataset
# from https://datadryad.org/stash/dataset/doi:10.6078/D1F671
# and save files to the relevant folder (change file names accordingly)

buildings_dataframe <- read.csv("C:/Users/AKumarMishra/OneDrive/databases/ASHRAEDBII/db_metadata.csv")
indoorEnvironmentData <- read.csv("C:/Users/AKumarMishra/OneDrive/databases/ASHRAEDBII/db_measurements/db_measurements_v2.1.0.csv")

# merge the metadata and ieq data by building id
mergedIEQData <- merge(buildings_dataframe, indoorEnvironmentData, by = "building_id")

df_rawdata <- mergedIEQData

# Keep complete cases for specified columns
cols_1 <- c('set', 'top', 'thermal_sensation')
df_data <- df_rawdata[complete.cases(df_rawdata[cols_1]), ]

# Select the specific building 
df_data_1bldg <- subset(df_data, df_data$building_id == 735)


## a linear model for the building------
### TSV~ SET model----
# generate the model
lm_model <- lm(thermal_sensation ~ set, data = df_data_1bldg)

# Extract the coefficients
intercept <- coef(lm_model)[1]
slope <- coef(lm_model)[2]

# Calculate the x-intercept where y = 0
x_intercept <- -intercept / slope

# build plot
p <- ggplot(df_data_1bldg, aes(set, thermal_sensation)) + 
  geom_point(size=2, color="#56B4E9") +
  geom_smooth(method="lm", color="#999999", se=F, size=2) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               label.x.npc = "left", label.y.npc = 0.90, #set the position of the eq
               formula = y ~ x, parse=TRUE,rr.digits = 3)+ # add eq to plot
  theme_classic()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12),
        panel.grid.major = element_line(color = "grey80"),  
        panel.grid.minor = element_line(color = "grey90"))+
  scale_x_continuous(breaks = c(19, 22, 25, 28, 31)) +  
  labs(x = "SET, °C", y = "Thermal sensation votes")+
  theme(axis.title=element_text(size=12),
        axis.text=element_text(size=12),
        legend.position = "none")

# Build the plot and extract the limits
plot_build <- ggplot_build(p)

# Extract the x and y axis limits
x_limits <- plot_build$layout$panel_params[[1]]$x.range
x_axis_min <- x_limits[1]
x_axis_max <- x_limits[2]
y_limits <- plot_build$layout$panel_params[[1]]$y.range
y_axis_min <- y_limits[1]
y_axis_max <- y_limits[2]

# add lines to x- and y-axis from the "neutral" point
p + geom_segment(aes(x = x_intercept, y = y_axis_min, xend = x_intercept, yend = 0),
                 linetype = "dashed", color = "#228833", size = 1)+
  geom_segment(aes(x = x_axis_min, y = 0, xend = x_intercept, yend = 0),
               linetype = "dashed", color = "#228833", size = 1)

### TSV ~ SET model----
# generate the model
lm_model_TSV <- lm(thermal_sensation ~ set, data = df_data_1bldg)
# output summary of model
summary(lm_model_TSV)

## an ordinal model for the building----
# Create the TSV column by binning the thermal sensations
df_data_1bldg <- df_data_1bldg %>%
  mutate(TSV = case_when(
    thermal_sensation >= -3 & thermal_sensation < -2.5 ~ "Cold",
    thermal_sensation >= -2.5 & thermal_sensation < -1.5 ~ "Cool",
    thermal_sensation >= -1.5 & thermal_sensation < -0.5 ~ "Slightly cool",
    thermal_sensation >= -0.5 & thermal_sensation <= 0.5 ~ "Neutral",
    thermal_sensation > 0.5 & thermal_sensation <= 1.5 ~ "Slightly warm",
    thermal_sensation > 1.5 & thermal_sensation <= 2.5 ~ "Warm",
    thermal_sensation > 2.5 ~ "Hot"
  ))

# redefine TSV as a factor data and set the levels
df_data_1bldg$TSV <- factor(df_data_1bldg$TSV, 
                            levels = c("Cold", "Cool", "Slightly cool", 
                                       "Neutral", "Slightly warm", 
                                       "Warm", "Hot"))

# Fit an ordinal model
modelSET <- polr(TSV ~ set , data = df_data_1bldg)

###  calcualte predicted probabilities for plot----
# create data set based on the observed SET data
newdat <- data.frame(set = seq(min(df_data_1bldg$set),
                               max(df_data_1bldg$set),
                               length.out = 1000))
# add model predictions to data set
newdat <- cbind(newdat, predict(modelSET, newdat, type = "probs"))
predicted_probs <- as.data.frame(predict(modelSET, newdata = newdat,
                                         type = "probs"))
predicted_probs <- cbind(predicted_probs, newdat[,1])

#rename columns
colnames(predicted_probs) <- c("Cold", "Cool", "Slightly cool", "Neutral",
                               "Slightly warm", "Warm", "Hot", "set")

# convert data to long form for plot
long_df <- pivot_longer(predicted_probs, 
                        cols = c(Cold, Cool, `Slightly cool`, Neutral, 
                                 `Slightly warm`, Warm, Hot), 
                        names_to = "TSV", 
                        values_to = "Probability")

# If you want to reorder the levels of 'TSV' column
long_df$TSV <- factor(long_df$TSV, 
                      levels = c("Cold", "Cool", "Slightly cool",
                                 "Neutral", "Slightly warm", "Warm", "Hot"))

# Find the maximum probability for each level of TSV and the corresponding SET
max_prob_points <- long_df %>%
  dplyr::group_by(TSV) %>%
  dplyr::summarise(
    max_probability = max(Probability, na.rm = TRUE),
    set_at_max_prob = set[which.max(Probability)]
  )

# find points of intersection between probability curves for different TSV
find_intersection <- function(df, col1, col2) {
  df %>%
    dplyr::mutate(diff = abs(df[[col1]] - df[[col2]])) %>%
    dplyr::filter(diff == min(diff)) %>%
    dplyr::select(set, diff)
}

# Find the intersection points
colnames(predicted_probs) <- c("Cold", "Cool", "Slightly_cool", "Neutral",
                               "Slightly_warm", "Warm", "Hot", "set")

find_intersection(predicted_probs, "Slightly_cool", "Neutral")
find_intersection(predicted_probs, "Neutral", "Slightly_warm")
find_intersection(predicted_probs, "Slightly_cool", "Slightly_warm")


# Create a visualization of predicted probabilites from the ordinal model
ggplot(long_df, aes(x = set, y = Probability, color = TSV)) +
  geom_line(size=1.5) +
  labs(x = "SET, °C",
       y = "Predicted Probability") +
  scale_color_manual(values = c("#6ea6cd", "#98cae1", "#c2e4ef", "#228833",
                                "#feda8b", "#fdb366", "#f67e4b"),
                     name = "TSV") +
  theme_classic()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(2, "cm"),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12),
        panel.grid.major = element_line(color = "grey80"),  # Add major grid lines
        panel.grid.minor = element_line(color = "grey90"))+
  scale_x_continuous(breaks = c(19, 22, 25, 28, 31)) + 
  scale_y_continuous(breaks = c(0.2, 0.4), limits = c(0, 0.45))
