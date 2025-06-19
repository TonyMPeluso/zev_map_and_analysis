#####################################################################
### ZEV Registration Analysis
###
### This script reads in Canadian census data and ZEV vehicle
### registration data to build and evaluate a Lasso regression model
### explaining Zero-Emission Vehicle (ZEV) ownership levels.
### It includes data preparation, model estimation, and various
### diagnostic plots saved to a dedicated 'plots' folder.
#####################################################################


# Load necessary libraries
library(tidyverse)
library(here)
library(sf)
library(glmnet)
library(caret)
library(broom)
library(corrplot)


# Create the 'plots' directory if it doesn't exist.
# Plots will be saved here automatically.
plots_dir <- here("plots")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}
  
  # --- 2. Data Preparation ---
  
  ## 2.1. Read and Process Census Data
  # Reads census data and creates derived variables like population density and shares.
  message("Loading and processing census data...")
census_df <- as_tibble(read_csv(here("data", "census_df.csv"), col_types = cols(
  CTUID = col_character()))) %>%
  na.omit() # Remove rows with any NA values

# Check for unique CMA_UID counts (for data exploration/validation)
census_df_check_cma <- census_df %>%
  group_by(CMA_UID) %>%
  count()

# Create derived census variables
census_df <- census_df %>%
  mutate(
    pop_density = Population / land_area,
    shareSingleDwell = dwell_type_SingDet / dwell_type_Smpl,
    shareBach = hh_educ_Bach / hh_educ_TotHighestDegreeSmpl,
    shareAboveBach = hh_educ_AboveBach / hh_educ_TotHighestDegreeSmpl,
    shareTravelDist_WithinCSD = traveL_dist_WithinCSDSmpl / travel_dist_TotDestSmpl,
    shareTravelDist_WithinCD = (traveL_dist_WithinCSDSmpl + traveL_dist_DiffCSDSameCDSmpl) / travel_dist_TotDestSmpl,
    shareTravelDist_WithinProv = (traveL_dist_WithinCSDSmpl + traveL_dist_DiffCSDSameCDSmpl + traveL_dist_DiffCSDDiffCDSmpl) / travel_dist_TotDestSmpl,
    shareLDVCommute = traveL_mode_CarEtcDriverSmpl / traveL_mode_TotModeSmpl,
    sharePublicTransCommute = traveL_mode_PublicTransSmpl / traveL_mode_TotModeSmpl,
    shareTravelTime_Less15 = traveL_time_Less15Smpl / traveL_time_TotCommuteTimeSmpl,
    shareTravelTime_Less29 = (traveL_time_Less15Smpl + traveL_time_15to29Smpl) / traveL_time_TotCommuteTimeSmpl,
    shareTravelTime_Less45 = (traveL_time_Less15Smpl + traveL_time_15to29Smpl + traveL_time_30to44Smpl) / traveL_time_TotCommuteTimeSmpl,
    shareTravelTime_Less59 = (traveL_time_Less15Smpl + traveL_time_15to29Smpl + traveL_time_30to44Smpl + traveL_time_45to59Smpl) / traveL_time_TotCommuteTimeSmpl
  )

## 2.2. Read and Process ZEV Fleet Data
# Loads ZEV registration data for Ontario.
message("Loading and processing ZEV fleet data...")
fleet_df <- read_csv(here("data", "CT_ZEV_Fleet_ON.csv"), col_types = cols(
  CTUID = col_character())) %>%
  as.data.frame() %>% # Keeping as data.frame for potential compatibility with older packages
  na.omit() %>%
  filter(fuel_type == "ZEV" & year == 2021) %>%
  dplyr::select(-"fuel_type_description") # Remove redundant column

# Check for unique SAC_code counts (for data exploration/validation)
fleet_df_check_sac <- fleet_df %>%
  group_by(SAC_code) %>%
  count()

## 2.3. Join Datasets and Select Variables
# Combines census and ZEV data, then selects relevant variables for the model.
message("Joining datasets and selecting variables...")
ZEV_df <- left_join(fleet_df, census_df, by = "CTUID") %>%
  na.omit() # Remove NAs potentially introduced by the join

# Calculate ZEVs per household (target variable)
ZEV_df$ZEV_perHH <- ZEV_df$value / ZEV_df$Households

# Define variables to be used in the regression model
select_v_model <- c("ZEV_perHH", "hh_inc_MedIncHHAftTax", "dwell_fin_MedValueOfDwellingSmpl",
                    "shareBach", "shareAboveBach", "shareSingleDwell", "shareLDVCommute",
                    "sharePublicTransCommute", "shareTravelTime_Less15", "shareTravelTime_Less29",
                    "shareTravelDist_WithinCSD", "shareTravelDist_WithinCD", "pop_density")

# Create the main dataset for model training and validation
ZEV_data <- ZEV_df %>%
  dplyr::select(all_of(select_v_model))

# Prepare a specific dataset for Ottawa (SAC_code == "505"), including geometry for mapping
ZEV_data_Ott <- ZEV_df %>%
  filter(SAC_code == "505") %>%
  dplyr::select(all_of(c("CTUID", select_v_model, "geometry")))

  
  # --- 3. Model Estimation (Lasso Regression) ---
  
  ## 3.1. Data Partitioning
  # Splits the main dataset into training (80%) and validation (20%) sets.
  message("Splitting data into training and validation sets...")
set.seed(7) # For reproducibility
validationIndex <- createDataPartition(ZEV_data$ZEV_perHH, p = 0.80, list = FALSE)
validation_set <- as.matrix(ZEV_data[-validationIndex, ])
training_set <- as.matrix(ZEV_data[validationIndex, ])

## 3.2. Train Lasso Regression Model
# Uses cross-validation to find the optimal lambda for Lasso regression.
message("Training Lasso regression model...")
# Predictors are all columns except the first (ZEV_perHH)
# Response is the first column (ZEV_perHH)
lasso_reg <- cv.glmnet(x = training_set[, 2:ncol(training_set)],
                       y = training_set[, 1],
                       family = "gaussian", # For continuous response
                       alpha = 1, # Alpha = 1 for Lasso
                       type.measure = "mse", # Mean squared error for cross-validation
                       nfolds = 20) # Number of folds for cross-validation

# Print model summary
print(lasso_reg)

# Plot Lasso cross-validation results and save to 'plots' folder
png(here(plots_dir, "lasso_cv_plot.png"), width = 800, height = 600)
plot(lasso_reg, main = "Lasso Regression Cross-Validation Plot")
dev.off()

## 3.3. Extract Coefficients and Model Performance
# Retrieves the coefficients at optimal lambda values and checks R-squared.
message("Extracting coefficients and evaluating model performance...")
cat("\n--- Lasso Regression Coefficients ---\n")
cat("\nCoefficients at lambda.min (minimizing MSE):\n")
# Fix: Convert dgCMatrix to a regular matrix before tidying
print(tidy(as.matrix(coef(lasso_reg, s = "lambda.min"))))

cat("\nCoefficients at lambda.1se (most regularized within 1 SE of min MSE):\n")
print(tidy(as.matrix(coef(lasso_reg, s = "lambda.1se"))))

# Deviance ratio is analogous to R-squared for glmnet models
cat("\nModel Deviance Ratio (R-squared equivalent):", round(lasso_reg$glmnet.fit$dev.ratio, 4), "\n")
cat("Mean ZEV_perHH in training data:", round(mean(training_set[, 1]), 4), "\n")

## 3.4. Generate Predictions and Analyze Errors
# Makes predictions on the validation set and specifically for Ottawa.
message("Generating predictions and analyzing errors for Ottawa...")
# Predict on the hold-out validation set
predictions_validation <- predict(lasso_reg,
                                  newx = validation_set[, 2:ncol(validation_set)],
                                  s = "lambda.min")

# Predict ZEV per household for Ottawa's census tracts
# Exclude the 'geometry' column (last column) from the prediction matrix
predictors_only <- ZEV_data_Ott %>%
  dplyr::select(all_of(select_v_model[2:length(select_v_model)])) # Exclude ZEV_perHH

pred_Ott <- predict(lasso_reg,
                    newx = as.matrix(predictors_only),
                    s = "lambda.min")

# Calculate prediction error for Ottawa (Actual - Predicted)
pred_err_Ott_df <- (ZEV_data_Ott$ZEV_perHH - pred_Ott[, 1]) %>%
  as.data.frame() %>%
  setNames("ZEV_perHH_Error")

# Basic statistics of prediction error
cat("\n--- Prediction Error for Ottawa (Actual - Predicted) ---\n")
cat("Mean error:", round(mean(pred_err_Ott_df$ZEV_perHH_Error), 4), "\n")
rmse_pred_err_Ott <- sqrt(mean(pred_err_Ott_df$ZEV_perHH_Error^2))
cat("RMSE of error:", round(rmse_pred_err_Ott, 4), "\n")
summary(pred_err_Ott_df$ZEV_perHH_Error)

# For general context
cat("\nMean households per CT in Ottawa (CMA_UID 505):", round(mean(census_df$Households[census_df$CMA_UID == "505"]), 0), "\n")
cat("Total ZEV registrations in Ottawa (SAC_code 505):", sum(fleet_df$value[fleet_df$SAC_code == "505"]), "\n")

  # --- 4. Visualization of Results ---
  
  ## 4.1. Prepare Data for Ottawa-Specific Plots
  message("Preparing data for Ottawa-specific plots...")
# Combine actual, predicted, and error values for easy plotting
ottawa_results_df <- ZEV_data_Ott %>%
  mutate(
    row_num = row_number(),
    ZEV_perHH_Predicted = pred_Ott[,1],
    ZEV_perHH_Error = ZEV_perHH - ZEV_perHH_Predicted
  ) %>%
  dplyr::select(row_num, CTUID, ZEV_perHH_Actual = ZEV_perHH,
                ZEV_perHH_Predicted, ZEV_perHH_Error, geometry)

## 4.2. Plotting Prediction Error Histogram
# Visualizes the distribution of prediction errors.
plot_error_hist <- ggplot(ottawa_results_df, aes(x = ZEV_perHH_Error)) +
  geom_histogram(binwidth = 0.0005, fill = "lightblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Prediction Error for Ottawa (ZEV/Household)",
       x = "Prediction Error (Actual - Predicted ZEV/HH)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(here(plots_dir, "ottawa_prediction_error_histogram.png"), plot = plot_error_hist, width = 9, height = 6)

## 4.3. Plotting Actual vs. Predicted ZEV/HH
# Shows how actual ZEV/HH compares to model predictions across Ottawa's census tracts.
plot_actual_predicted_lines <- ggplot(ottawa_results_df, aes(x = row_num)) +
  geom_line(aes(y = ZEV_perHH_Actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = ZEV_perHH_Predicted, color = "Predicted"), linetype = "dashed", linewidth = 1) +
  labs(title = "Actual vs. Predicted ZEV per Household in Ottawa (by CTUID)",
       x = "Census Tract Unit ID (Arbitrary Order)",
       y = "ZEV per Household",
       color = "Value Type") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(here(plots_dir, "ottawa_actual_vs_predicted_lines.png"), plot = plot_actual_predicted_lines, width = 12, height = 7)

## 4.4. Plotting Actual ZEV/HH as a Bar Chart
# A horizontal bar chart of actual ZEV/HH values for each census tract in Ottawa.
plot_actual_bar_chart <- ggplot(ottawa_results_df, aes(x = reorder(CTUID, ZEV_perHH_Actual), y = ZEV_perHH_Actual)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() + # Makes bars horizontal
  labs(title = "Actual ZEV per Household in Ottawa",
       x = "Census Tract Unit ID",
       y = "ZEV per Household") +
  theme_minimal() +
  # Hide y-axis labels if there are too many CTUIDs to prevent clutter
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(here(plots_dir, "ottawa_actual_bar_chart.png"), plot = plot_actual_bar_chart, width = 10, height = 12)

# --- 5. Exploratory Variable Analysis and Plots ---

## 5.1. Rename Columns for Plotting
# Temporarily renames columns for more readable plot titles in this section.
# The original ZEV_data column names are restored at the end of this section.
original_colnames_zev_data <- colnames(ZEV_data)
readable_colnames_zev_data <- c("ZEV_perHH", "Median_Income", "Value_Dwelling", "Share_Bachelor_Edu",
                                "Share_Above_Bachelor_Edu", "Share_Single_Dwell", "Share_LDV_Commute",
                                "Share_Public_Trans_Commute", "Share_Travel_Time_Less15",
                                "Share_Travel_Time_Less29", "Share_Travel_Dist_WithinCSD",
                                "Share_Travel_Dist_WithinCD", "Pop_Density")
colnames(ZEV_data) <- readable_colnames_zev_data

# Variables to plot (excluding the target 'ZEV_perHH')
vars_for_eda_plots <- readable_colnames_zev_data[2:length(readable_colnames_zev_data)]

## 5.2. Histograms of Explanatory Variables
message("Generating histograms for explanatory variables...")
for(var_name in vars_for_eda_plots) {
  file_path <- here(plots_dir, paste0("hist_", var_name, ".png"))
  png(file_path, width = 800, height = 600)
  hist(ZEV_data[[var_name]], main = paste("Histogram of", var_name), xlab = var_name, col = "lightgreen", border = "black")
  dev.off()
}

## 5.3. Density Plots of Explanatory Variables
message("Generating density plots for explanatory variables...")
for(var_name in vars_for_eda_plots) {
  file_path <- here(plots_dir, paste0("density_", var_name, ".png"))
  png(file_path, width = 800, height = 600)
  plot(density(ZEV_data[[var_name]]), main = paste("Density Plot of", var_name), xlab = var_name, col = "darkblue", lwd = 2)
  dev.off()
}

## 5.4. Box Plots of Explanatory Variables
message("Generating box plots for explanatory variables...")
for(var_name in vars_for_eda_plots) {
  file_path <- here(plots_dir, paste0("boxplot_", var_name, ".png"))
  png(file_path, width = 600, height = 800) # Taller for vertical boxplots
  boxplot(ZEV_data[[var_name]], main = paste("Boxplot of", var_name), ylab = var_name, col = "gold", border = "brown")
  dev.off()
}

## 5.5. Correlation Plot of Explanatory Variables
message("Generating correlation plot...")
# Exclude the target variable 'ZEV_perHH' for the correlation matrix
correlations <- cor(ZEV_data %>% dplyr::select(-ZEV_perHH))
png(here(plots_dir, "correlation_plot.png"), width = 1000, height = 1000)
corrplot(correlations, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         addCoef.col = "darkgreen", # Add coefficients to the plot
         number.cex = 0.7, # Size of correlation coefficients
         col = colorRampPalette(c("blue", "white", "red"))(200)) # Custom color palette
dev.off()

# Restore original column names to ZEV_data
colnames(ZEV_data) <- original_colnames_zev_data

message("Analysis complete. All plots saved to the 'plots' folder.")