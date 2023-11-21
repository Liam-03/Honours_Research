library(dplyr)
library(tidyr)
library(tidyverse)
library(mltools)
library(data.table)
library(corrplot)

# Cleaning demographics 
demographics_file_original <- read.csv("demographics.csv") # Read the csv file and store 

demographics_file <- demographics_file_original %>%
  mutate(item = ifelse(grepl("Gender|Gênero", item), "1. Gender", item),
         item = ifelse(grepl("Age", item), "2. Age (years)", item),
         item = ifelse(grepl("Weight\\(kg\\)", item), "4a. Weight (kg)", item),
         item = ifelse(grepl("Weight \\(lbs\\)", item), "4b. Weight (lbs)", item),
         item = ifelse(grepl("Education|Nível de educação", item), "5. Education level", item),
         item = ifelse(grepl("Marital status|Estado civil", item), "6. Marital status", item),
         item = ifelse(grepl("Employment status|Status de emprego", item), "7. Employment status", item),
         item = ifelse(grepl("Gross annual household income \\(US dollars\\)|Renda familiar anual bruta \\(dólares americanos\\)", item), "8. Gross annual household income (US dollars)", item))

demographics_height_mod <- demographics_file %>%
  filter(item == "3a. Height (cm)" | item == "3b. Height (feet)" | item == "3c. Height (inches)") %>%
  select(PIN, item, response) %>%
  mutate(response = as.numeric(ifelse(is.na(response), 0, response))) %>%
  filter(response != 0, ) %>%
  mutate(response = case_when(
    grepl("feet", item, ignore.case = TRUE) ~ response * 30.48,
    grepl("inches", item, ignore.case = TRUE) ~ response * 2.54,
    TRUE ~ response
  )) %>%
  group_by(PIN) %>%
  summarise(Height_cm = sum(response, na.rm = TRUE))

demographics_weight_mod <- demographics_file %>%
  filter(item == "4a. Weight (kg)" | item == "4b. Weight (lbs)") %>%
  select(item, response) %>%
  mutate(response = as.numeric(ifelse(is.na(response), 0, response))) %>%
  filter(response != 0) %>%
  mutate(Weight_kg = ifelse(item == "4b. Weight (lbs)", response * 0.453592, response),
         item = ifelse(item == "4b. Weight (lbs)", "4a. Weight (kg)", item))

demographics_pivot <- demographics_file %>%
  filter(complete == "y") %>%
  select(PIN, location, item, response) %>%
  pivot_wider(id_cols = c(PIN, location), names_from = item, values_from = response) %>%
  select(-c("3a. Height (cm)", "3b. Height (feet)", "3c. Height (inches)", "4a. Weight (kg)", "4b. Weight (lbs)", "English language")) %>%
  rename(Gender = "1. Gender", Age = "2. Age (years)", Education_level = "5. Education level", Marital_status = "6. Marital status",
         Employment_status = "7. Employment status", Gross_annual_household_income_USD = "8. Gross annual household income (US dollars)") %>%
  mutate(Age = as.numeric(Age))

# Add height and weight to demographics
demographics_pivot <- demographics_pivot %>%
  mutate(Height_cm = demographics_height_mod$Height_cm,
         Weight_kg = demographics_weight_mod$Weight_kg)

# Adding variables to the dataframe
final_dataset_all_features <- merge(demographics_pivot, clicks_per_participant, by = "PIN") %>%
  rename(number_clicks = event_type)

final_dataset_all_features <- merge(final_dataset_all_features, mean_response_time_PIN, by = "PIN")
final_dataset_all_features <- merge(final_dataset_all_features, median_response_time_PIN, by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, mean_median_length_df, by = "PIN")
final_dataset_all_features <- merge(final_dataset_all_features, actual_idealised_merged_df, by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, acute_angles_mean_median, by = "PIN")
final_dataset_all_features <- merge(final_dataset_all_features, angles_mean_median , by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, x_flips_df_per_click_PIN , by = "PIN")
final_dataset_all_features <- merge(final_dataset_all_features, x_flips_median , by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, mean_max_median_deviation , by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, mean_median_velocity_2, by = "PIN")
final_dataset_all_features <- merge(final_dataset_all_features, max_locals_velocity, by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, acceleration_metrics, by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, velocity_200ms, by = "PIN")
final_dataset_all_features <- merge(final_dataset_all_features, acceleration_200ms, by = "PIN")

final_dataset_all_features <- merge(final_dataset_all_features, click_hold_df_PIN, by = "PIN") %>%
  rename(mean_click_hold = mean_duration, median_click_hold = median_duration)

# Add target feature PHQ9
PHQ9_classification <- PHQ9_sums %>%
  select(PIN, Classification) %>%
  rename(PHQ9_status = Classification)

final_dataset_all_features_PH9 <- merge(final_dataset_all_features, PHQ9_classification, by = "PIN")

# Replace outliers
IQR_height <- IQR(final_dataset_all_features_PH9$Height_cm)
quartiles_height <- quantile(final_dataset_all_features_PH9$Height_cm, probs = c(0.25, 0.75))
Q1_height <- quartiles_height[1]
Q3_height <- quartiles_height[2]
upper_bound_height <- Q3_height + 1.5 * IQR_height
lower_bound_height <- Q1_height - 1.5 * IQR_height

final_dataset_all_features_PH9$Height_cm <- ifelse(final_dataset_all_features_PH9$Height_cm > upper_bound_height | final_dataset_all_features_PH9$Height_cm < lower_bound_height, NA, final_dataset_all_features_PH9$Height_cm)

final_dataset_all_features_PH9 <- final_dataset_all_features_PH9 %>%
  group_by(PHQ9_status) %>%
  mutate(Height_cm = ifelse(is.na(Height_cm), median(Height_cm, na.rm = TRUE), Height_cm)) %>%
  ungroup()

# Convert height/weight to BMI
final_dataset_all_features_PH9 <- final_dataset_all_features_PH9 %>%
  mutate(BMI = Weight_kg/((Height_cm/100)^2)) %>%
  select(-c(Height_cm, Weight_kg)) %>%
  relocate(BMI, .after = Age)

range(final_dataset_all_features_PH9$BMI)

# Extracting only numerical features and PIN
numerical_final_dataset_all_features_PH9 <- final_dataset_all_features_PH9 %>%
  select(where(is.numeric))

# Assess significance using Wilcoxon test for numeric features
features_MDD <- numerical_final_dataset_all_features_PH9 %>%
  filter(PHQ9_status == 1) %>%
  select(-PHQ9_status)

features_subclinical <- numerical_final_dataset_all_features_PH9 %>%
  filter(PHQ9_status == 0) %>%
  select(-PHQ9_status)

wilcox_significance_results <- list()
significant_results <- list()

for (feature in colnames(features_MDD)) {
  values_MDD <- features_MDD[[feature]]
  values_subclinical <- features_subclinical[[feature]]
  
  wilcox_result <- wilcox.test(values_MDD, values_subclinical)
  wilcox_significance_results[[feature]] <- wilcox_result
  
  if (wilcox_result$p.value < 0.05) {
    # Store the significant result in the list
    significant_results[[feature]] <- wilcox_result
  }
}

# Remove insignificant features
significant_features_list <- names(significant_results)
demographic_features_list <- names(demographics_pivot)

significant_features_demographics_PHQ9 <- final_dataset_all_features_PH9 %>%
  select(c(any_of(demographic_features_list), all_of(significant_features_list), "PHQ9_status"))

# Correlation matrix
numerical_significant_features <- significant_features_demographics_PHQ9 %>%
  select(where(is.numeric))

cor_significant_features <- cor(numerical_significant_features)

par(cex = 0.5)
corrplot(cor_significant_features, method = 'number')

indices_corr <- which((cor_significant_features > 0.75 | cor_significant_features < -0.75), arr.ind = TRUE)
indices_corr <- as.data.frame(indices_corr)
filtered_corr <- indices_corr %>%
  filter(row != col)

# Assess correlation with PHQ9_raw
PHQ9_raw <- PHQ9_sums %>%
  select(c(PIN, sum))

numerical_significant_features_PHQ9_raw <- merge(significant_features_demographics_PHQ9, PHQ9_raw, by = "PIN") %>%
  select(where(is.numeric)) %>%
  select(-PHQ9_status)

correlation_PHQ9_raw <- cor(numerical_significant_features_PHQ9_raw)[c(1:length(numerical_significant_features_PHQ9_raw)), "sum"]

correlation_df <- as.data.frame(correlation_PHQ9_raw) %>%
  mutate(abs_corr = abs(correlation_PHQ9_raw)) 

correlation_df <- correlation_df[order(-correlation_df$abs_corr),]

# Remove correlated variables
significant_uncorrelated_dataset <- significant_features_demographics_PHQ9 %>%
  select(-c(median_response_time, q3_path_ideal_ratio, median_mean_angle, global_mean, local_min_per_click, median_local_min, median_local_max))

# NOTE: mean path length and x-flips per click have cor of 0.75 so can also try removing mean path length

final_dataset_numeric <- significant_uncorrelated_dataset %>%
  select(where(is.numeric))

cor_final_dataset <- cor(final_dataset_numeric)

par(cex = 0.5)
corrplot(cor_final_dataset, method = 'number')

# Remove PIN for ML 
significant_uncorrelated_dataset_ML <- significant_uncorrelated_dataset %>%
  select(-PIN)

# Removing insignificant demographic variables
significant_uncorrelated_dataset_ML_2 <- significant_uncorrelated_dataset_ML %>%
  select(-c(Gross_annual_household_income_USD, Age, Gender))
