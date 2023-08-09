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

# Final pivoted dataframe
demographics_pivot <- demographics_pivot %>%
  mutate(Height_cm = demographics_height_mod$Height_cm,
         Weight_kg = demographics_weight_mod$Weight_kg)

# Adding variables to the dataframe
unsupervised_df <- merge(demographics_pivot, clicks_per_participant, by = "PIN") %>%
  rename(number_clicks = event_type)

unsupervised_df <- merge(unsupervised_df, click_hold_df_PIN, by = "PIN")

x_flips <- x_flips_df_per_click_PIN %>%
  select(PIN, ratio)
unsupervised_df <- merge(unsupervised_df, x_flips, by = "PIN") %>%
  rename(x_flips = ratio)

unsupervised_df <- merge(unsupervised_df, mean_response_time_PIN, by = "PIN")

length_per_click <- length_per_click_PIN %>%
  select(PIN, ratio) %>%
  rename(mean_length_click = ratio)
unsupervised_df <- merge(unsupervised_df, length_per_click, by = "PIN")

path_ideal_ratio <- path_lengths_ratio_df %>%
  select(PIN, distance_ratio)
unsupervised_df <- merge(unsupervised_df, path_ideal_ratio , by = "PIN") %>%
  rename(path_ideal_ratio = distance_ratio)

mean_vel <- time_distance_combo %>%
  select(PIN, avg_velocity_PIN)
unsupervised_df <- merge(unsupervised_df, mean_vel , by = "PIN") %>%
  rename(mean_velocity = avg_velocity_PIN)

mean_angle <- mean_angle_df_per_click_PIN %>%
  select(PIN, ratio)
unsupervised_df <- merge(unsupervised_df, mean_angle , by = "PIN") %>%
  rename(mean_angle = ratio)

acute_angle <- acute_angles_df_per_click_PIN %>%
  select(PIN, ratio)
unsupervised_df <- merge(unsupervised_df, acute_angle , by = "PIN") %>%
  rename(acute_angles = ratio)

unsupervised_df <- merge(unsupervised_df, max_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_max_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, avg_mean_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, median_deviation_PIN , by = "PIN")

unsupervised_df <- merge(unsupervised_df, mean_velocity_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, max_velocity_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_max_velocity_PIN , by = "PIN")

local_max <- local_max_PIN %>%
  select(PIN, local_max_per_click)
unsupervised_df <- merge(unsupervised_df, local_max , by = "PIN")

local_min <- local_min_PIN %>%
  select(PIN, local_min_per_click)
unsupervised_df <- merge(unsupervised_df, local_min , by = "PIN")

unsupervised_df <- merge(unsupervised_df, max_acceleration_overall_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_max_acceleration , by = "PIN")
unsupervised_df <- merge(unsupervised_df, min_acceleration_overall_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_min_acceleration , by = "PIN")
unsupervised_df <- merge(unsupervised_df, median_acceleration_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, q1_acceleration_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, q3_acceleration_PIN , by = "PIN")

vel_bin1 <- velocity_bin_1 %>%
  group_by(PIN) %>%
  mutate(distance = sum(sm_distance), time = sum(sm_time), velocity_bin_1 = distance/time) %>%
  select(PIN, velocity_bin_1) %>%
  filter(!duplicated(PIN))
unsupervised_df <- merge(unsupervised_df, vel_bin1 , by = "PIN")

unsupervised_df <- merge(unsupervised_df, median_acceleration_bin_1and2_PIN , by = "PIN")

# Extracting only numerical features
unsupervised_numerical_df <- unsupervised_df %>%
  select_if(is.numeric)

# Correlation matrix
unsupervised_numerical_cor_pearson <- cor(unsupervised_numerical_df)
unsupervised_numerical_cor_spearman <- cor(unsupervised_numerical_df, method = "spearman")

corrplot(unsupervised_numerical_cor_pearson, method = 'number')
corrplot(unsupervised_numerical_cor_spearman, method = 'number')

indices_pearson <- which((unsupervised_numerical_cor_pearson > 0.8 | unsupervised_numerical_cor_pearson < -0.8), arr.ind = TRUE)
indices_pearson <- as.data.frame(indices_pearson)
filtered_pearson <- indices_pearson %>%
  filter(row != col)

indices_spearman <- which((unsupervised_numerical_cor_spearman > 0.8 | unsupervised_numerical_cor_spearman < -0.8), arr.ind = TRUE)
indices_spearman <- as.data.frame(indices_spearman)
filtered_spearman <- indices_spearman %>%
  filter(row != col)


