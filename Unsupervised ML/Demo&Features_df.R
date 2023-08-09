library(dplyr)
library(tidyr)
library(tidyverse)
library(mltools)
library(data.table)

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

# One hot encoding
not_encode <- demographics_pivot %>%
  select(PIN, Age, Height_cm, Weight_kg)

encoded_variables <- as.data.table(demographics_pivot) %>%
  select(-c(PIN, Age, Height_cm, Weight_kg )) %>%  # Exclude non-categorical columns if necessary
  mutate(across(everything(), as.factor)) %>%  # Convert all columns to factors
  one_hot()  # Apply one-hot encoding

one_hot_encoded_df <- cbind(not_encode, encoded_variables)

# Adding variables to encoded df
unsupervised_df <- merge(one_hot_encoded_df, clicks_per_participant, by = "PIN") %>%
  rename(number_clicks = event_type)

unsupervised_df <- merge(unsupervised_df, click_hold_df_PIN, by = "PIN")

x_flips <- x_flips_df_per_click_PIN %>%
  select(PIN, ratio)
unsupervised_df <- merge(unsupervised_df, x_flips, by = "PIN") %>%
  rename(x_flips = ratio)

unsupervised_df <- merge(unsupervised_df, mean_response_time_PIN, by = "PIN")

unsupervised_df <- merge(unsupervised_df, length_per_click_PIN, by = "PIN") %>%
  rename(mean_length_click = ratio)

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

unsupervised_df <- merge(unsupervised_df, max_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_max_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, mean_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, avg_mean_deviation_PIN , by = "PIN")
unsupervised_df <- merge(unsupervised_df, median_deviation_PIN , by = "PIN")

  

