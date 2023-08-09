library(dplyr)
library(ggplot2)
library(tidyr)

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

# 1) Gender demographics
gender_counts <- demographics_file %>%
  filter(item == "1. Gender") %>%
  select(item, response) %>%
  group_by(response) %>%
  summarize(Count = n()) %>%
  rename(Gender_response = response)

print(gender_counts)

gender_barplot <- ggplot(gender_counts, aes(x = Gender_response, y = Count, fill = Gender_response)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Gender Response Distribution", 
       x = "Gender", 
       y = "Number") +
  theme_minimal()

print(gender_barplot)

# 2) Age demographics
age_demographics <- demographics_file %>%
  filter(item == "2. Age (years)") %>%
  mutate(Age_numeric = as.numeric(response)) %>%
  select(item, Age_numeric) 

age_mean_sd <- age_demographics %>%
  group_by(item) %>%
  summarise(Age_mean = mean(Age_numeric), Age_sd = sd(Age_numeric))

print(age_mean_sd)

age_boxplot <- ggplot(age_demographics, aes(x = item, y = Age_numeric)) +
  geom_boxplot() +
  ylab("Age") +
  ggtitle("Boxplot of All Ages") +
  theme(axis.text.x = element_blank()) + 
  stat_summary(fun=mean, geom="point", shape=4, size=4)

print(age_boxplot)

# 3) Height demographics
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

print(demographics_height_mod)

Height_mean_sd <- demographics_height_mod %>%
  summarise(Height_mean = mean(Height_cm), Height_sd = sd(Height_cm))

print(Height_mean_sd)

height_boxplot <- ggplot(demographics_height_mod, aes(x = "", y = Height_cm)) +
  geom_boxplot() +
  labs(title = "Boxplot of Heights", y = "Height") +
  theme(axis.text.x = element_blank())  # Remove x-axis labels

print(height_boxplot)

# 4) Weight demographics
demographics_weight_mod <- demographics_file %>%
  filter(item == "4a. Weight (kg)" | item == "4b. Weight (lbs)") %>%
  select(item, response) %>%
  mutate(response = as.numeric(ifelse(is.na(response), 0, response))) %>%
  filter(response != 0) %>%
  mutate(response = ifelse(item == "4b. Weight (lbs)", response * 0.453592, response),
         item = ifelse(item == "4b. Weight (lbs)", "4a. Weight (kg)", item))

print(demographics_weight_mod)

weight_mean_sd <- demographics_weight_mod %>%
  group_by(item) %>%
  summarise(Weight_mean = mean(response), Weight_sd = sd(response))

print(weight_mean_sd)

weight_boxplot <- ggplot(demographics_weight_mod, aes(x = item, y = response)) +
  geom_boxplot() +
  ylab("Weight") +
  ggtitle("Boxplot of All Weights") +
  theme(axis.text.x = element_blank()) + 
  stat_summary(fun=mean, geom="point", shape=4, size=4)

print(weight_boxplot)

# 5) Education demographics
education_level_counts <- demographics_file %>%
  filter(item == "5. Education level") %>%
  select(item, response) %>%
  group_by(response) %>%
  summarize(Count = n()) %>%
  rename(Education_Level = response)

print(education_level_counts)

education_level_barplot <- ggplot(education_level_counts, aes(x = Education_Level, y = Count, fill = Education_Level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Education Level Distribution", 
       x = "Education Level", 
       y = "Number") +
  theme_minimal()

print(education_level_barplot)

# 6) Marital Status demographics
marital_status_counts <- demographics_file %>%
  filter(item == "6. Marital status") %>%
  select(item, response) %>%
  group_by(response) %>%
  summarize(Count = n()) %>%
  rename(Marital_Status = response)

print(marital_status_counts)

marital_status_barplot <- ggplot(marital_status_counts, aes(x = Marital_Status, y = Count, fill = Marital_Status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Marital Status Distribution", 
       x = "Marital Status", 
       y = "Number") +
  theme_minimal()

print(marital_status_barplot)

# 7) Employment demographics
employment_status_count <- demographics_file %>%
  filter(item == "7. Employment status") %>%
  select(item, response) %>%
  group_by(response) %>%
  summarize(Count = n()) %>%
  rename(Employment_status = response)

print(employment_status_count)

employment_status_barplot <- ggplot(employment_status_count, aes(x = Employment_status, y = Count, fill = Employment_status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Employment Status Distribution", 
       x = "Employment Status", 
       y = "Number") +
  theme_minimal()

print(employment_status_barplot)

# 8) Yearly income demographics
yearly_income_counts <- demographics_file %>%
  filter(item == "8. Gross annual household income (US dollars)") %>%
  select(item, response) %>%
  group_by(response) %>%
  summarize(Count = n()) %>%
  rename(Income_level = response)

print(yearly_income_counts)

yearly_income_barplot <- ggplot(yearly_income_counts, aes(x = Income_level, y = Count, fill = Income_level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Yearly Income Distribution", 
       x = "Yearly Income", 
       y = "Number") +
  theme_minimal()

print(yearly_income_barplot)