library(dplyr)
library(ggplot2)
library(tidyr)

GAD7_File <- read.csv("GAD-7.csv") # Read the csv file and store 

GAD7_sums <- GAD7_File %>% 
  select(PIN, item, response) %>%
  group_by(PIN) %>%
  summarise(sum = sum(as.numeric(response[item %in% c(1:7)], na.rm = TRUE))) %>%
  mutate(Classification = ifelse(sum >= 10, 1, 0)) %>%
  mutate(Illness_status = ifelse(Classification == 1, "GAD", "Subclinical"))

print(GAD7_sums)

GAD7_dist <- GAD7_sums %>%
  group_by(Illness_status) %>%
  summarize(Count = n())

print(GAD7_dist)

GAD7_barplot <- ggplot(GAD7_dist, aes(x = Illness_status, y = Count, fill = Illness_status)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "GAD7 Distribution", 
       x = "Illness Status", 
       y = "Number") +
  theme_minimal()

print(GAD7_barplot)
