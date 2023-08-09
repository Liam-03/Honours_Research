library(dplyr)
library(ggplot2)
library(tidyr)

PHQ9_File <- read.csv("PHQ-9.csv") # Read the csv file and store 

PHQ9_sums <- PHQ9_File %>% 
  select(PIN, item, response) %>%
  group_by(PIN) %>%
  summarise(sum = sum(as.numeric(response[item %in% c(1:9)], na.rm = TRUE))) %>%
  mutate(Classification = ifelse(sum >= 10, 1, 0)) %>%
  mutate(Illness_status = ifelse(Classification == 1, "MD", "Subclinical"))
  
print(PHQ9_sums)

PHQ9_dist <- PHQ9_sums %>%
  group_by(Illness_status) %>%
  summarize(Count = n())

print(PHQ9_dist)

PHQ9_barplot <- ggplot(PHQ9_dist, aes(x = Illness_status, y = Count, fill = Illness_status)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "PHQ9 Distribution", 
       x = "Illness Status", 
       y = "Number") +
  theme_minimal()

print(PHQ9_barplot)
