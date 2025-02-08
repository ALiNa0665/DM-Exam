# Load necessary libraries
install.packages("tidyr")
library(tidyr)
library(dplyr)

setwd("/work/PE")

# Read the CSV files
choices <- read.csv("choices.csv", sep = ";")
losses <- read.csv("losses.csv", sep = ";")
net_outcome <- read.csv("net_outcome.csv", sep = ";")
wins <- read.csv("wins.csv", sep = ";")

# Extract the Subject IDs
subject_ids <- choices$Subj_ID

# Reshape the data to long format for each file
choices_long <- choices %>% 
  pivot_longer(cols = starts_with("Choice_"), 
               names_to = "Trial", 
               values_to = "Choice") %>% 
  mutate(Trial = as.numeric(gsub("Choice_", "", Trial)))

losses_long <- losses %>% 
  pivot_longer(cols = starts_with("Losses_"), 
               names_to = "Trial", 
               values_to = "Losses") %>% 
  mutate(Trial = as.numeric(gsub("Losses_", "", Trial)))

wins_long <- wins %>% 
  pivot_longer(cols = starts_with("Wins_"), 
               names_to = "Trial", 
               values_to = "Wins") %>% 
  mutate(Trial = as.numeric(gsub("Wins_", "", Trial)))

net_outcome_long <- net_outcome %>% 
  pivot_longer(cols = starts_with("Outcome_"), 
               names_to = "Trial", 
               values_to = "Net_Outcome") %>% 
  mutate(Trial = as.numeric(gsub("Outcome_", "", Trial)))

# Merge all long format data by Subject ID and Trial
parameter_estimation <- choices_long %>% 
  select(Subj_ID, Trial, Choice) %>% 
  left_join(losses_long %>% select(Subj_ID, Trial, Losses), by = c("Subj_ID", "Trial")) %>% 
  left_join(wins_long %>% select(Subj_ID, Trial, Wins), by = c("Subj_ID", "Trial")) %>% 
  left_join(net_outcome_long %>% select(Subj_ID, Trial, Net_Outcome), by = c("Subj_ID", "Trial"))

# Save the combined data to a new CSV file
write.csv2(parameter_estimation, "parameter_estimation.csv", row.names = FALSE)

# Print the head of the combined data to verify
print(head(parameter_estimation))
