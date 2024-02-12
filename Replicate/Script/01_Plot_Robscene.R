#### Preamble ####
# Purpose: Replication on paper written by Feldman.G etc.
# Author: Zhijun Zhong & Xincheng Zhang
# Date: 8 February 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca / xinchenggg.zhang@mail.utoronto.ca

## Data resource

### Kutscher, L., & Feldman, G. (2023). Impact of past behavior normality on 
### regret: Replication and extension of three experiments of the exceptionality 
### effect. OSF. https://osf.io/fnmk4/

### Data can be accessed by the file in the link above at path 
### ".../Data and code/osf-past-normality-regret-replication-exp2-data-v2.csv"

### Work Setup ### 
library(knitr)
library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(ggstatsplot)

### read data
study <- read.csv("/cloud/project/output/data/robscene_study.csv")

### Filter data
study_filtered <- select(rob, Q_TotalDuration, sc3_c2_compensation, 
                         sc3_c2_regret, sc3_c3_compensation, sc3_c3_regret, 
                         age, Sc3condition)

###calculate frequency
freq_sc3_c2 <- study_filtered %>%
  count(sc3_c2_compensation) %>%
  rename(Compensation_Amount = sc3_c2_compensation, Frequency = n)

freq_sc3_c3 <- study_filtered %>%
  count(sc3_c3_compensation) %>%
  rename(Compensation_Amount = sc3_c3_compensation, Frequency = n)

# Combine the two frequency tables and add a new column to differentiate them
freq_combined <- bind_rows(
  mutate(freq_sc3_c2, Compensation_Type = 'personal compensation'),
  mutate(freq_sc3_c3, Compensation_Type = 'external compensation')
)

# Calculate means for each compensation type to add vertical lines
mean_sc3_c2 <- mean(study_filtered$sc3_c2_compensation, na.rm = TRUE)
mean_sc3_c3 <- mean(study_filtered$sc3_c3_compensation, na.rm = TRUE)

freq_combined <- freq_combined %>%
  arrange(Compensation_Type, Compensation_Amount)

# Now plot the scatter plot with the lines connecting the points
ggplot_object <- ggplot(freq_combined, aes(x = Compensation_Amount, y = Frequency, color = Compensation_Type, group = Compensation_Type)) +
  geom_point(size = 2) +
  geom_line() +  # This adds the lines connecting the points
  geom_vline(xintercept = mean_sc3_c2, color = "deepskyblue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_sc3_c3, color = "hotpink", linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_color_manual(values = c('personal compensation' = 'deepskyblue', 'external compensation' = 'hotpink')) +
  labs(title = "Frequency of Compensation Amounts",
       x = "Compensation Amount",
       y = "Frequency",
       color = "Compensation Type") +
  theme_minimal()

ggplot_object

# Histogram for sc3_c2_compensation and sc3_c2_regret

freq_sc3_c2_r <- study_filtered %>%
  count(sc3_c2_regret) %>%
  rename(regret_Amount = sc3_c2_regret, Frequency = n)

# Tabulate the frequencies of each compensation value for sc3_c3
freq_sc3_c3_r <- study_filtered %>%
  count(sc3_c3_regret) %>%
  rename(regret_Amount = sc3_c3_regret, Frequency = n)

# Combine the two frequency tables and add a new column to differentiate them
freq_combined_r <- bind_rows(
  mutate(freq_sc3_c2_r, regret_Type = 'Personal regret'),
  mutate(freq_sc3_c3_r, regret_Type = 'External regret')
)

# Calculate means for each compensation type to add vertical lines
mean_sc3_c2_regret <- mean(study_filtered$sc3_c2_regret, na.rm = TRUE)
mean_sc3_c3_regret <- mean(study_filtered$sc3_c3_regret, na.rm = TRUE)

ggplot_object_r <- ggplot(freq_combined_r, aes(x = regret_Amount, y = Frequency, color = regret_Type, group = regret_Type)) +
  geom_point(size = 2) +
  geom_line() +  
  geom_vline(xintercept = mean_sc3_c2_regret, color = "darkorange", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_sc3_c3_regret, color = "violetred", linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, 90)) +
  scale_color_manual(values = c('Personal regret' = 'darkorange', 'External regret' = 'violetred')) +
  labs(title = "Frequency of Regret Amounts",
       x = "Regret Amount",
       y = "Frequency",
       color = "Regret Type") +
  theme_minimal()

ggsave("/cloud/project/Replicate/Graphs/combined_rob_compensation.png", plot = ggplot_object, width = 10, height = 8, units = "in")

ggsave("/cloud/project/Replicate/Graphs/combined_rob_regret.png", plot = ggplot_object_r, width = 10, height = 8, units = "in")

