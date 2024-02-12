#### Preamble ####
# Purpose: Replication on paper written by Feldman.G etc.
# Author: Zhijun Zhong & Xincheng Zhang
# Date: 8 February 2024
# Contact: Jerryzz.zhong@mail.utoronto.ca / xinchenggg.zhang@mail.utoronto.ca

## Data resource

### Fillon, A., Strauch, L., & Feldman, G. (2023). Evaluations of action and 
### inaction decision-makers in risky decisions resulting in negative outcomes: 
### Inaction agents are preferred to and perceived as more competent and normative 
### than action agents [Data set]. OSF. https://osf.io/a8e4d/ 

### Data can be accessed by the file in the link above at path ".../Data and code/inactionwetruststudy_1.csv"

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

### Plotting Graphs for study 1 ### 

study1 <- read.csv("/cloud/project/output/data/study1.csv")

### cleaning data which get read of na values
study1_clean <- study1 %>%
  filter(!is.na(preference), !is.na(competence), !is.na(normative), !is.na(regret))

### calculate means for each columns
means <- study1_clean %>%
  summarise(preference_mean = mean(preference, na.rm = TRUE),
            competence_mean = mean(competence, na.rm = TRUE),
            norms_mean = mean(normative, na.rm = TRUE),
            regret_mean = mean(regret, na.rm = TRUE))

### write a function to calculate proportion
calculate_proportion <- function(data, column) {
  prop <- mean(data[[column]], na.rm = TRUE) / length(na.omit(data[[column]]))
  return(prop)
}

# Creating the plots with vertical lines and proportions
p1 <- ggplot(study1_clean, aes(x = preference)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$preference_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$preference_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Preference")+
  labs(y = "Count")+ 
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions
p2 <- ggplot(study1_clean, aes(x = competence)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$competence_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$competence_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Competence")+ 
  labs(y = "Count")+ 
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions
p3 <- ggplot(study1_clean, aes(x = normative)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$norms_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$norms_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Norms")+
  labs(y = "Count")+
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions
p4 <- ggplot(study1_clean, aes(x = regret)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$regret_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$regret_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Regret")+
  labs(y = "Count")+scale_x_continuous(breaks = -5:5)

# Combine the plots into one figure
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


# Save the combined plot to a file
ggsave("/cloud/project/Replicate/Graphs/combined_histograms.png", plot = combined_plot, width = 10, height = 8, units = "in")

