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

## check whether package being installed or not
check_and_install_packages <- function(package_names) {
  for (package_name in package_names) {
    if (!require(package_name, character.only = TRUE)) {
      install.packages(package_name)
      library(package_name, character.only = TRUE)
    }
  }
}

## if not, install package 
packages_needed <- c("ggplot2", "dplyr", "knitr", "janitor", "tidyverse", "lubridate", "readr", "gridExtra", "ggstatsplot") # Replace with the packages you need
check_and_install_packages(packages_needed)

### Work Setup ### 
library(knitr)
library(janitor)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)

### Plot the Data ###
study2 <- read.csv("/cloud/project/output/data/study2.csv")

# clean and get rid of all na values
study2_clean <- study2 %>%
  filter(!is.na(preference), !is.na(competence), !is.na(descriptive.norms), !is.na(Injunctive), !is.na(regret), !is.na (preference))

### Normal condition ###
normal <- study2_clean %>%
  filter(condition == "1")

### calculate mean values
means_normal <- normal %>%
  summarise(preference_mean_normal = mean(preference, na.rm = TRUE),
            competence_mean_normal = mean(competence, na.rm = TRUE),
            des_norms_mean_normal = mean(descriptive.norms, na.rm = TRUE),
            i_norms_mean_normal = mean(Injunctive, na.rm = TRUE),
            regret_mean_normal = mean(regret, na.rm = TRUE), 
            preference_mean_normal = mean(preference, na.rm = TRUE),
  )

# Creating the plots with vertical lines and proportions
no_p5 <- ggplot(normal, aes(x = preference)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means_normal$preference_mean_normal, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means_normal$preference_mean_normal), hjust = 1.1, vjust = 2) +
  ggtitle("Normal Condition Perference")+
  labs(y = "Count")+ 
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions
no_p6 <- ggplot(normal, aes(x = regret)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means_normal$regret_mean_normal, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means_normal$regret_mean_normal), hjust = 1.1, vjust = 2) +
  ggtitle("Normal Condition Regret")+
  labs(y = "Count")+ 
  scale_x_continuous(breaks = -5:5)

## Positive condition 
Postive <- study2_clean %>%
  filter(condition == "2")

### calculate mean values
means_postive <- Postive %>%
  summarise(preference_mean_postive = mean(preference, na.rm = TRUE),
            competence_mean_postive = mean(competence, na.rm = TRUE),
            des_norms_mean_postive = mean(descriptive.norms, na.rm = TRUE),
            i_norms_mean_postive = mean(Injunctive, na.rm = TRUE),
            regret_mean_postive = mean(regret, na.rm = TRUE), 
            preference_mean_postive = mean(preference, na.rm = TRUE),
  )

# Creating the plots with vertical lines and proportions
po_p5 <- ggplot(Postive, aes(x = preference)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means_postive$preference_mean_postive, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means_postive$preference_mean_postive), hjust = 1.1, vjust = 2) +
  ggtitle("Postive Condition Perference")+
  labs(y = "Count")+
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions
po_p6 <- ggplot(Postive, aes(x = regret)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means_postive$regret_mean_postive, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means_postive$regret_mean_postive), hjust = 1.1, vjust = 2) +
  ggtitle("Postive Condition Regret")+
  labs(y = "Count")+
  scale_x_continuous(breaks = -5:5)

## Negative condition 
negative <- study2_clean %>%
  filter(condition == "3")

## calculate mean values
means_negative <- negative %>%
  summarise(preference_mean_negative = mean(preference, na.rm = TRUE),
            competence_mean_negative = mean(competence, na.rm = TRUE),
            des_norms_mean_negative = mean(descriptive.norms, na.rm = TRUE),
            i_norms_mean_negative = mean(Injunctive, na.rm = TRUE),
            regret_mean_negative = mean(regret, na.rm = TRUE), 
            preference_mean_negative = mean(preference, na.rm = TRUE),
  )

# Creating the plots with vertical lines and proportions
ne_p5 <- ggplot(negative, aes(x = preference)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means_negative$preference_mean_negative, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means_negative$preference_mean_negative), hjust = 1.1, vjust = 2) +
  ggtitle("Negative Condition Preference")+
  labs(y = "Count")+
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions
ne_p6 <- ggplot(negative, aes(x = regret)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means_negative$regret_mean_negative, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means_negative$regret_mean_negative), hjust = 1.1, vjust = 2) +
  ggtitle("Negative Condition Regret")+
  labs(y = "Count")+
  scale_x_continuous(breaks = -5:5)

# Combine the plots into one figure
plot <- grid.arrange(no_p5, no_p6, po_p5, po_p6, ne_p5, ne_p6, ncol = 2, nrow = 3)

# Save the combined plot to a file
ggsave("/cloud/project/Replicate/Graphs/combined_histograms_study2.png", plot = plot, width = 10, height = 12, units = "in")




