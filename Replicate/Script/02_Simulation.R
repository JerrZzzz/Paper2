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

## study 1

# reproducibility

set.seed(500)

# Number of entries

n <- 339

# Simulate 'preference', 'competence', 'normative' with a uniform distribution from -5 to 5

preference <- sample(-5:5, n, replace = TRUE)
competence <- sample(-5:5, n, replace = TRUE)
normative <- sample(-5:5, n, replace = TRUE)

# Simulate 'regret' with a bias towards positive values
# One way to do this is to use a normal distribution centered on a positive value and then truncate values to the -5 to 5 range

regret <- rnorm(n, mean = 1, sd = 3) # Adjust mean and sd to your preference for skewness
regret <- pmin(pmax(regret, -5), 5) # Truncate values to be within -5 to 5

# Create the data frame

simulated_study1 <- data.frame(preference, competence, normative, regret)

#draw the graph

means <- simulated_study1 %>%
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

p1 <- ggplot(simulated_study1, aes(x = preference)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$preference_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$preference_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Preference")+
  labs(y = "Count")+ 
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions

p2 <- ggplot(simulated_study1, aes(x = competence)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$competence_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$competence_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Competence")+ 
  labs(y = "Count")+ 
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions

p3 <- ggplot(simulated_study1, aes(x = normative)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$norms_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$norms_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Norms")+
  labs(y = "Count")+
  scale_x_continuous(breaks = -5:5)

# Creating the plots with vertical lines and proportions

p4 <- ggplot(simulated_study1, aes(x = regret)) + 
  geom_histogram(binwidth = 1) + 
  geom_vline(xintercept = means$regret_mean, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Mean: %.2f", means$regret_mean), hjust = 1.1, vjust = 2) +
  ggtitle("Regret")+
  labs(y = "Count")+scale_x_continuous(breaks = -5:5)

# Combine the plots into one figure

combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

## simulation on study 2 

set.seed(300)

# Number of entries

n <- 464

# Simulate 'preference' with a uniform distribution from -5 to 5

preference <- sample(-5:5, n, replace = TRUE)

# Simulate 'regret' with a bias towards positive values
# Using a normal distribution and then truncate values to the -5 to 5 range

regret <- rnorm(n, mean = 1, sd = 3) # Adjust mean and sd to skewness preference
regret <- pmin(pmax(regret, -5), 5) # Truncate values to be within -5 to 5

# Create 'condition' column with roughly equal numbers of 1, 2, and 3

condition_values <- rep(1:3, length.out = n)

# Now shuffle that vector to randomize the order
condition <- sample(condition_values)

# Create the data frame

simulated_study2 <- data.frame(preference, regret, condition)

### draw the graph

normal <- simulated_study2 %>%
  filter(condition == "1")

### calculate mean values

means_normal <- normal %>%
  summarise(preference_mean_normal = mean(preference, na.rm = TRUE),
            regret_mean_normal = mean(regret, na.rm = TRUE), 
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

Postive <- simulated_study2 %>%
  filter(condition == "2")

### calculate mean values

means_postive <- Postive %>%
  summarise(preference_mean_postive = mean(preference, na.rm = TRUE),
            regret_mean_postive = mean(regret, na.rm = TRUE), 
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

negative <- simulated_study2 %>%
  filter(condition == "3")

## calculate mean values

means_negative <- negative %>%
  summarise(preference_mean_negative = mean(preference, na.rm = TRUE),
            regret_mean_negative = mean(regret, na.rm = TRUE), 
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

## Robbery Scene

set.seed(400)

# Number of entries
n <- 344

# Create a normal distribution for 'compensation' and truncate values to be within 1 to 11
compensation <- round(rnorm(n, mean = 6, sd = 2))
compensation <- pmin(pmax(compensation, 1), 11)

# Simulate 'regret' with a bias towards the maximum value of 4
regret <- sample(2:4, n, replace = TRUE, prob = c(0.1, 0.2, 0.7))

# Assign conditions such that an examiner can only have values for c2 or c3 but not both
condition <- sample(c("2", "3"), n, replace = TRUE)

# Initialize columns with NAs
sc3_c2_compensation <- rep(NA, n)
sc3_c3_compensation <- rep(NA, n)
sc3_c2_regret <- rep(NA, n)
sc3_c3_regret <- rep(NA, n)

# Assign values to columns based on the condition
for (i in 1:n) {
  if (condition[i] == 2) {
    sc3_c2_compensation[i] <- compensation[i]
    sc3_c2_regret[i] <- regret[i]
  } else {
    sc3_c3_compensation[i] <- compensation[i]
    sc3_c3_regret[i] <- regret[i]
  }
}

# Create the data frame
simulated_rob <- data.frame(sc3_c2_compensation, sc3_c3_compensation,
                                sc3_c2_regret, sc3_c3_regret, condition)

simulated_rob$condition <- NULL

simulated_rob$condition <- ifelse(!is.na(simulated_rob$sc3_c2_regret), 2, 3)


## draw the graph

freq_sc3_c2 <- simulated_rob %>%
  count(sc3_c2_compensation) %>%
  rename(Compensation_Amount = sc3_c2_compensation, Frequency = n)

freq_sc3_c3 <- simulated_rob %>%
  count(sc3_c3_compensation) %>%
  rename(Compensation_Amount = sc3_c3_compensation, Frequency = n)

# Combine the two frequency tables and add a new column to differentiate them
freq_combined <- bind_rows(
  mutate(freq_sc3_c2, Compensation_Type = 'personal compensation'),
  mutate(freq_sc3_c3, Compensation_Type = 'external compensation')
)

# Calculate means for each compensation type to add vertical lines
mean_sc3_c2 <- mean(simulated_rob$sc3_c2_compensation, na.rm = TRUE)
mean_sc3_c3 <- mean(simulated_rob$sc3_c3_compensation, na.rm = TRUE)

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

freq_sc3_c2_r <- simulated_rob %>%
  count(sc3_c2_regret) %>%
  rename(regret_Amount = sc3_c2_regret, Frequency = n)

# Tabulate the frequencies of each compensation value for sc3_c3
freq_sc3_c3_r <- simulated_rob %>%
  count(sc3_c3_regret) %>%
  rename(regret_Amount = sc3_c3_regret, Frequency = n)

# Combine the two frequency tables and add a new column to differentiate them
freq_combined_r <- bind_rows(
  mutate(freq_sc3_c2_r, regret_Type = 'Personal regret'),
  mutate(freq_sc3_c3_r, regret_Type = 'External regret')
)

# Calculate means for each compensation type to add vertical lines
mean_sc3_c2_regret <- mean(simulated_rob$sc3_c2_regret, na.rm = TRUE)
mean_sc3_c3_regret <- mean(simulated_rob$sc3_c3_regret, na.rm = TRUE)

ggplot_object_r <- ggplot(freq_combined_r, aes(x = regret_Amount, y = Frequency, color = regret_Type, group = regret_Type)) +
  geom_point(size = 2) +
  geom_line() +  
  geom_vline(xintercept = mean_sc3_c2_regret, color = "darkorange", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean_sc3_c3_regret, color = "violetred", linetype = "dashed", size = 1) +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_color_manual(values = c('Personal regret' = 'darkorange', 'External regret' = 'violetred')) +
  labs(title = "Frequency of Regret Amounts",
       x = "Regret Amount",
       y = "Frequency",
       color = "Regret Type") +
  theme_minimal()

ggplot_object_r

