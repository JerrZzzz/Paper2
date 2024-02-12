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

### Workspace Setup###
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

study1b <- read.csv("/cloud/project/input/data/inactionwetrust_study1b.csv")
study2raw <- read.csv("/cloud/project/input/data/inactionwetrust_study2.csv", sep = ";")
studyrob <- read.csv("/cloud/project/input/data/osf-past-normality-regret-replication-exp2-data-v2.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")

######################### clean study 1 

# remove top 2 rows 
study1b <- study1b[-(1:2), ]

# select rows 
study1b <- select(study1b, preference, competence, normative, Q55, check_1, check_2, check_3, gender, age)

# rename regret
study1b <- rename(study1b, regret = Q55)

#change to numeric values
study1b$preference <- as.numeric(as.character(study1b$preference))
study1b$competence <- as.numeric(as.character(study1b$competence))
study1b$normative <- as.numeric(as.character(study1b$normative))
study1b$regret <- as.numeric(as.character(study1b$regret))
study1b$gender <- as.numeric(as.character(study1b$gender))

# Test 1 all value of preference, competence, normative and regret are value from -5 to 5 

check_values <- function(data, columns, min_val, max_val) {
  results <- list()
  for (col in columns) {
    valid_values <- all(data[[col]] >= min_val & data[[col]] <= max_val)
    results[[col]] <- valid_values
  }
  return(results)
}

# Columns to check
columns_to_check <- c("preference", "competence", "normative", "regret")

# Checking values
results <- check_values(study1b, columns_to_check, -5, 5)

# Print results
print(results)

# test 2 

check_integer_columns <- function(data, columns) {
  results <- sapply(columns, function(col) {
    all(data[[col]] == floor(data[[col]]))
  })
  return(results)
}

# Columns to check
columns_to_check <- c("preference", "competence", "normative", "regret")

# Execute the check
integer_results <- check_integer_columns(study1b, columns_to_check)

# Print results
print(integer_results)

# write it to csv
write.csv(study1b, "/cloud/project/output/data/study1.csv")

################# clean study2 

study2raw$preference <- as.numeric(as.character(study2raw$preference))
study2raw$competence <- as.numeric(as.character(study2raw$competence))
study2raw$descriptive.norms <- as.numeric(as.character(study2raw$descriptive.norms))
study2raw$Injunctive <- as.numeric(as.character(study2raw$Injunctive))
study2raw$regret <- as.numeric(as.character(study2raw$regret))
study2raw$joy <- as.numeric(as.character(study2raw$joy))

## test 1 same as above

columns_to_check2 <- c("preference", "competence", "descriptive.norms", "Injunctive", "regret", "joy")

# Function to check if all values in the specified columns are between -5 to 5
check_values_range <- function(data, columns, min_val, max_val) {
  results <- sapply(columns, function(col) {
    all(data[[col]] >= min_val & data[[col]] <= max_val, na.rm = TRUE)
  })
  return(results)
}

# Use the function to check the columns in study2raw
value_range_results <- check_values_range(study2raw, columns_to_check2, -5, 5)

# Output the results
print(value_range_results)

### test 2 

# Function to verify all values in specified columns are integers
verify_integer_values <- function(data, columns) {
  results <- sapply(columns, function(col) {
    all(data[[col]] == as.integer(data[[col]]), na.rm = TRUE)
  })
  return(results)
}

# Run the test on your dataset
integer_value_results <- verify_integer_values(study2raw, columns_to_check)

# Output the results
print(integer_value_results)

# since study 2 is clear so write it to csv then go right into plot creating

write.csv(study2raw, "/cloud/project/output/data/study2.csv")

# clean study robbery

studyrob$Sc3condition <- 0

for (i in 1:nrow(studyrob)){
  if (!is.na(data$Sc3_C1_text[i])){
    studyrob$Sc3condition[i] <- 1
  }
  else if (!is.na(studyrob$Sc3_C2_text[i])){
    studyrob$Sc3condition[i] <- 2
  }
  else if (!is.na(studyrob$Sc3_C3_text[i])){
    studyrob$Sc3condition[i] <- 3
  }
  else {
    studyrob$Sc3condition[i] <- NA
  }
}

### test 1

check_conditions <- function(df) {
  # Initialize a vector to store results
  results <- rep(TRUE, nrow(df))
  
  # Loop through each row
  for (i in 1:nrow(df)) {
    # Check the condition for the first set of columns
    if (!is.na(df$sc3_c1_compensation[i]) & !is.na(df$sc3_c1_regret[i])) {
      results[i] <- is.na(df$sc3_c2_compensation[i]) & is.na(df$sc3_c2_regret[i]) & 
        is.na(df$sc3_c3_compensation[i]) & is.na(df$sc3_c3_regret[i])
    }
    
    # Check the condition for the second set of columns
    else if (!is.na(df$sc3_c2_compensation[i]) & !is.na(df$sc3_c2_regret[i])) {
      results[i] <- is.na(df$sc3_c1_compensation[i]) & is.na(df$sc3_c1_regret[i]) & 
        is.na(df$sc3_c3_compensation[i]) & is.na(df$sc3_c3_regret[i])
    }
    
    # Check the condition for the third set of columns
    else if (!is.na(df$sc3_c3_compensation[i]) & !is.na(df$sc3_c3_regret[i])) {
      results[i] <- is.na(df$sc3_c1_compensation[i]) & is.na(df$sc3_c1_regret[i]) & 
        is.na(df$sc3_c2_compensation[i]) & is.na(df$sc3_c2_regret[i])
    }
  }
  
  return(results)
}

# Run the check function on your data frame
results <- check_conditions(studyrob)

# Print results
print(results)

# If all values are TRUE, then the dataset passes the test
all(results)

## test 2

columns_to_check <- c("sc3_c1_compensation", "sc3_c1_regret", 
                      "sc3_c2_compensation", "sc3_c2_regret", 
                      "sc3_c3_compensation", "sc3_c3_regret")

# Function to check if all values in the specified columns are integers
are_all_integers <- function(data, columns) {
  results <- sapply(columns, function(col) {
    all(data[[col]] == round(data[[col]]), na.rm = TRUE)
  })
  return(results)
}

# Run the function on the studyrob data
integer_check_results <- are_all_integers(studyrob, columns_to_check)

# Print the results
print(integer_check_results)

# write it to csv
write.csv(studyrob, "/cloud/project/output/data/robscene_study.csv")

