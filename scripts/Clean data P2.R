#### Preamble ####
# Purpose: Clean the survey data downloaded from "The Gender Gap in Undergraduate Economics Course Persistence and Degree" Selection
# Author: Yahui Zhang, Tiago Martins, Zhiqi Chen
# Date: 25 February 2022
# Contact: yahui.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: download raw data needed for replication 

#### Workspace setup ####
#Package "haven" is needed to import required files 
install.packages("haven")

library(tidyverse)
library(haven)

#### import raw data ####
#prior to importing the data sets into R studio, download the data sets to current working directory
#data for analyzing Introduction to Macroeconomics Course Persistence
read_dta("Macro_data.dta")
Macro_data <- read_dta("Macro_data.dta")

#data for analyzing Intermediate Microeconomics Course Persistence
read_dta("Int_micro_data.dta")
Int_micro_data <- read_dta("Int_micro_data.dta")

#### clean data ####
#Clean for section one
#clean raw data for analyzing Introduction to Macroeconomics Course Persistence
#For female data
Macro_data %>%
  filter(female == "1") -> Macro_data_female

#summaries data needed together
Macro_data_female %>% 
  group_by(micro_grade, macro_econ) %>% 
  summarise(micro_grade, macro_econ) -> Macro_data_female_grade

#Count number of observations for each grade group that chooses to join or not join the next level course
Macro_data_female_grade %>%
  count('micro_grade' == "0.00" & 'macro_econ' == "1") %>%
  as.data.frame -> Macro_female_count

#remove one column that we will not use
Macro_female_count <- Macro_female_count[-c(3)]

#Count total observations for each grade group
rowsum(Macro_female_count[,3], as.integer(gl(nrow(Macro_female_count), 2, nrow(Macro_female_count)))) -> Macro_Total

#Duplicate each row to match the number of rows
Macro_Total[rep(seq_len(nrow(Macro_Total)), each = 2), ] -> Macro_Total

#Merge the total observation column and all the other columns
Macro_female_count_total <- cbind(Macro_female_count, Macro_Total)

#Change column names to avoid confusing the names with the data sets
colnames(Macro_female_count_total)[3] <- "observation_number"
colnames(Macro_female_count_total)[4] <- "Total_number"

#Calculate the percentage of students joining next level course in each grade group and round the decimal to one
Macro_female_count_total %>% 
  mutate(Percentage = 100*(observation_number/Total_number)) %>%
  mutate_if(is.numeric, round, digits = 1) -> Macro_female_count_total

#clean raw data for analyzing Introduction to Macroeconomics Course Persistence
#For Male data
Macro_data %>%
  filter(female == "0") -> Macro_data_male

#summaries data needed together
Macro_data_male %>% 
  group_by(micro_grade, macro_econ) %>% 
  summarise(micro_grade, macro_econ) -> Macro_data_male_grade

#Count number of observations for each grade group that chooses to join or not join the next level course
Macro_data_male_grade %>%
  count('micro_grade' == "0.00" & 'macro_econ' == "1") %>%
  as.data.frame -> Macro_male_count

#remove one column that we will not use
Macro_male_count <- Macro_male_count[-c(3)]

#Count total observations for each grade group
rowsum(Macro_male_count[,3], as.integer(gl(nrow(Macro_male_count), 2, nrow(Macro_male_count)))) -> Macro_male_Total

#Duplicate each row to match the number of rows
Macro_male_Total[rep(seq_len(nrow(Macro_male_Total)), each = 2), ] -> Macro_male_Total

#Merge the total observation column and all the other columns
Macro_male_count_total <- cbind(Macro_male_count, Macro_male_Total)

#Change column names to avoid confusing the names with the data sets
colnames(Macro_male_count_total)[3] <- "male_observation_number"
colnames(Macro_male_count_total)[4] <- "male_Total_number"

#Calculate the percentage of students joining next level course in each grade group and round the decimal to one
Macro_male_count_total %>% 
  mutate(Percentage = 100*(male_observation_number/male_Total_number)) %>%
  mutate_if(is.numeric, round, digits = 1) -> Macro_male_count_total

#clean for section two
#clean raw data for Intermediate Microeconomics Course Persistence
#For female data
Int_micro_data %>%
  filter(female == "1") -> Int_micro_data_female

#summaries data needed together
Int_micro_data_female %>% 
  group_by(macro_grade, int_micro) %>% 
  summarise(macro_grade, int_micro) -> Int_micro_data_female_grade

#Count number of observations for each grade group that chooses to join or not join the next level course
Int_micro_data_female_grade %>%
  group_by(macro_grade, int_micro) %>%
  count('macro_grade' == "0.00" & 'int_micro' == "1") %>%
  as.data.frame -> Int_micro_female_count

#remove one column that we will not use
Int_micro_female_count <- Int_micro_female_count[-c(3)]

#Count total observations for each grade group
Int_micro_data_female_grade %>%
  group_by(macro_grade) %>%
  count('macro_grade' == "0.00" & 'int_micro' == "1") %>%
  as.data.frame -> Int_micro_Total

Int_micro_Total <- Int_micro_Total[-c(1)]

#Duplicate each row to match the number of rows
Int_micro_Total[rep(seq_len(nrow(Int_micro_Total)), each = 2), ] -> Int_micro_Total

Int_micro_Total <- Int_micro_Total[-c(2:3),]

Int_micro_Total <- Int_micro_Total[-c(1)]

#Merge the total observation column and all the other columns
Int_micro_female_count_total <- cbind(Int_micro_female_count, Int_micro_Total)

#Change column names to avoid confusing the names with the data sets
colnames(Int_micro_female_count_total)[3] <- "observation_number"
colnames(Int_micro_female_count_total)[4] <- "Total_number"

#Calculate the percentage of students joining next level course in each grade group and round the decimal to one
Int_micro_female_count_total %>% 
  mutate(Percentage = 100*(observation_number/Total_number)) %>%
  mutate_if(is.numeric, round, digits = 1) -> Int_micro_female_count_total

#clean raw data for Intermediate Microeconomics Course Persistence
#For male data
Int_micro_data %>%
  filter(female == "0") -> Int_micro_data_male

#summaries data needed together
Int_micro_data_male %>% 
  group_by(macro_grade, int_micro) %>% 
  summarise(macro_grade, int_micro) -> Int_micro_data_male_grade

#Count number of observations for each grade group that chooses to join or not join the next level course
Int_micro_data_male_grade %>%
  count('macro_grade' == "0.00" & 'int_micro' == "1") %>%
  as.data.frame -> Int_micro_male_count

#remove one column that we will not use
Int_micro_male_count <- Int_micro_male_count[-c(3)]

#Count total observations for each grade group
rowsum(Int_micro_male_count[,3], as.integer(gl(nrow(Int_micro_male_count), 2, nrow(Int_micro_male_count)))) -> Int_micro_male_Total

#Duplicate each row to match the number of rows
Int_micro_male_Total[rep(seq_len(nrow(Int_micro_male_Total)), each = 2), ] -> Int_micro_male_Total

#Merge the total observation column and all the other columns
Int_micro_male_count_total <- cbind(Int_micro_male_count, Int_micro_male_Total)

#Change column names to avoid confusing the names with the data sets
colnames(Int_micro_male_count_total)[3] <- "observation_number"
colnames(Int_micro_male_count_total)[4] <- "Total_number"

#Calculate the percentage of students joining next level course in each grade group and round the decimal to one
Int_micro_male_count_total %>% 
  mutate(Percentage = 100*(observation_number/Total_number)) %>%
  mutate_if(is.numeric, round, digits = 1) -> Int_micro_male_count_total

#### Save clean data ####
#data for analyzing previous grade
write_csv(Macro_female_count_total, "Macro_female_count_total.csv")
write_csv(Macro_male_count_total, "Macro_male_count_total.csv")
write_csv(Int_micro_female_count_total, "Int_micro_female_count_total.csv")
write_csv(Int_micro_male_count_total, "Int_micro_male_count_total.csv")

#data for analyzing relative grade 
write_csv(Macro_data_female, "Macro_data_female.csv")
write_csv(Macro_data_male, "Macro_data_male.csv")
write_csv(Int_micro_data_female, "Int_micro_data_female.csv")
write_csv(Int_micro_data_male, "Int_micro_data_male.csv")


