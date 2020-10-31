#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data. 
raw_data <- read_dta("inputs/data/usa_00001.dta"
                     )
# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
names(raw_data)

reduced_data_p <- 
  raw_data %>% 
  select(age,
         sex, 
         educd,
         stateicp,
         inctot,
         race,
         perwt,
         citizen,
         empstat,
         labforce)

# Cleaning data
# Remove people who can't vote
reduced_data_p$age<-as.numeric(reduced_data_p$age)
reduced_data_p<-reduced_data_p %>% filter(age>=18 & 
                                          (citizen=="naturalized citizen"|citizen=="born abroad of american parents")
                                          )
# Adjust the NA
reduced_data_p$inctot<-ifelse(reduced_data_p$inctot==9999999,
                                      NaN,reduced_data_p$inctot)
# Drop NA
reduced_data_p<-na.omit(reduced_data_p)

# Clean memory
rm(raw_data)

#### What's next? ####

# Making some age-groups
reduced_data_p<-reduced_data_p %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 30 ~ '21 to 30',
                              age >30  & age <= 40 ~ '31 to 40',
                              age >40  & age <= 50 ~ '41 to 50',
                              age >50  & age <= 60 ~ '51 to 60',
                              age >60  & age <= 70 ~ '61 to 70',
                              age >70  & age <= 80 ~ '71 to 80',
                              age >80 ~ 'above 80'
  )) 

         