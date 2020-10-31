#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_s <- read_dta("inputs/data/ns20200625/ns20200625.dta")
# Add the labels
raw_data_s <- labelled::to_factor(raw_data_s)
# Just keep some variables
reduced_data_s <- 
  raw_data_s %>% 
  select(age,
         gender,
         education,
         state,
         household_income,
         race_ethnicity,
         vote_2020,
         vote_intention,
         registration,
         trump_biden,
         employment,
         consider_trump,
         not_trump)

# Change the age type
reduced_data_s$age<-as.numeric(reduced_data_s$age)

# Remove people who didn't do the survey proply.
reduced_data_s<-reduced_data_s %>% 
  filter(registration=="Registered"&
        vote_intention=="Yes, I will vote"&
        !(consider_trump=="Yes" & not_trump =="Yes")&
        !(consider_trump=="No" & not_trump =="No")&
         (consider_trump=="Yes" & trump_biden!="Joe Biden")&
         (vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
)

# Droping NA.
reduced_data_s<-na.omit(reduced_data_s)

# Clean memory
rm(raw_data_s)

# Making some age-groups
reduced_data_s<-reduced_data_s %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 30 ~ '21 to 30',
                              age >30  & age <= 40 ~ '31 to 40',
                              age >40  & age <= 50 ~ '41 to 50',
                              age >50  & age <= 60 ~ '51 to 60',
                              age >60  & age <= 70 ~ '61 to 70',
                              age >70  & age <= 80 ~ '71 to 80',
                              age >80 ~ 'above 80'
  )) 


#### What else???? ####

# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

