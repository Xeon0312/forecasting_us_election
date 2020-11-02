#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from UCLA
# Author: Boyu Cao
# Data: 22 October 2020
# Contact: boyu.cao@mail.utoronto.ca
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

# Dropping NA.
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

# Unified the columns name
reduced_data_s$education[reduced_data_s$education=="Other post high school vocational training"]<-"High school graduate"

# race

other_asian_or_pacific_islander<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)","Asian (Korean)","Asian (Filipino)",
                                   "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
                                   "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")

reduced_data_s<-reduced_data_s %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% other_asian_or_pacific_islander ~"other asian or pacific islander",
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity=="Other race "~"Other race"
  )) 
reduced_data_s$race_ethnicity<-NULL

# employee
yes<-c("Full-time employed", "Part-time employed")
no<-c("Retired", "Student", "Self-employed", "Permanently disabled","Unemployed or temporarily on layoff","Homemaker","Other:" )

reduced_data_s<-reduced_data_s %>% 
  mutate(labforce = case_when(employment %in% yes ~"Yes",
                              employment %in% no ~"No",
                         )) 
reduced_data_s$employment<-NULL

reduced_data_s%>% select(vote_2020,age,agegroup,gender,education,state,household_income,race,labforce)->survey_data

# Add cells
survey_data$cell<-paste(survey_data$agegroup,survey_data$gender)

# Convert variables to factors
f.cols.survey<-c("agegroup","gender","education","state","household_income" ,"race", "cell","vote_2020","labforce")
survey_data[f.cols.survey] <- lapply(survey_data[f.cols.survey], factor) 
## To predict probability of voting for Trump (Biden as ref)
survey_data$vote_2020 <- relevel(survey_data$vote_2020, ref =  "Joe Biden") 

unique(survey_data)
rm(reduced_data_s)

# Output cleaned data as csv
write_csv(survey_data, "outputs/survey-cleaned.csv")