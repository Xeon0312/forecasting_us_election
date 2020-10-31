#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS
# Author: Boyu Cao
# Data: 22 October 2020
# Contact: boyu.cao@mail.utoronto.ca
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
reduced_data_p$labforce<-ifelse(reduced_data_p$labforce=="n/a",
                                NA,reduced_data_p$labforce)
# Drop NA
reduced_data_p<-na.omit(reduced_data_p)
reduced_data_p <- labelled::to_factor(reduced_data_p)
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

# Unified the columns name
## gender
reduced_data_p$sex<-ifelse(reduced_data_p$sex=="female","Female","Male")
reduced_data_p$labforce<-ifelse(reduced_data_p$labforce==2,"No","Yes")
reduced_data_p<-rename(reduced_data_p,gender=sex)

## education
grade_3toless<-c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade_4to8<-c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade_9to12<-c("grade 9","grade 10","grade 11","12th grade, no diploma")
high_school_grad<-c("ged or alternative credential","regular high school diploma")
col_not_grad<-c("some college, but less than 1 year",
                "1 or more years of college credit, no degree")
reduced_data_p<-reduced_data_p %>% 
  mutate(educd2 = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd=="doctoral degree"~'Doctorate degree',
                            educd =="master's degree" ~ 'Masters degree',
                            educd=="professional degree beyond a bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd %in% col_not_grad~"Completed some college, but no degree",
                            educd %in% high_school_grad~"High school graduate",
                            educd %in% grade_9to12~"Completed some high school",
                            educd %in% grade_4to8~"Middle School - Grades 4 - 8",
                            educd %in% grade_3toless ~"3rd Grade or less"
  )) 
### drop educd & rename educd2
reduced_data_p<-rename(reduced_data_p,education=educd2)
reduced_data_p$educd<-NULL

## Short the states name
reduced_data_p<-reduced_data_p %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC")) 
reduced_data_p$stateicp<-NULL

## House hold income
reduced_data_p<-reduced_data_p %>% 
  mutate(household_income = case_when(inctot<=14999 ~ "Less than $14,999",
                                      inctot>=15000 & inctot<=19999~"$15,000 to $19,999",
                                      inctot>=20000 & inctot<=24999~"$20,000 to $24,999",
                                      inctot>=25000 & inctot<=29999~"$25,000 to $29,999",
                                      inctot>=30000 & inctot<=34999~"$30,000 to $34,999",
                                      inctot>=35000 & inctot<=39999~"$35,000 to $39,999",
                                      inctot>=40000 & inctot<=44999~"$40,000 to $44,999",
                                      inctot>=45000 & inctot<=49999~"$45,000 to $49,999",
                                      inctot>=50000 & inctot<=54999~"$50,000 to $54,999",
                                      inctot>=55000 & inctot<=59999~"$55,000 to $59,999",
                                      inctot>=60000 & inctot<=64999~"$60,000 to $64,999",
                                      inctot>=65000 & inctot<=69999~"$65,000 to $69,999",
                                      inctot>=70000 & inctot<=74999~"$70,000 to $74,999",
                                      inctot>=75000 & inctot<=79999~"$75,000 to $79,999",
                                      inctot>=80000 & inctot<=84999~"$80,000 to $84,999",
                                      inctot>=85000 & inctot<=89999~"$85,000 to $89,999",
                                      inctot>=90000 & inctot<=94999~"$90,000 to $94,999",
                                      inctot>=95000 & inctot<=99999~"$95,000 to $99,999",
                                      inctot>=100000 & inctot<=124999~"$100,000 to $124,999",
                                      inctot>=125000 & inctot<=149999~"$125,000 to $149,999",
                                      inctot>=150000 & inctot<=174999~"$150,000 to $174,999",
                                      inctot>=175000 & inctot<=199999~"$175,000 to $199,999",
                                      inctot>=200000 & inctot<=249999~"$200,000 to $249,999",
                                      inctot>=250000~"$250,000 and above"
  )) 

reduced_data_p$inctot<-NULL

## race
reduced_data_p<-reduced_data_p %>% 
  mutate(race2 = case_when(race=="white"~"White",
                           race=="chinese"~"Chinese",
                           race=="black/african american/negro"~"Black, or African American",
                           race=="two major races"~"Other race",
                           race=="other race, nec"~"Other race",
                           race=="japanese"~"Japanese",
                           race=="american indian or alaska native"~"American Indian or Alaska Native",
                           race=="three or more major races"~"Other race",
                           race=="other asian or pacific islander"~"other asian or pacific islander"
  )) 

reduced_data_p$race<-reduced_data_p$race2
reduced_data_p$race2<-NULL

unique(reduced_data_p)

reduced_data_p%>% select(perwt,age,agegroup,gender,education,state,household_income,race,labforce)->post_data

rm(reduced_data_p)

