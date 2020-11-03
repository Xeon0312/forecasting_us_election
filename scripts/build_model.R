library(lme4)
library(brms)
library(tidybayes)
library(caret)
library(ROCR)



# Bulid multi-level regression
model_logit1 <- glmer(vote_2020~(1+race+agegroup|cell)+gender+education+state+household_income+labforce,
                      data = survey_data, 
                      family=binomial)
