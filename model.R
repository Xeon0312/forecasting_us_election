### Preamble
# Purpose: Models for predicting the 2020 US election, based on
# data sets X and Y.
# TODO: Fill X and Y with the names of data sets used.
# Author: Ziyue Yang
# Date: Octorber 31, 2020
# Contact: ziyue.yang@mail.utoronto.ca
# License: MIT
# Pre-requisites: TODO (.)

# Load the packages. Uncomment the lines of install.packages if necessary
# install.packages('broom')
# install.packages('brms')
# install.packages('here')
# install.packages('tidybayes')
# install.packages('tidyverse')

library(broom)
library(brms)
library(here)
library(tidybayes)
library(tidyverse)

# Constant variables

SURVEY_MODEL_PATH = "outputs/model/brms_model"
survey_data <- read.csv("outputs/survey-cleaned.csv")
post_strat_data <- read.csv("outputs/post-strat-cleaned.csv")

# Uncomment if you want to glimpse over the data
# head(SURVEY_DATA)
# head(POST_STRAT_DATA)


# ########## Data simulation. Comment out when real data is obtained ###########
# 
# # Number of observations
# n = 200
# 
# poll_df <- tibble(
#              age_group = sample(c(20:80), size = n, replace = TRUE),
#              gender = sample(c("Male", "Female"), size = n, replace = TRUE),
#              education = sample(c("High School", "Bachelors", "Graduate"),
#                                 size = n, replace = TRUE),
#              state = sample(c(1:50), size = n, replace = TRUE),
#              household_income = sample(c("None", "1 to 50,000",
#                                          "50,000 to 100,000",
#                                          "100,000 to 200,000",
#                                          "Above 200,000"), size = n,
#                                        replace = TRUE),
#              vote_intention = sample(c("Democratics", "Republicans"), size = n,
#                                      replace = TRUE),
#              vote_2020 = sample(c("Trump", "Biden"), size = n, replace = TRUE)
# )
# ##############################################################################


survey_model <- brms::brm(vote_2020 ~ age_group + gender + education + state + race + 
                     house_income + labforce,
                   data = survey_data,
                   family = bernoulli(),
                   file = SURVEY_MODEL_PATH
)

# Reading model
survey_model <- read_rds(paste(SURVEY_MODEL_PATH, ".rds", sep = ""))

# Summary Section
# This includes the estimates, TOFILL(.)
summary(survey_model)


### Note: post_stratified_estimates are based on variables in simulated data.
# Major change is required when working with real stratified data.
survey_estimates <- 
  survey_model %>%
  tidybayes::add_predicted_draws(newdata = survey) %>% 
  # replace DATA with the post-stratification data, which will be used for prediction
  rename(trump_predict = .prediction) %>%
  mutate(trump_predict_prop = trump_predict * cell_prop_of_division_total) %>%
  group_by(state, .draw) %>%
  summarise(trump_predict = sum(trump_predict_prop)) %>%
  group_by(state) %>%
  summarize(mean = mean(trump_predict),
            lower = quantile(trump_predict, 0.025),
            upper = quantile(trump_predict, 0.975)) # Confidence level of 95

post_stratified_estimates





















