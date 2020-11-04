library(lme4)
library(brms)
library(tidybayes)
library(caret)
library(ROCR)



# import model
model_logit <- readRDS("./outputs/model/model_logit.rds")
source('./scripts/01-data_cleaning-post-strat.R')
source('./scripts/01-data_cleaning-survey.R')

# Adjust our dataset to compute
prob_trump<-predict(model_logit,type=c('response'))
vote_trump<-ifelse(prob_trump>=0.5,"Donald Trump","Joe Biden")
survey_data_with_pred<-cbind(survey_data,vote_trump)
survey_data_with_pred$vote_trump<- as.factor(survey_data_with_pred$vote_trump)
survey_data_with_pred$vote_2020<- as.factor(survey_data_with_pred$vote_2020)

## Use confusion matrix to see our model's accrary
confusion_matrix<-confusionMatrix(survey_data_with_pred$vote_trump,survey_data_with_pred$vote_2020)[2]
accuary<-confusionMatrix(survey_data_with_pred$vote_trump,survey_data_with_pred$vote_2020)[3]$overall['Accuracy']
confusion_matrix
accuary

# Post-Stratification

## Use our model to predict the censes data
census_data_select = c("agegroup","gender","education","state",
                       "household_income","race","cell","labforce")
vote_2020_prob<-predict(model_logit,census_data[,census_data_select],type="response")
vote_2020_pred<-ifelse(vote_2020_prob>0.5,"Donald Trump","Joe Biden")
census_data_with_pred<-cbind(census_data,vote_2020_pred)

## Add weight to each vote
census_data_with_pred$trump_votes<-ifelse(census_data_with_pred$vote_2020_pred=="Donald Trump",census_data_with_pred$perwt,0)
census_data_with_pred$biden_votes<-ifelse(census_data_with_pred$vote_2020_pred=="Joe Biden",census_data_with_pred$perwt,0)

## Choose state from the votes and catgoary them
census_data_with_pred %>% group_by(state) %>% summarise(Trump=sum(trump_votes),Biden=sum(biden_votes))->predicted_states
predicted_states$winner<-ifelse(predicted_states$Trump>predicted_states$Biden,
                                "Donald Trump","Joe Biden")

## summary the vote to state vote
predicted_states<-predicted_states %>% 
  mutate(electoral_votes = case_when(state=="CA"~55,state=="TX"~38,state=="FL"~29,state=="NY"~29,state=="IL"~20,state=="PA"~20,state=="OH"~18,
                                     state=="GA"~16,state=="MI"~16,state=="NC"~15,state=="NJ"~14,state=="VA"~13,state=="WA"~12,state=="AZ"~11,
                                     state=="IN"~11,state=="MA"~11,state=="TN"~11,state=="MD"~10,state=="MN"~10,state=="MO"~10,state=="WI"~10,
                                     state=="AL"~9,state=="CO"~9,state=="SC"~9,state=="KY"~8,state=="LA"~8,state=="CT"~7,state=="OK"~7,
                                     state=="OR"~7,state=="AR"~6,state=="IA"~6,state=="KS"~6,state=="MS"~6,state=="NV"~6,state=="UT"~6,
                                     state=="NE"~5,state=="NM"~5,state=="WV"~5,state=="HI"~4,state=="ID"~4,state=="ME"~4,state=="NH"~4,
                                     state=="RI"~4,state=="AK"~3,state=="DE"~3,state=="MT"~3,state=="ND"~3,state=="SD"~3,state=="VT"~3,
                                     state=="WY"~3,state=="DC"~3
  )) 
# reference: https://www.britannica.com/topic/United-States-Electoral-College-Votes-by-State-1787124


## Get the result
predicted_states %>% group_by(winner) %>% summarise(final_votes_pred=sum(electoral_votes))->election_pred
election_pred
