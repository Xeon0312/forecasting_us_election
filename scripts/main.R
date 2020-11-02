####*****Multi-level regression*****####
library(lme4)
library(brms)
library(tidybayes)
library(caret)
library(ROCR)

####Model 1####
model_logit1 <- glmer(vote_2020~(1+race+agegroup|cell)+gender+education+state+household_income+labforce,
                      data = survey_data, 
                      family=binomial)

summary(model_logit1)

prob.1<-predict(model_logit1,type=c('response'))
result_model1<-ifelse(prob.1>=0.5,"Joe Biden","Donald Trump")
survey_data.result<-cbind(survey_data,result_model1)
#Logistic: Confusion Matrix (optional)
cm.1<-confusionMatrix(survey_data.result$result_model1,survey_data.result$vote_2020)[2]
accu.1<-confusionMatrix(survey_data.result$result_model1,survey_data.result$vote_2020)[3]$overall['Accuracy']
cm.1
accu.1
#Logistic: ROC Curve (optional)
roc.1 <- roc(survey_data.result$vote_2020, prob.1)
auc(roc.1)
plot(roc.1, auc.polygon=TRUE, print.auc = TRUE,asp = NA)


####Model 2-Too slow, don’t run it, just to give you an idea what can be changed in building model#### 
model_logit2 <- glmer(vote_2020~(1+race+state|cell)+agegroup+gender+education+household_income,
                      data = survey_data, 
                      family=binomial)

summary(model_logit2)


prob.2<-predict(model_logit2,type=c('response'))
result_model2<-ifelse(prob.2>=0.5,"Joe Biden","Donald Trump")
survey_data.result<-cbind(survey_data.result,result_model2)

#Logistic: Confusion Matrix (optional)
cm.2<-confusionMatrix(survey_data.result$result_model2,survey_data.result$vote_2020)[2]
#Logistic: ROC Curve (optional)
roc.2 <- roc(survey_data.result$vote_2020, prob.2)
auc(roc.2)
plot(roc.2, auc.polygon=TRUE, print.auc = TRUE,asp = NA)


####*****Post-Stratification*****####

####Apply model on census data####
vote_2020_prob<-predict(model_logit1,census_data[,c("agegroup","gender","education","state",
                                                    "household_income","race","cell")],type="response")
vote_2020_pred<-ifelse(vote_2020_prob>0.5,"Joe Biden","Donald Trump")
census_data.result<-cbind(census_data,vote_2020_pred)

####calculate total votes based on person weight####
census_data.result$trump_votes<-ifelse(census_data.result$vote_2020_pred=="Donald Trump",census_data.result$perwt,0)
census_data.result$biden_votes<-ifelse(census_data.result$vote_2020_pred=="Joe Biden",census_data.result$perwt,0)

####Calculate vote per state####
census_data.result %>% group_by(state) %>% summarise(Trump=sum(trump_votes),Biden=sum(biden_votes))->predicted_states
predicted_states$winner<-ifelse(predicted_states$Trump>predicted_states$Biden,
                                "Donald Trump","Joe Biden")

####Mutate electoral votes per state (google this information)####
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


predicted_states %>% group_by(winner) %>% summarise(total_votes=sum(electoral_votes))->election_result
election_result