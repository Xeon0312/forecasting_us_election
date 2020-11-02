library(haven)
library(tidyverse)
#### *****Data Selection & Cleaning*****####
#### Survey data -Load and select####
raw_data_survey <- read_dta("ns20200625.dta")
raw_data_survey <- labelled::to_factor(raw_data_survey)
# Just keep some variables-make sure it exists in census data as well (except for vote intention)
reduced_data_survey <- 
  raw_data_survey %>% 
  select(vote_2020,
         vote_intention,
         registration,
         age,
         gender,
         education,
         state,
         household_income,
         race_ethnicity)
#Adjust Data types
reduced_data_survey$age<-as.numeric(reduced_data_survey$age)
# Filter on survey data
#filter only on the people that are both registered & intented to vote (Optional, depends on your assumptions)
#(Assuming people will vote unless they explicitly say no)
filtered_data_survey<-reduced_data_survey %>% 
  filter(registration=="Registered"&
           vote_intention!="No, I am not eligible to vote"&
           vote_intention!="No, I will not vote but I am eligible"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden")
  )
#Drop NAs (4296 out of 6479, 66% data kept)
filtered_data_survey<-na.omit(filtered_data_survey)

rm(raw_data_survey,reduced_data_survey)

####Census data- Load and select####
raw_data_census <- read_dta("usa_00004.dta")
raw_data_census <- labelled::to_factor(raw_data_census)
# Just keep some variables
reduced_data_census <- 
  raw_data_census %>% 
  select(perwt,
         citizen,
         age,
         sex, 
         educd,
         stateicp,
         hhincome,
         race
  )
#Change data types
reduced_data_census$age<-as.numeric(reduced_data_census$age)
#reduced_data_census$inctot<-as.numeric(reduced_data_census$inctot)
#Filter Census data- only keeping those who can vote
filtered_data_census<-reduced_data_census %>% filter(age>=18 & (citizen=="naturalized citizen"|citizen=="born abroad of american parents"))

#Adjust some NAs
filtered_data_census$hhincome<-ifelse(filtered_data_census$hhincome==9999999,
                                      NaN,filtered_data_census$hhincome)


#Drop NAs (222298/228159, 97% data kept)
filtered_data_census<-na.omit(filtered_data_census)

rm(raw_data_census,reduced_data_census)


####Map data style between survey & census####
#####Create Age group in both datasets####
filtered_data_survey<-filtered_data_survey %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 35 ~ '21 to 35',
                              age >35  & age <= 50 ~ '35 to 50',
                              age >50  & age <= 65 ~ '50 to 65',
                              age >65  & age <= 80 ~ '65 to 80',
                              age >80 ~ 'above 80'
  )) 
filtered_data_census<-filtered_data_census %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <=  35 ~ '21 to 35',
                              age >35  & age <= 50 ~ '35 to 50',
                              age >50  & age <= 65 ~ '50 to 65',
                              age >65  & age <= 80 ~ '65 to 80',
                              age >80 ~ 'above 80' 
  )) 

unique(filtered_data_census$agegroup)
unique(filtered_data_survey$agegroup)


####Map sex/gender column####
unique(filtered_data_census$sex)
unique(filtered_data_survey$gender)
filtered_data_census$sex<-ifelse(filtered_data_census$sex=="female","Female","Male")
#rename census column so that variable names match
filtered_data_census<-rename(filtered_data_census,gender=sex)

unique(filtered_data_census$gender)
unique(filtered_data_survey$gender)


####Map education/educd column####
#Some asumptions & changes made other than direct string conversion
#1.  Other post high school vocational training=High school graduate (survey data)
#2.  Completed some graduate, but no degree=College degree
#2.  professional degree beyond a bachelor's degree=College degree
unique(filtered_data_census$educd)
unique(filtered_data_survey$education)

#Survey
filtered_data_survey$education[filtered_data_survey$education=="Other post high school vocational training"]<-"High school graduate"
filtered_data_survey$education[filtered_data_survey$education=="Completed some graduate, but no degree"]<-"College Degree (such as B.A., B.S.)"
#Census
grade3.less<-c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade4to8<-c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade9to11<-c("grade 9","grade 10","grade 11","12th grade, no diploma")
edu.highsch<-c("ged or alternative credential","regular high school diploma")
edu.somecoll<-c("some college, but less than 1 year",
                "1 or more years of college credit, no degree")
filtered_data_census<-filtered_data_census %>% 
  mutate(educd2 = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd=="doctoral degree"~'Doctorate degree',
                            educd =="master's degree" ~ 'Masters degree',
                            educd=="professional degree beyond a bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd %in% edu.somecoll~"Completed some college, but no degree",
                            educd %in% edu.highsch~"High school graduate",
                            educd %in% grade9to11~"Completed some high school",
                            educd %in% grade4to8~"Middle School - Grades 4 - 8",
                            educd %in% grade3.less ~"3rd Grade or less"
  )) 
#drop educd & rename educd2
filtered_data_census<-rename(filtered_data_census,education=educd2)
filtered_data_census$educd<-NULL
unique(filtered_data_census$education)
unique(filtered_data_survey$education)

####Map Sate/Stateicp####
filtered_data_census<-filtered_data_census %>% 
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
filtered_data_census$stateicp<-NULL

unique(filtered_data_census$state)
unique(filtered_data_survey$state)

####Map household income####
x<-unique(filtered_data_survey$household_income)
min(filtered_data_census$hhincome)
max(filtered_data_census$hhincome)
filtered_data_census<-filtered_data_census %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 

filtered_data_census$hhincome<-NULL

unique(filtered_data_census$household_income)
unique(filtered_data_survey$household_income)

####Map race####
length(unique(filtered_data_survey$race_ethnicity))
length(unique(filtered_data_census$race))

other_asian_or_pacific_islander<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)","Asian (Korean)","Asian (Filipino)",
              "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
              "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")
#survey data
filtered_data_survey<-filtered_data_survey %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% other_asian_or_pacific_islander ~"other asian or pacific islander",
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity=="Other race "~"Other race"
  )) 
filtered_data_survey$race_ethnicity<-NULL

#census data
filtered_data_census<-filtered_data_census %>% 
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
unique(filtered_data_census$race2)

filtered_data_census$race<-filtered_data_census$race2
filtered_data_census$race2<-NULL

unique(filtered_data_census$race)
unique(filtered_data_survey$race)


####Populate Datset for modelling####
filtered_data_survey%>% select(vote_2020,age,agegroup,gender,education,state,household_income,race)->survey.data
filtered_data_census%>% select(perwt,age,agegroup,gender,education,state,household_income,race)->census.data
rm(filtered_data_census,filtered_data_survey)

####Create cell variable in both datasets####
#create cells- Do not create too many cells as the sample obs number is limited
survey.data$cell<-paste(survey.data$gender,survey.data$race)
census.data$cell<-paste(census.data$gender,census.data$race)


####Convert variables to factors#####
f.cols.survey<-c("agegroup","gender","education","state","household_income" ,"race", "cell","vote_2020")
survey.data[f.cols.survey] <- lapply(survey.data[f.cols.survey], factor) 
f.cols.census<-c("agegroup","gender","education","state","household_income" ,"race", "cell")
census.data[f.cols.census] <- lapply(census.data[f.cols.census], factor) 
survey.data$vote_2020 <- relevel(survey.data$vote_2020, ref = "Donald Trump")    #To predict probability of voting for Biden (Trump as ref)

####Count number of cells####
length(unique(survey.data$cell))  #90 cells in survey data
length(unique(census.data$cell))



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
cm.1<-confusionMatrix(survey.data.result$result_model1,survey.data.result$vote_2020)[2]
accu.1<-confusionMatrix(survey.data.result$result_model1,survey.data.result$vote_2020)[3]$overall['Accuracy']
cm.1
accu.1
#Logistic: ROC Curve (optional)
roc.1 <- roc(survey.data.result$vote_2020, prob.1)
auc(roc.1)
plot(roc.1, auc.polygon=TRUE, print.auc = TRUE,asp = NA)


####Model 2-Too slow, don’t run it, just to give you an idea what can be changed in building model#### 
model_logit2 <- glmer(vote_2020~(1+race+state|cell)+agegroup+gender+education+household_income,
                      data = survey.data, 
                      family=binomial)

summary(model_logit2)


prob.2<-predict(model_logit2,type=c('response'))
result_model2<-ifelse(prob.2>=0.5,"Joe Biden","Donald Trump")
survey.data.result<-cbind(survey.data.result,result_model2)

#Logistic: Confusion Matrix (optional)
cm.2<-confusionMatrix(survey.data.result$result_model2,survey.data.result$vote_2020)[2]
#Logistic: ROC Curve (optional)
roc.2 <- roc(survey.data.result$vote_2020, prob.2)
auc(roc.2)
plot(roc.2, auc.polygon=TRUE, print.auc = TRUE,asp = NA)


####*****Post-Stratification*****####

####Apply model on census data####
vote_2020_prob<-predict(model_logit1,census.data[,c("agegroup","gender","education","state",
                                                    "household_income","race","cell")],type="response")
vote_2020_pred<-ifelse(vote_2020_prob>0.5,"Joe Biden","Donald Trump")
census.data.result<-cbind(census.data,vote_2020_pred)

####calculate total votes based on person weight####
census.data.result$trump_votes<-ifelse(census.data.result$vote_2020_pred=="Donald Trump",census.data.result$perwt,0)
census.data.result$biden_votes<-ifelse(census.data.result$vote_2020_pred=="Joe Biden",census.data.result$perwt,0)

####Calculate vote per state####
census.data.result %>% group_by(state) %>% summarise(Trump=sum(trump_votes),Biden=sum(biden_votes))->predicted_states
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