library('tidyverse')
library('ggplot2')
library(pivottabler)

source("Functiions.R")
#For pred Dataset
pred<-read.csv('C:/Users/Admin/OneDrive/Analytics/HTS ML Output/Nairobi Predictions.csv')

#For purposes of joining the data later, add the column county
pred <-pred %>% mutate(county='Nairobi')

#Create Age Group Category
pred <-pred %>% mutate(AgeGroup=ifelse(AgeAtTest<=15,'Under 15 Yrs','Over 15 Yrs'))

#Lets now create the Risk Outcome Column for the 4 Values
pred <- pred %>% mutate(RiskOutcome = ifelse(Positive>=0.372,'Highest Risk',
                                                     ifelse(Positive >=0.25 & Positive<0.372,'High Risk',
                                                            ifelse(Positive>=0.094 & Positive<0.25,'Medium Risk','Low Risk'))))
pred$RiskOutcome<- factor(pred$RiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk", "Highest Risk"))
pred$RiskOutcome<-as.factor(pred$RiskOutcome)
table(pred$RiskOutcome)


# Combine the 2 top risk outcomes
pred <- pred %>% mutate(HHRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk','High Risk',
                                                       ifelse(RiskOutcome =='Medium Risk','Medium Risk','Low Risk')))
pred$HHRiskOutcome<- factor(pred$HHRiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk"))
table(pred$HHRiskOutcome)

#Combine the Medium and High Risks
pred <- pred %>% mutate(HHMRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk' | RiskOutcome =='Medium Risk',
                                                        'High Risk','Low Risk'))
pred$HHMRiskOutcome<- factor(pred$HHMRiskOutcome,levels = c("Low Risk","High Risk"))
table(pred$HHMRiskOutcome)



AllRisks<-Set_Risk_Summary(pred,RiskOutcome,'All Risks')
HHRisks<-Set_Risk_Summary(pred,HHRiskOutcome,'Combi HighestHigh Risks')
HHMRisks<-Set_Risk_Summary(pred,HHMRiskOutcome,'Combi MediumHighestHigh Risks')

# Combine the 3 Risks
combiRisks<-rbind(AllRisks,HHRisks,HHMRisks)

#Since Low Risk Precision and Recall doesn't change, only focus on High/Highest
combiHighRisks <-combiRisks %>%
  filter((RiskOutcome=='Highest Risk' & type=='All Risks') | (RiskOutcome=='High Risk' & type=='Combi HighestHigh Risks') 
         | (RiskOutcome=='High Risk' & type=='Combi MediumHighestHigh Risks'))



# Summarize the Risk Outcomes by Final Test Result and Age Group
PredsByAge<-Set_Risk_Age_Summary(pred,RiskOutcome,AgeGroup,'All Risks')
HHPredsByAge<-Set_Risk_Age_Summary(pred,HHRiskOutcome,AgeGroup,'Combi HighestHigh Risks')
HHMPredsByAge<-Set_Risk_Age_Summary(pred,HHMRiskOutcome,AgeGroup,'Combi MediumHighestHigh Risks')

# Combine the 3 Risks
combiAgeRisks<-rbind(PredsByAge,HHPredsByAge,HHMPredsByAge)

#Since Low Risk Precision and Recall doesn't change, only focus on High/Highest
combiAgeHighRisks <-combiAgeRisks %>%
  filter((RiskOutcome=='Highest Risk' & type=='All Risks') | (RiskOutcome=='High Risk' & type=='Combi HighestHigh Risks') 
         | (RiskOutcome=='High Risk' & type=='Combi MediumHighestHigh Risks')
  )

# Summarize the Risk Outcomes by Final Test Result and Facility
PredsBySite<-Set_Risk_Age_Summary(pred,RiskOutcome,Sitecode,'All Risks')
HHPredsBySite<-Set_Risk_Age_Summary(pred,HHRiskOutcome,Sitecode,'Combi HighestHigh Risks')
HHMPredsBySite<-Set_Risk_Age_Summary(pred,HHRiskOutcome,Sitecode,'Combi MediumHighestHigh Risks')

