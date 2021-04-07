library('tidyverse')
library('ggplot2')
library(pivottabler)

source("Functiions.R")
#For Machakos Dataset
machakos<-read.csv('C:/Users/Admin/OneDrive/Analytics/HTS ML Output/Machakos Predictions.csv')

#For purposes of joining the data later, add the column county
machakos <-machakos %>% mutate(county='Machakos')

#Create Age Group Category
machakos <-machakos %>% mutate(AgeGroup=ifelse(AgeAtTest<=15,'Under 15 Yrs','Over 15 Yrs'))

#Lets now create the Risk Outcome COlumn for the 4 Values
machakos <- machakos %>% mutate(RiskOutcome = ifelse(Positive>=0.372,'Highest Risk',
                                                     ifelse(Positive >=0.25 & Positive<0.372,'High Risk',
                                                            ifelse(Positive>=0.094 & Positive<0.25,'Medium Risk','Low Risk'))))
machakos$RiskOutcome<- factor(machakos$RiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk", "Highest Risk"))
machakos$RiskOutcome<-as.factor(machakos$RiskOutcome)
table(machakos$RiskOutcome)


# Combine the 2 top risk outcomes
machakos <- machakos %>% mutate(HHRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk','High Risk',
                                                       ifelse(RiskOutcome =='Medium Risk','Medium Risk','Low Risk')))
machakos$HHRiskOutcome<- factor(machakos$HHRiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk"))
table(machakos$HHRiskOutcome)

#Combine the Medium and High Risks
machakos <- machakos %>% mutate(HHMRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk' | RiskOutcome =='Medium Risk',
                                                        'High Risk','Low Risk'))
machakos$HHMRiskOutcome<- factor(machakos$HHMRiskOutcome,levels = c("Low Risk","High Risk"))
table(machakos$HHMRiskOutcome)



AllRisks<-Set_Risk_Summary(RiskOutcome,'All Risks')
HHRisks<-Set_Risk_Summary(HHRiskOutcome,'Combi HighestHigh Risks')
HHMRisks<-Set_Risk_Summary(HHMRiskOutcome,'Combi MediumHighestHigh Risks')

# Combine the 3 Risks
combiRisks<-rbind(AllRisks,HHRisks,HHMRisks)
  
#Since Low Risk Precision and Recall doesn't change, only focus on High/Highest
combiHighRisks <-combiRisks %>%
  filter((RiskOutcome=='Highest Risk' & type=='All Risks') | (RiskOutcome=='High Risk' & type=='Combi HighestHigh Risks') 
         | (RiskOutcome=='High Risk' & type=='Combi MediumHighestHigh Risks'))
  


# Summarize the Risk Outcomes by Final Test Result and Age Group
PredsByAge<-Set_Risk_Age_Summary(RiskOutcome,AgeGroup,'All Risks')
HHPredsByAge<-Set_Risk_Age_Summary(HHRiskOutcome,AgeGroup,'Combi HighestHigh Risks')
HHMPredsByAge<-Set_Risk_Age_Summary(HHRiskOutcome,AgeGroup,'Combi MediumHighestHigh Risks')

# Combine the 3 Risks
combiAgeRisks<-rbind(PredsByAge,HHPredsByAge,HHMPredsByAge)

#Since Low Risk Precision and Recall doesn't change, only focus on High/Highest
combiAgeHighRisks <-combiAgeRisks %>%
  filter((RiskOutcome=='Highest Risk' & type=='All Risks') | (RiskOutcome=='High Risk' & type=='Combi HighestHigh Risks') 
         | (RiskOutcome=='High Risk' & type=='Combi MediumHighestHigh Risks')
  )

# Summarize the Risk Outcomes by Final Test Result and Facility
PredsBySite<-Set_Risk_Age_Summary(RiskOutcome,Sitecode,'All Risks')
HHPredsBySite<-Set_Risk_Age_Summary(HHRiskOutcome,Sitecode,'Combi HighestHigh Risks')
HHMPredsBySite<-Set_Risk_Age_Summary(HHRiskOutcome,Sitecode,'Combi MediumHighestHigh Risks')

