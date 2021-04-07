library('tidyverse')
library('ggplot2')
library(kableExtra)

source("C:/Users/Admin/OneDrive/Analytics/HTS ML Output/Functiions - Combi.R")

machakos<-read.csv('C:/Users/Admin/OneDrive/Analytics/HTS ML Output/machakos_preds.csv')
nairobi <-read.csv('C:/Users/Admin/OneDrive/Analytics/HTS ML Output/nairobi_preds.csv')
siaya <-read.csv('C:/Users/Admin/OneDrive/Analytics/HTS ML Output/siaya_preds.csv')
homabay <-read.csv('C:/Users/Admin/OneDrive/Analytics/HTS ML Output/homabay_preds.csv')

#For purposes of joining the data later, add the column county
machakos <-machakos %>% mutate(county='Machakos')
nairobi <-nairobi %>% mutate(county='Nairobi')
siaya <-siaya %>% mutate(county='Siaya')
homabay <-homabay %>% mutate(county='Homabay')

# Create Function for computing the Risk Outcomes for different Counties
Set_RiskOutcome<-function(dataset,highestrisk,highrisk,mediumrisk){
  df <- {{dataset}}%>% mutate(RiskOutcome = ifelse(Positive>={{highestrisk}},'Highest Risk',
                                                   ifelse(Positive >={{highrisk}} & Positive<{{highestrisk}},'High Risk',
                                                          ifelse(Positive>={{mediumrisk}} & Positive<{{highrisk}},'Medium Risk','Low Risk'))))
  df$RiskOutcome<- factor(df$RiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk", "Highest Risk"))
  return (df)
}

#Compute the Risk Outcomes  for the different Counties
machakos <-Set_RiskOutcome(machakos,0.372,0.25,0.094)
nairobi <-Set_RiskOutcome(nairobi,0.562,0.275,0.088)
siaya <-Set_RiskOutcome(siaya,0.508,0.26,0.076)
homabay <-Set_RiskOutcome(homabay,0.362,0.188,0.05)

#Combine the Nairobi and Machakos and Siaya Datasets
combi <- rbind(machakos,nairobi,siaya,homabay)

 
table(combi$RiskOutcome,combi$county)

rm(machakos)
rm(nairobi)
rm(siaya)
rm(homabay)

# Output Positivity for Each County
posit <- combi %>% group_by(county,FinalTestResult) %>% summarise(num=n()) %>% ungroup()
# Pivot Wider Final Test Result becomes the Columns, Risk Outcome becomes the Rows
posit <- posit %>% pivot_wider(names_from = FinalTestResult, values_from = num)
posit <-posit %>% mutate(TotalTested=Positive+Negative,
positivity=(Positive/TotalTested))

#Formatted Table 

#Create Age Group Category
combi <-combi %>% mutate(AgeGroup=ifelse(AgeAtTest<15,'Under 15 Yrs','Over 15 Yrs'))

# Combine the 2 top risk outcomes
combi <- combi %>% mutate(HHRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk','High Risk',
                                               ifelse(RiskOutcome =='Medium Risk','Medium Risk','Low Risk')))
combi$HHRiskOutcome<- factor(combi$HHRiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk"))
#Combine the Medium and High Risks
combi <- combi %>% mutate(HHMRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk' | RiskOutcome =='Medium Risk',
                                                'High Risk','Low Risk'))
combi$HHMRiskOutcome<- factor(combi$HHMRiskOutcome,levels = c("Low Risk","High Risk"))

combi <- combi %>% mutate(HHMLRiskOutcome = ifelse(RiskOutcome =='Highest Risk'| RiskOutcome =='High Risk' | RiskOutcome =='Medium Risk' |RiskOutcome =='Low Risk',
                                                  'All Risk','Aii'))



# Combine the 4 Risks
combiRisks<-rbind(Set_Risk_Summary(combi,HHMLRiskOutcome,'All Risks'),
                  Set_Risk_Summary(combi,RiskOutcome,'Highest Risk'),
                  Set_Risk_Summary(combi,HHRiskOutcome,'HighestHigh Risks'),
                  Set_Risk_Summary(combi,HHMRiskOutcome,'MediumHighestHigh Risks'))

combiHighRisk<-get_HighRisk(combiRisks)



# Summarize the Risk Outcomes by Final Test Result and Age Group

combiRisksAge<-rbind(Set_Risk_Age_Summary(combi,HHMLRiskOutcome,AgeGroup,'All Risks'),
                     Set_Risk_Age_Summary(combi,RiskOutcome,AgeGroup,'All Risk'),
                     Set_Risk_Age_Summary(combi,HHRiskOutcome,AgeGroup,'HighestHigh Risks'),
                     Set_Risk_Age_Summary(combi,HHMRiskOutcome,AgeGroup,'MediumHighestHigh Risks'))

combiHighRisksAge<-get_HighRisk(combiRisksAge)
