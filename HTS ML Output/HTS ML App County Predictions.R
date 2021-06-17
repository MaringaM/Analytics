library('tidyverse')
library('ggplot2')
library('kableExtra')
library('Hmisc') #This Library will help in dividing the data into groups  using the cut function

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



#Compute the Risk Outcomes  for the different Counties
machakos <-Set_RiskOutcome(machakos,0.372,0.25,0.094)
nairobi <-Set_RiskOutcome(nairobi,0.562,0.275,0.088)
siaya <-Set_RiskOutcome(siaya,0.508,0.26,0.076)
homabay <-Set_RiskOutcome(homabay,0.362,0.188,0.05)

#Combine the Nairobi and Machakos and Siaya Datasets
combi <- rbind(machakos,nairobi,siaya,homabay)

# Reordering group factor levels
combi$county <- factor(combi$county,  levels = c("Siaya", "Homabay", "Nairobi", "Machakos"))



rm(machakos)
rm(nairobi)
rm(siaya)
rm(homabay)


# Include RowNumber in Combined Dataset
combi <- combi %>% 
  arrange(desc(Positive)) %>%
  mutate(rowNum=row_number())
#Put the Testing Data into Groups
combi$RowGroup<-as.numeric(cut2(combi$rowNum, g=10))
combi$RowGroup<-combi$RowGroup*10

combi <- combi %>% group_by(FinalTestResult,RowGroup)%>% mutate(ResRowNum=row_number()) %>% ungroup()
table(combi$RiskOutcome,combi$county)


# Output Positivity for Each County
posit <- combi %>% group_by(county,FinalTestResult) %>% summarise(num=n()) %>% ungroup()
# Pivot Wider Final Test Result becomes the Columns, Risk Outcome becomes the Rows
posit <- posit %>% pivot_wider(names_from = FinalTestResult, values_from = num)
posit <-posit %>% mutate(TotalTested=Positive+Negative,
positivity=(Positive/TotalTested))

#Create Age Group Category
combi <-combi %>% mutate(AgeGroup=ifelse(AgeAtTest<15,'Under 15 Yrs','Over 15 Yrs'))

#Create Age Group Category - Based on DATIM
combi <-combi %>% mutate(Age_Grp=ifelse(AgeAtTest<=9,'Under 10 Yrs',
                                         ifelse(AgeAtTest>=10 & AgeAtTest<=10, '10 to 14 Yrs',
                                                ifelse(AgeAtTest>=15 & AgeAtTest<=19, '15 to 19 Yrs',"Over 20 Years"
                                                       ))))

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

saveRDS(combi, file = "combi.rds")

# Combine the 4 Risks
combiRisks<-rbind(Set_Risk_Summary(combi,HHMLRiskOutcome,'All Risks'),
                  Set_Risk_Summary(combi,RiskOutcome,'Highest Risk'),
                  Set_Risk_Summary(combi,HHRiskOutcome,'HighestHigh Risks'),
                  Set_Risk_Summary(combi,HHMRiskOutcome,'MediumHighestHigh Risks'))

combiHighRisk<-get_HighRisk(combiRisks)

saveRDS(combiHighRisk, file = "combiHighRisk.rds")


# Summarize the Risk Outcomes by Final Test Result and Age Group

combiRisksAge<-rbind(Set_Risk_Age_Summary(combi,HHMLRiskOutcome,AgeGroup,'All Risks'),
                     Set_Risk_Age_Summary(combi,RiskOutcome,AgeGroup,'All Risk'),
                     Set_Risk_Age_Summary(combi,HHRiskOutcome,AgeGroup,'HighestHigh Risks'),
                     Set_Risk_Age_Summary(combi,HHMRiskOutcome,AgeGroup,'MediumHighestHigh Risks'))

combiHighRisksAge<-get_HighRisk(combiRisksAge)

saveRDS(combiHighRisksAge, file = "combiHighRisksAge.rds")
# Summarize the Risk Outcomes by Final Test Result and Age Group (Including 10-14, 15-19)

combiRisksAgeGrp<-rbind(Set_Risk_Age_Summary(combi,HHMLRiskOutcome,Age_Grp,'All Risks'),
                     Set_Risk_Age_Summary(combi,RiskOutcome,Age_Grp,'All Risk'),
                     Set_Risk_Age_Summary(combi,HHRiskOutcome,Age_Grp,'HighestHigh Risks'),
                     Set_Risk_Age_Summary(combi,HHMRiskOutcome,Age_Grp,'MediumHighestHigh Risks'))

combiHighRisksAgeGrp<-get_HighRisk(combiRisksAgeGrp)

combiHighRisksAgeGrp_Adole <- combiHighRisksAgeGrp %>% 
  filter(Age_Grp %in% c("Under 10 Yrs","10 to 14 Yrs","15 to 19 Yrs"))
saveRDS(combiHighRisksAgeGrp_Adole, file = "combiHighRisksAgeGrp_Adole.rds")

