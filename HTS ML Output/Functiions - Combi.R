
Set_RiskOutcome<-function(dataset,highestrisk,highrisk,mediumrisk){
df <- {{dataset}}%>% mutate(RiskOutcome = ifelse(Positive>={{highestrisk}},'Highest Risk',
                                                    ifelse(Positive >={{highrisk}} & Positive<{{highestrisk}},'High Risk',
                                                           ifelse(Positive>={{mediumrisk}} & Positive<{{highrisk}},'Medium Risk','Low Risk'))))
  df$RiskOutcome<- factor(df$RiskOutcome,levels = c("Low Risk", "Medium Risk", "High Risk", "Highest Risk"))
  return (df)
}


Set_Risk_Summary <- function(dataset,outcome,name) {
df<-{{dataset}} %>% 
    group_by({{outcome}},FinalTestResult,county) %>%
    summarise(num=n()) %>% 
    ungroup()

# Rename Column HHRiskOutcome to help with Rbind later
df <- df %>% rename(RiskOutcome={{outcome}}) 

hf<-df %>% group_by(county,FinalTestResult) %>%
    summarise(sumres=sum(num)) %>% 
    ungroup()
hf <- hf  %>%  pivot_wider(names_from = FinalTestResult, values_from = sumres)
  # Rename Column HHRiskOutcome to help with Rbind later
hf <- hf %>% rename(Tot_Neg=Negative,
                    Tot_Pos=Positive,
                    hf_county=county)
  

  # Pivot Wider Final Test Result becomes the Columns, Risk Outcome becomes the Rows
df <- df  %>%  pivot_wider(names_from = FinalTestResult, values_from = num)

  # Calculate Precision 
  df<- df%>% mutate(RowTots=(Positive+Negative),
                    prec_Neg=(Negative/RowTots),
                    prec_pos=(Positive/RowTots),
                    type=name
  )
  #Combine the Tables to include the Totals for Negative & Positive
  df<- cbind(df,hf)
  df<- df%>% select(-hf_county)
  # Calculate Recall 
  # Calculate Recall 
df<- df%>%mutate(rec_Neg=(Negative/Tot_Neg),
                   rec_pos=(Positive/Tot_Pos)
                 )
return (df)
}

#Create Another function 
Set_Risk_Age_Summary <- function(dataset,outcome,var2,name) {
  df<-{{dataset}} %>% 
    group_by({{outcome}},{{var2}},FinalTestResult,county) %>%
    summarise(num=n()) %>%
    ungroup() 
  # Rename Column HHRiskOutcome to help with Rbind later
  df <- df %>% rename(RiskOutcome={{outcome}}) 
  
  hf<-df %>% group_by({{var2}},FinalTestResult,county) %>%
    summarise(sumres=sum(num)) %>% 
    ungroup()
  hf <- hf  %>%  pivot_wider(names_from = FinalTestResult, values_from = sumres)
  # Rename Column HHRiskOutcome to help with Rbind later
  hf <- hf %>% rename(Tot_Neg=Negative,
                      Tot_Pos=Positive)
# Pivot Wider Final Test Result becomes the Columns, Risk Outcome becomes the Rows
  df <- df %>% pivot_wider(names_from = FinalTestResult, values_from = num)
  
  #Put a 0 where there's NA
  df[is.na(df)] <- 0
  
  df <- df %>%mutate(RowTots=(Positive+Negative),
                     prec_Neg=Negative/(RowTots),
                     prec_pos=Positive/(RowTots),
                    type=name
  ) 
  gf<- merge(x = df, y = hf, by = c("county","AgeGroup"), all.x = TRUE)

  # Calculate Recall 
gf<- gf%>%mutate(rec_Neg=(Negative/Tot_Neg),
                 rec_pos=(Positive/Tot_Pos)
)
  
  return (gf)
}

get_HighRisk<-function(dataset){
# 
#  )  
#Since Low Risk Precision and Recall doesn't change, only focus on High/Highest
hr <-{{dataset}} %>%
    filter((RiskOutcome=='Highest Risk' & type=='Highest Risk') | (RiskOutcome=='High Risk' & type=='HighestHigh Risks') 
           | (RiskOutcome=='High Risk' & type=='MediumHighestHigh Risks')|(RiskOutcome=='All Risk' & type=='All Risks')) %>%
  mutate(RiskOutcome=ifelse((RiskOutcome=='Highest Risk' & type=='Highest Risk'),"Highest Risk",
                              ifelse((RiskOutcome=='High Risk' & type=='HighestHigh Risks'),"Highest&High Risk",
                                   ifelse((RiskOutcome=='High Risk' & type=='MediumHighestHigh Risks'),'HighestHigh&Medium Risks',"All Risks"
                                   )))
  )
}
