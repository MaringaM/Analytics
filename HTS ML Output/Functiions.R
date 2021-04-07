#Create A function 
Set_Risk_Summary <- function(dataset,outcome,name) {
  df<-{{dataset}} %>% 
    group_by({{outcome}},FinalTestResult) %>%
    summarise(num=n()) %>% 
    ungroup()
  # Rename Column HHRiskOutcome to help with Rbind later
  df <- df %>% rename(RiskOutcome={{outcome}}) 
  # Pivot Wider Final Test Result becomes the Columns, Risk Outcome becomes the Rows
  df <- df  %>%  pivot_wider(names_from = FinalTestResult, values_from = num)
  # Calculate Precision and Recall
  df<- df%>% mutate(RowTots=(Positive+Negative),
                    Tot_Neg=sum(df$Negative),
                    Tot_Pos=sum(df$Positive),
                    prec_Neg=paste0(round((Negative/RowTots)*100,digits = 0),'%'),
                    prec_pos=paste0(round((Positive/RowTots)*100,digits = 0),'%'),
                    rec_Neg=paste0(round((Negative/Tot_Neg)*100,digits = 0),'%'),
                    rec_pos=paste0(round((Positive/Tot_Pos)*100,digits = 0),'%'),
                    type=name
  )
  return (df)
}

#Create Another function 
Set_Risk_Age_Summary <- function(dataset,outcome,var2,name) {
  df<-{{dataset}} %>% 
    group_by({{outcome}},{{var2}},FinalTestResult) %>%
    summarise(num=n()) %>%
    ungroup()
  # Rename Column HHRiskOutcome to help with Rbind later
  df <- df %>% rename(RiskOutcome={{outcome}}) 
  
  # Pivot Wider Final Test Result becomes the Columns, Risk Outcome becomes the Rows
  df <- df %>% pivot_wider(names_from = FinalTestResult, values_from = num)
  
  #Put a 0 where there's NA
  df[is.na(df)] <- 0
  
  df <- df %>%mutate(RowTots=(Positive+Negative),
                     Tot_Neg=sum(df$Negative),
                     Tot_Pos=sum(df$Positive),
                     prec_Neg=paste0(round((Negative/(RowTots))*100,digits = 0),'%'),
                     prec_pos=paste0(round((Positive/(RowTots))*100,digits = 0),'%'),
                     rec_Neg=paste0(round((Negative/Tot_Neg)*100,digits = 0),'%'),
                     rec_pos=paste0(round((Positive/Tot_Neg)*100,digits = 0),'%'),
                    type=name
  ) 
  return (df)
}
