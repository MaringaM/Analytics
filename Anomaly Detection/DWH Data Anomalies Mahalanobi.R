setwd("D:/OneDrive/R Work/FacilityAnomalies")
library(dplyr)

#read in raw data
raw_data<-read.csv('./DWHLogs_Feb26.csv',stringsAsFactors=FALSE)

#Variables to check for outliers
str(raw_data)

#Change all columnnames to lowercase
names(raw_data)<-tolower(names(raw_data))

#Get the Tier for the Facilities
df <- raw_data %>%
  mutate(FacilityType = ifelse(grepl('Hospital', facilityname), "Hospital",
                               ifelse(grepl("Dispensary", facilityname), "Dispensary", "Health Center")))

vars <- c("txcurr","khis_txcurr", "patients", "art",  "visits", "pharm", "labs", "exits")

#Convert the current ART Columns from Character to Numeric
df[,vars]<-lapply(df[,vars],as.numeric)

#Convert NA to 0
df[is.na(df)] <- 0

#Get the Means of the columns specified
colMeans(df[,vars])

#Get the CoVariance
cov(df[,vars])

#Calculate the mahalanobis distance
md <-mahalanobis(df[,vars],colMeans(df[,vars]),cov(df[,vars]))

#Add the Mala
df$md<-round(md,3)

#Mahalanobis Outliers
df$outlier_maha <-FALSE
df$outlier_maha[df$md>10]<-TRUE

