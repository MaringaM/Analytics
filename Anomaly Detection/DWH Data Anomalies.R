setwd("D:/OneDrive/R Work/FacilityAnomalies")
library(dplyr)

#read in raw data
raw_data<-read.csv('./DWHLogs_Feb26.csv',stringsAsFactors=FALSE)

#Variables to check for outliers
str(raw_data)

#Change all columnnames to lowercase
names(raw_data)<-tolower(names(raw_data))

#Get the Tier for the Facilities
raw_data <- raw_data %>%
  mutate(FacilityType = ifelse(grepl('Hospital', facilityname), "Hospital",
                               ifelse(grepl("Dispensary", facilityname), "Dispensary", "Health Center")))

df<-raw_data %>%  filter(dashboardrefreshdate=='2/22/2021' & num==1)

df$dateuploaded<-as.Date(df$dateuploaded,format="%m/%d/%Y")
df$siteabstractiondate<-as.Date(df$siteabstractiondate,format="%m/%d/%Y")

df <- df%>%
  mutate(diff_days=difftime(df$dateuploaded,df$siteabstractiondate,units=c("days")),
         days_eom=(difftime(as.Date("2021-01-31",origin = "1899-12-30"),df$siteabstractiondate,units=c("days")))
  )

vars <- c("txcurr","khis_txcurr", "patients", "art",  "visits", "pharm", "labs", "exits", "diff_days")

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

df$txcurr_var<-(1-(df$txcurr/df$khis_txcurr))*100
df$txcurr_var=round(df$txcurr_var,0)

df <-df %>% mutate(dbStatus = ifelse((substr(dateuploaded, 1, 4)<2021),"Site Uploaded before 2021",
                                     ifelse(diff_days<0,"Site Abstraction issue",
                                            ifelse(diff_days>=0 & diff_days<=30,"Data uploaded is within 30 days","Stale DB"))),
                   var_status=ifelse(txcurr_var>=-5 & txcurr_var <=5,"Acceptable Variance","Out of bounds"))

df <-df %>% mutate(dbStatus_eom = ifelse(days_eom<=0, "Uploaded bef Jan31(<=5 days) & after Jan","Uploaded more than 5 days before Jan31"))

df_inc <-df %>% select (mflcode,facilityname,county,ctpartner,dateuploaded,patients,art,visits,labs,pharm,exits)%>%
  filter(patients==0 | art==0 | visits==0 | pharm==0 | labs==0| exits ==0)
write.csv(df_inc,"inc.csv")

statsbyEMR<-df %>% group_by(dbStatus_eom,dbStatus,emrstatus,emr,var_status,txcurr_var) %>%
  summarise(sites=n(),
            txcurr=sum(txcurr),
            k_txcurr=sum(khis_txcurr)) %>%
  ungroup()

out<-df%>% select(mflcode,facilityname,county,ctpartner,emr,emrstatus,dateuploaded,siteabstractiondate,txcurr,khis_txcurr,txcurr_var,var_status,dbStatus,dbStatus_eom)


ken_ob<-df %>% select (facilityname,dateuploaded,siteabstractiondate,txcurr,khis_txcurr,days_eom,dbStatus,var_status,emr) %>%
  filter(dbStatus=="Data uploaded is within 30 days" & txcurr_var>0)
