# Load Libraries
library(tidyverse)
library(openxlsx)
library(readxl)
library(lubridate)

#Set Working Directory
setwd("C:/Users/Admin/Desktop/Reporting Rates")

#Read in the excel raw data
rr<-read_excel("ReportingRates CT Raw.xlsx")

#Declare From and To Dates
FromDate=as.Date("2021-04-01",origin = "1899-12-30")
ToDate=ceiling_date(ymd(as.Date(FromDate)) %m+% months(0), "month")+days(4)

#Replace NULL with na in the entire dataset
rr<-na_if(rr,"NULL")

#Convert Upload Dates from String to Dates
rr$UploadDate=as.Date(as.numeric(rr$UploadDate), origin = "1899-12-30")
rr$UploadDate_MPI=as.Date(as.numeric(rr$UploadDate_MPI), origin = "1899-12-30")
#Convert to Month Year format
rr$Upload_monthYear=format(as.Date(as.numeric(rr$Upload_monthYear), origin = "1899-12-30"),"%b-%Y")
rr$Upload_monthYear_MPI=format(as.Date(as.numeric(rr$Upload_monthYear_MPI), origin = "1899-12-30"), "%b-%Y")

#Remove Duplicates from original dataset - Uniwue Site with 1 Upload Date, upload MPI
rr <-rr %>%
  group_by(DisplayMFL,UploadDate,UploadDate_MPI) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  select(-rownum)

#In Original Dataset Create a column called UploadedCT & UploadedMPI
rr <- rr %>% mutate(uploadedCT=ifelse(!is.na(UploadDate),1,0),
                    uploadedMPI=ifelse(!is.na(UploadDate_MPI),1,0),
)

# Unique Number of EMR Sites = Denominator
length(unique(rr$DisplayMFL))

#Create a data Frame to hold distinct list of sites for the denominator / Baseline EMR Sites
#We will use later for left joining
rr_denom <-rr %>% select(DisplayMFL, DisplayFacilityName, DisplayCounty, DisplayMechanism, DisplaySubcounty, DisplayAgency) %>%
  group_by(DisplayMFL) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  select(-rownum)

###   CT RECENCY

#Filter the Data to the specific RR Period
ct_recency<-rr %>%filter((UploadDate>=FromDate & UploadDate <=ToDate))
length(ct_recency$DisplayMFL)

#Number of Duplicates in CT_Recency Those that reported in both April and between 1st and 5th May
length(ct_recency$DisplayMFL) - length(unique(ct_recency$DisplayMFL))

#Remove duplicates.Making Sure we are counting a site only once within the reporting period being looked at
ct_recency <-ct_recency %>%   group_by(DisplayMFL) %>%   mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%  ungroup() %>%  select(-rownum)

#Join the CT Recency Dataset to the EMR Sites Denominator
#Left Join
rt<-merge(x = rr_denom, y = ct_recency, by = "DisplayMFL", all.x = TRUE)

#Select the columns to remain with
rt <-rt %>% select(DisplayMFL, DisplayFacilityName.x, DisplayCounty.x, DisplayMechanism.x, DisplaySubcounty.x, DisplayAgency.x, UploadDate)

#Rename the column Names in the data frane
rt<-rt %>%
  rename(
    FacilityName= `DisplayFacilityName.x`,
    County = `DisplayCounty.x`,
    CTPartner=`DisplayMechanism.x`,
    Subcounty= `DisplaySubcounty.x`,
    Agency =`DisplayAgency.x`
  )

###   MPI RECENCY
#Filter the Data to the specific RR Period
mpi_recency<-rr %>%filter((UploadDate_MPI>=FromDate & UploadDate_MPI <=ToDate))

#Number of Duplicates in CT_Recency
length(mpi_recency$DisplayMFL) - length(unique(mpi_recency$DisplayMFL))

#Remove duplicates.
#Making Sure we are counting a site only once within the reporting period being looked at
mpi_recency <-mpi_recency %>%   group_by(DisplayMFL) %>%   mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%  ungroup() %>%  select(-rownum)

#Join the MPI Recency Dataset to the Combined CT Recency n EMR Sites Denominator
#Left Join
rt2<-merge(x = rt, y = mpi_recency, by = "DisplayMFL", all.x = TRUE)

#Delete the 2 data frames we don't need
rm(mpi_recency)
rt2 <-rt2 %>% select(DisplayMFL, FacilityName, County, CTPartner, Subcounty, Agency, UploadDate.x,UploadDate_MPI)
#Rename the column Names in the data frane
rt2<-rt2 %>% rename(UploadDate= `UploadDate.x`)


#### CONSISTENCY CT - Uploaded every month last 3 months
# ymd(as.Date(StartDate)) %m+% months(-2)) - Gives us the start Date for l3 Months back
# Gives us the last Date ceiling_date(ymd(as.Date(StartDate)) %m+% months(0), "month")-days(1)

#Filter the data to return - only sites that have Uploaded every month last 3 months
ct_consistency<-rr %>%select(DisplayMFL,DisplayFacilityName,DisplayCounty,DisplayMechanism,
                             Upload_monthYear,UploadDate,uploadedCT) %>%
  filter(UploadDate >=(ymd(as.Date(FromDate)) %m+% months(-2))
         & UploadDate <=ceiling_date(ymd(as.Date(FromDate)) %m+% months(0), "month")+days(4))

#Pivot the Data then add instances of uploads by month, filter only those that uploaded all 3 months
ct_consistency<-ct_consistency %>% select(DisplayMFL,DisplayFacilityName,Upload_monthYear,uploadedCT) %>%
  pivot_wider(names_from = Upload_monthYear, values_from=uploadedCT) %>%
  replace(is.na(.), 0) %>%
  mutate(sums = rowSums(.[3:6],na.rm = TRUE)) %>%
 filter(sums>=3)

#Left Join with the recency (CT and MPI) and denominator datasets
combined_rr<-merge(x = rt2, y = ct_consistency, by = "DisplayMFL", all.x = TRUE)

#Delete the un_used dataframes
rm(rt2)
rm(ct_consistency)

# Generate Consistency raw dataset
combined_rr <-combined_rr %>%
  select(DisplayMFL,FacilityName,County,Subcounty,CTPartner,Agency,UploadDate,UploadDate_MPI,sums)

# Filter out the sites that have reported 3 times or more between the From and to date
combined_rr <- combined_rr %>% select(DisplayMFL,FacilityName,County,Subcounty,CTPartner,Agency,UploadDate,UploadDate_MPI,sums) %>%
  mutate(ct_recency=ifelse(!is.na(UploadDate),1,0),
         mpi_recency=ifelse(!is.na(UploadDate_MPI),1,0),
         ct_consistency=ifelse(sums==3,1,0))

# Create a data frame that has expected uploads, Recency of Reporting and Consistency by County
CT_ByCounty <- combined_rr %>%
  group_by(County) %>%
  summarise(numsites=n(),
            Recency=sum(ct_recency),
            per_CTRecency=(Recency/numsites),
            CTConsistency=sum(ct_consistency,na.rm = TRUE),
            per_CTConsistency=(CTConsistency/numsites),
            MPI_Recency=sum(mpi_recency,na.rm = TRUE),
            per_MPIRecency=(MPI_Recency/numsites)) %>%
  ungroup()
sum(CT_ByCounty$CTConsistency)

# Create a data frame that has expected uploads, Recency of Reporting and Consistency by Partner
CT_ByPartner <- combined_rr %>%
  group_by(CTPartner) %>%
  summarise(numsites=n(),
            ct_recency=sum(ct_recency),
            per_CTRecency=(ct_recency/numsites),
            CTConsistency=sum(ct_consistency,na.rm = TRUE),
            per_CTConsistency=(CTConsistency/numsites),
            mpi_recency=sum(mpi_recency),
            per_MPIRecency=(mpi_recency/numsites)) %>%
  ungroup()


FileName=paste0("ReportingRates_CT_",format(FromDate, format = "%B%Y"),".xlsx")
list_of_datasets <- list("CT_ByCounty" = CT_ByCounty, "CT_ByPartner" = CT_ByPartner)
write.xlsx(list_of_datasets, file = FileName)



