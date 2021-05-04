# Load Libraries
library(tidyverse)
library(openxlsx)
library(readxl)
library(lubridate)

#Set Working Directory
setwd("C:/Users/Admin/Desktop/Reporting Rates")

#Read in the excel raw data
rr<-read_excel("ReportingRates HTS Raw.xlsx",na = "NULL")

#Declare From and To Dates
FromDate=as.Date("2021-03-01",origin = "1899-12-30")
ToDate=ceiling_date(ymd(as.Date(FromDate)) %m+% months(0), "month")+days(4)


#Convert Upload Dates from String to Dates
  #rr$UploadDate=as.Date(as.numeric(rr$UploadDate), origin = "1899-12-30")
  #rr$UploadDate_MPI=as.Date(as.numeric(rr$UploadDate_MPI), origin = "1899-12-30")

#Convert to Month Year format
rr$Upload_monthYear_HTS=format(rr$Upload_monthYear_HTS,"%b-%Y")
rr$Upload_monthYear_MPI=format(rr$Upload_monthYear_MPI, "%b-%Y")

# HTS Is Unique - Create a unique Identifier (MFLCode+Partner) to cater for multi-partner support
rr<-rr %>% mutate(UniqIdentifier=paste0(DisplayMFL,'-',DisplayMechanism))


#Remove Duplicates from original dataset - Uniwue Site with 1 Upload Date, upload MPI
rr <-rr %>%
  group_by(UniqIdentifier,UploadDate,UploadDate_MPI) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  select(-rownum)

#In Original Dataset Create a column called UploadedHTS & UploadedMPI
rr <- rr %>% mutate(uploadedHTS=ifelse(!is.na(UploadDate),1,0),
                    uploadedMPI=ifelse(!is.na(UploadDate_MPI),1,0),
)

# Unique Number of EMR Sites = Denominator
length(unique(rr$UniqIdentifier))

#Create a data Frame to hold distinct list of sites for the denominator / Baseline EMR Sites
#We will use later for left joining
rr_denom <-rr %>% select(UniqIdentifier,DisplayMFL, DisplayFacilityName, DisplayCounty, DisplayMechanism, DisplayAgency) %>%
  group_by(UniqIdentifier) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%
  ungroup() %>%
  select(-rownum)

#   HTS RECENCY

#Filter the Data to the specific RR Period
hts_recency<-rr %>%filter((UploadDate>=FromDate & UploadDate <=ToDate))
length(hts_recency$UniqIdentifier)

#Number of Duplicates in HTS_Recency Those that reported in both April and between 1st and 5th May
length(hts_recency$UniqIdentifier) - length(unique(hts_recency$UniqIdentifier))

#Remove duplicates.Making Sure we are counting a site only once within the reporting period being looked at
hts_recency <-hts_recency %>% 
  group_by(UniqIdentifier) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%  
  ungroup() %>%  
  select(-rownum)

#Join the CT Recency Dataset to the EMR Sites Denominator
#Left Join
rt<-merge(x = rr_denom, y = hts_recency, by = "UniqIdentifier", all.x = TRUE)

#Select the columns to remain with
rt <-rt %>% select(UniqIdentifier,DisplayMFL.x, DisplayFacilityName.x, DisplayCounty.x, DisplayMechanism.x, DisplayAgency.x, UploadDate)

#Rename the column Names in the data frame
rt<-rt %>%
  rename(
    MFLCode = `DisplayMFL.x`,
    FacilityName= `DisplayFacilityName.x`,
    County = `DisplayCounty.x`,
    CTPartner=`DisplayMechanism.x`,
    Agency =`DisplayAgency.x`
  )

###   MPI RECENCY
#Filter the Data to the specific RR Period
mpi_recency<-rr %>%filter((UploadDate_MPI>=FromDate & UploadDate_MPI <=ToDate))

#Number of Duplicates in CT_Recency
length(mpi_recency$UniqIdentifier) - length(unique(mpi_recency$UniqIdentifier))

#Remove duplicates.
#Making Sure we are counting a site only once within the reporting period being looked at
mpi_recency <-mpi_recency %>% 
  group_by(UniqIdentifier) %>%  
  mutate(rownum = row_number()) %>%
  filter(rownum == 1) %>%  
  ungroup() %>%  
  select(-rownum)

#Join the MPI Recency Dataset to the Combined CT Recency n EMR Sites Denominator
#Left Join
rt2<-merge(x = rt, y = mpi_recency, by = "UniqIdentifier", all.x = TRUE)

#Delete the 2 data frames we don't need
rm(mpi_recency)
rt2 <-rt2 %>% select(UniqIdentifier,DisplayMFL, FacilityName, County, CTPartner, Agency, UploadDate.x,UploadDate_MPI)

#Rename the column Names in the data frane
rt2<-rt2 %>% rename(UploadDate= `UploadDate.x`)

#### CONSISTENCY HTS - Uploaded every month last 3 months

#Filter the data to return - only sites that have Uploaded every month last 3 months
hts_consistency<-rr %>%select(UniqIdentifier,DisplayMFL,DisplayFacilityName,DisplayCounty,DisplayMechanism,
                             Upload_monthYear_HTS,UploadDate,uploadedHTS) %>%
  filter(UploadDate >=(ymd(as.Date(FromDate)) %m+% months(-2))
         & UploadDate <=ceiling_date(ymd(as.Date(FromDate)) %m+% months(0), "month")+days(4))

#Pivot the Data then add instances of uploads by month, filter only those that uploaded all 3 months
hts_consistency<-hts_consistency %>% select(UniqIdentifier,DisplayMFL,DisplayFacilityName,Upload_monthYear_HTS,uploadedHTS) %>%
  pivot_wider(names_from = Upload_monthYear_HTS, values_from=uploadedHTS) %>%
  replace(is.na(.), 0) %>%
  mutate(sums = rowSums(.[4:7],na.rm = TRUE)) %>%
  filter(sums>=3)



#Left Join with the recency (hts and MPI) and denominator datasets
combined_rr<-merge(x = rt2, y = hts_consistency, by = "UniqIdentifier", all.x = TRUE)

combined_rr<-combined_rr %>% rename(DisplayMFL= `DisplayMFL.x`)

#Delete the un_used dataframes
rm(rt2)
rm(hts_consistency)

# Generate Consistency raw dataset
combined_rr <-combined_rr %>%
  select(UniqIdentifier,DisplayMFL,FacilityName,County,CTPartner,Agency,UploadDate,UploadDate_MPI,sums)

# Filter out the sites that have reported 3 times or more between the From and to date
combined_rr <- combined_rr %>% select(DisplayMFL,FacilityName,County,CTPartner,Agency,UploadDate,UploadDate_MPI,sums) %>%
  mutate(hts_recency=ifelse(!is.na(UploadDate),1,0),
         mpi_recency=ifelse(!is.na(UploadDate_MPI),1,0),
         hts_consistency=ifelse(sums==3,1,0))


# Create a data frame that has expehtsed uploads, Recency of Reporting and Consistency by County
hts_ByCounty <- combined_rr %>%
  group_by(County) %>%
  summarise(numsites=n(),
            Recency=sum(hts_recency),
            per_htsRecency=(Recency/numsites),
            htsConsistency=sum(hts_consistency,na.rm = TRUE),
            per_htsConsistency=(htsConsistency/numsites),
            MPI_Recency=sum(mpi_recency,na.rm = TRUE),
            per_MPIRecency=(MPI_Recency/numsites)) %>%
  ungroup()
sum(hts_ByCounty$htsConsistency)

# Create a data frame that has expehtsed uploads, Recency of Reporting and Consistency by Partner
hts_ByPartner <- combined_rr %>%
  group_by(CTPartner) %>%
  summarise(numsites=n(),
            hts_recency=sum(hts_recency),
            per_htsRecency=(hts_recency/numsites),
            htsConsistency=sum(hts_consistency,na.rm = TRUE),
            per_htsConsistency=(htsConsistency/numsites),
            mpi_recency=sum(mpi_recency),
            per_MPIRecency=(mpi_recency/numsites)) %>%
  ungroup()


FileName=paste0("ReportingRates_HTS_",format(FromDate, format = "%B%Y"),".xlsx")
list_of_datasets <- list("hts_ByCounty" = hts_ByCounty, "hts_ByPartner" = hts_ByPartner)
write.xlsx(list_of_datasets, file = FileName)


