library(httr)
library(jsonlite)
library(tidyverse)

#Declare From and To Dates
StartDate=as.Date("2021-02-01",origin = "1899-12-30")
EndDate=as.Date("2021-02-28",origin = "1899-12-30")

dataperiods <-c(paste0(format(seq(StartDate,EndDate,by="month"), "%Y%m"),";"))
dataperiods =noquote(dataperiods)

url="https://hiskenya.org/api/26/analytics.json?dimension=ou:LEVEL-5;HfVjCurKxh2&dimension=dx:NYkr7LlxmUg;qw7eJpLE3vK;dlldM4hP2Wk;NOga2tabGrd;wu0ITFRjUzF;D9YwtS6RhQ1;xMNhnyu7vm1;kLXGWRLzCAw;du5RMT3aecB;oCFXmpol7D8;F9OR49Lc1aR;cBTa1jVzT8f;J4vNm7YEkdj;M5zablilTPO;gMICOUtzqRb;lj9QYJqS7bN;YXJf27jfkvS;XiRbc0DSMOH;OePJt8CcZ0d;JiuqbydCIcy;pkShOkgNQt2;atSQz5O7e2A;gTkVw97FnQK;Yk5WVF4EQoX;kmuBTU9NAjw;G7zACbrJMmY;Th97Ccd4BuJ;OYGWqG4HnBZ;M6hV3ObOCHV&dimension=pe:"
endurl="&displayProperty=NAME&showHierarchy=true&tableLayout=true&columns=dx&rows=ou;pe&hideEmptyRows=true&paging=false"

combi_url=paste(url,paste(dataperiods,collapse=" "),endurl)

#Remove Spaces from the URL
combi_url<- as.character(gsub(" ", "",combi_url, perl=TRUE))
#Remove the ; before &
combi_url<-as.character(gsub(";&", "&",combi_url, perl=TRUE))

#Remove any enter Keys in the URL
combi_url<-as.character(gsub("\r\n", "",combi_url, perl=TRUE))

#get Username and Password
username <- noquote(rstudioapi::askForPassword("Khis username"))
password <- noquote(rstudioapi::askForPassword("KHIS password"))

#Get Data from Api in JSON Format
cont_raw = GET(combi_url,authenticate(username,password))
#Get Content from JSONFile
data_raw_ugly = fromJSON(rawToChar(cont_raw$content))

#Extract ColumnNames from Header file of JSON Dataset
raw_Names<-data_raw_ugly$headers$name

#Extract data from the JSON Dataset
data_f<-as.data.frame(data_raw_ugly$rows)

#Add  ColumnNames from raw_Names to the data_f
colnames(data_f)<-raw_Names


#Clean_up Data Frame Column Names
clean_df<-data_f %>% rename(
  country="Org unit level 1",
  County="Org unit level 2",
  SubCounty="Org unit level 3",
  Ward="Org unit level 4",
  Facility="Org unit level 5",
  khisId="Organisation unit ID",
  MFLCode="Organisation unit code",
  ReportPeriod="Period ID",
  TotalLinked="MOH 731   Linked_Total HV01-35",
  NegativeTotal="MOH 731   Negative_Total HV01-27",
  PositiveTotal="MOH 731   Positive_Total   (Sum  HV01-18 to  HV01-25) HV01-26",
  TestedTotal="MOH 731  Tested_Total   (Sum  HV01-01 to  HV01-10) HV01-10",
  Positive_1014F="MOH 731   Positive_10-14(F)  HV01-19",
  Positive_1014M="MOH 731   Positive_10-14(M)  HV01-18",
  Positive_1519F= "MOH 731   Positive_15-19(F)  HV01-21" ,                        
  Positive_1519M= "MOH 731   Positive_15-19(M) HV01-20" ,                         
  Positive_19= "MOH 731   Positive_1-9 HV01-17" ,                              
  Positive_2024F= "MOH 731   Positive_20-24(F)  HV01-23" ,                        
  Positive_2024M= "MOH 731   Positive_20-24(M)  HV01-22" ,                        
  Positive_25F= "MOH 731   Positive_25+(F)   HV01-25" ,                         
  Positive_25M= "MOH 731   Positive_25+(M) HV01-24" ,                           
  Tested_1014F="MOH 731  Tested_10-14(F)   HV01-03",                           
  Tested_1014M= "MOH 731  Tested_10-14 (M) HV01-02" ,                           
  Tested_1519F= "MOH 731  Tested_15-19(F)  HV01-05" ,                           
  Tested_1519M= "MOH 731  Tested_15-19 (M) HV01-04" ,                           
  Tested_19= "MOH 731  Tested_1-9 HV01-01" ,                                 
  Tested_2024F= "MOH 731  Tested_20-24(F)  HV01-07" ,                           
  Tested_2024M= "MOH 731  Tested_20-24(M)   HV01-06"  ,                         
  Tested_25F= "MOH 731  Tested_25+ (F)  HV01-09",                             
  Tested_25M= "MOH 731  Tested_25+ (M)   HV01-08",                            
)

#Select only columns Needed
clean_df<-clean_df %>% select(County,SubCounty,Ward,Facility,khisId,MFLCode,ReportPeriod,tested_Under15,tested_Over15,positive_under15,
                              positive_Over15,TotalLinked,
                              NegativeTotal,PositiveTotal,TestedTotal,Positive_1014F,	Positive_1014M,	Positive_1519F,	
                              Positive_1519M,	Positive_19,	Positive_2024F,	Positive_2024M,	Positive_25F,	Positive_25M,	
                              Tested_1014F,	Tested_1014M,	Tested_1519F,	Tested_1519M,	Tested_19,	Tested_2024F,	
                              Tested_2024M,	Tested_25F,	Tested_25M)

#Replace the words County, Sub County, Ward in the columns
clean_df$County <- as.character(gsub(" County", "",clean_df$County, perl=TRUE))
clean_df$SubCounty <- as.character(gsub(" Sub County", "",clean_df$SubCounty, perl=TRUE))
clean_df$Ward <- as.character(gsub(" Ward", "",clean_df$Ward, perl=TRUE))

#Replace NULL with na in the entire dataset
clean_df[clean_df=="NULL"]<-NA

#Change all columnnames to lowercase
names(clean_df)<-tolower(names(clean_df))



#Get Year for appending in the file name
year_dataPer = paste(unique(substr(clean_df$reportperiod, 1, 4)),collapse='')
paste(year_dataPer, collapse = '')

#Generate FileName
filename=paste0("C:/Users/Admin/Documents/HTSWork.csv")


#Write to csv
write_csv(clean_df,"KHISHTS.csv")

#Clear environment
#rm(list = ls())

