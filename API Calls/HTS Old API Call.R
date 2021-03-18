library(httr)
library(jsonlite)
library(tidyverse)

#Declare From and To Dates
EndDate=as.Date("2017-12-01",origin = "1899-12-30")
StartDate=as.Date("2016-01-01",origin = "1899-12-30")
dataperiods <-c(paste0(format(seq(StartDate,EndDate,by="month"), "%Y%m"),";"))
dataperiods =noquote(dataperiods)

url="https://hiskenya.org/api/26/analytics.json?dimension=ou:LEVEL-5;HfVjCurKxh2&dimension=dx:h4pR1wz1JQb;c2pgDecBILP;NMWgSKFW6U7;QXQFd2JB10B;RCFqEXIiqed;xN6GhQGgaoI;DBZH3aC9gwQ&dimension=pe:"
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

rm(combi_url)

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
  PositiveFemale1524="Female 15-24yrs  Receiving HIV + Results ",
  PositiveFemaleAbove25="Female above 25yrs Receiving HIV + Results ",
  PositiveFemaleUnder25="Female under 15yrs Receiving HIV + Results ",
  PositiveMale1524="Male 15-24yrs  Receiving HIV + Results ",
  PositiveMaleAbove25="Male above 25yrs Receiving HIV + Results ",
  PositiveMaleUnder25="Male under 15yrs Receiving HIV + Results ",
  TestedTotal="Total Tested HIV"
)

#Select only columns Needed
clean_df<-clean_df %>% select(County,SubCounty,Ward,Facility,khisId,MFLCode,ReportPeriod,PositiveFemale1524,
                              PositiveFemaleAbove25,PositiveFemaleUnder25,PositiveMale1524,
                              PositiveMaleAbove25,PositiveMaleUnder25,TestedTotal)

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
filename=paste0("D:/R Work/Arima Models/HTS_Old_",year_dataPer,".csv")

#Write to csv
write_csv(clean_df,filename)

