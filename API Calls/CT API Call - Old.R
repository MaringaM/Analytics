library(httr)
library(jsonlite)
library(tidyverse)

#Declare From and To Dates
StartDate=as.Date("2016-01-01",origin = "1899-12-30")
EndDate=as.Date("2017-12-01",origin = "1899-12-30")

dataperiods <-c(paste0(format(seq(StartDate,EndDate,by="month"), "%Y%m"),";"))
dataperiods =noquote(dataperiods)

url="https://hiskenya.org/api/26/analytics.json?dimension=ou:LEVEL-5;HfVjCurKxh2&dimension=dx:TqhUjblmU6n;QISrDYwXnMw;cbUoZG6mlS7;oTjp04eBIaV;ISGNo6jMJlI&dimension=pe:"
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
cont_raw = GET(combi_url,authenticate(username, password))
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
  txcurr_bel1="Currently on ART - below 1 year",
  txcurr_malebelow="Currently on ART - Male below 15 years",
  txcurr_femalebelow="Currently on ART - Female Below 15 years",
  txcurr_maleabove="Currently on ART - Male above 15 years",
  txcurr_femaleabove="Currently on ART - Female above 15 years"
)


#Select only columns Needed
clean_df<-clean_df %>%  select(County,SubCounty,Ward,Facility,khisId,MFLCode,ReportPeriod,txcurr_bel1,
                              txcurr_malebelow,txcurr_femalebelow,txcurr_maleabove,
                              txcurr_femaleabove)

#Convert the current ART Columns from Character to Numeric
clean_df[8:ncol(clean_df)] <- lapply(clean_df[8:ncol(clean_df)], as.numeric)

#Replace NA with 0 in the entire dataset
clean_df[is.na(clean_df)]<-0

#Create One Column txcurr that is a sum of the txcurr sub-sets

clean_df <- clean_df %>% group_by(Facility,ReportPeriod) %>%
  mutate(txcurr=sum(txcurr_bel1,txcurr_malebelow,txcurr_femalebelow,txcurr_maleabove,txcurr_femaleabove)) %>%
  select(MFLCode,Facility,County,SubCounty,Ward,ReportPeriod,txcurr)


#Replace the words County, Sub County, Ward in the columns
clean_df$County <- as.character(gsub(" County", "",clean_df$County, perl=TRUE))
clean_df$SubCounty <- as.character(gsub(" Sub County", "",clean_df$SubCounty, perl=TRUE))
clean_df$Ward <- as.character(gsub(" Ward", "",clean_df$Ward, perl=TRUE))

#Change all columnnames to lowercase
names(clean_df)<-tolower(names(clean_df))

#Get Year for appending in the file name
year_dataPer = paste(unique(substr(clean_df$reportperiod, 1, 4)),collapse='')
paste(year_dataPer, collapse = '')

#Generate FileName
filename=paste0("D:/R Work/Arima CT MM/CT_Old_",year_dataPer,".csv")

#Write to csv
write_csv(clean_df,filename)

