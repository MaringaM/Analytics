library(dplyr)
# Read in the data
dwh<-read.csv('C:/Users/Admin/OneDrive/Not GIT Stuff/DWHAnalytics2.csv')
#Change all column names to lowercase
names(dwh)<-tolower(names(dwh))

#Check the structure of the DB
str(dwh)

#Convert the dates to date format
datecols<-colnames(dwh[,grep("date", colnames(dwh))])
dwh[,datecols]<-lapply(dwh[,datecols],as.Date,format = "%m/%d/%Y")

rm(datecols)

txvar<-colnames(dwh[,grep("txcurr", colnames(dwh))])
#Convert the current ART Columns from Character to Numeric
dwh[,txvar]<-lapply(dwh[,txvar],as.numeric)

rm(txvar)
#Convert NA to 0
dwh[is.na(dwh)] <- 0

#1. check for Facilities whose TXCurr has Changed

dwh <- dwh%>% mutate(status=ifelse(prev_txcurr < cur_txcurr,"increased txcurr",
                            ifelse(prev_txcurr>cur_txcurr,"reduced txcurr",
                                   ifelse(prev_txcurr==cur_txcurr,"same txcurr","aiiii"))))

#2. Check For Sites whose upload Date Changed
dwh <- dwh%>% mutate(uploadstatus=ifelse(prev_uploaddate < cur_uploaddate,"new upload",
                                   ifelse(prev_uploaddate>cur_uploaddate,"wierd",
                                          ifelse(prev_uploaddate==cur_uploaddate,"same upload","aiiii"))))

dwh<-dwh %>% mutate(diff=cur_txcurr-prev_txcurr)


#3. Category of Issues - WHo is to blame
dwh <- dwh%>% mutate(blamer=ifelse(dwh$uploadstatus=='new upload' & dwh$status=='reduced txcurr','Facility ReUpload Data',
                                   ifelse(dwh$uploadstatus=='new upload' & dwh$status=='increased txcurr','Expected Increase',
                                          ifelse(dwh$uploadstatus=='new upload' & dwh$status=='same txcurr','To Flag Ama',
                                         ifelse((dwh$uploadstatus=='same upload' & dwh$status=='increased txcurr'),"ETL Issues?",
                                                ifelse(dwh$uploadstatus=='same upload' & dwh$status=='reduced txcurr',"Expected Drop",
                                                       'Expected Same?'))))))






#Same Upload - ETL Issues
si<-dwh %>% select(facilitycode,facilityname,prev_uploaddate,prev_siteabstractiondate,prev_txcurr, cur_uploaddate,cur_siteabstractiondate,cur_txcurr,diff) %>% 
  filter(dwh$uploadstatus=='same upload' & dwh$status=='increased txcurr')

#New Upload and reduced TXCurr
sr<-dwh %>% select(facilitycode,ctpartner,facilityname,prev_uploaddate,prev_siteabstractiondate,prev_txcurr, cur_uploaddate,cur_siteabstractiondate,cur_txcurr,diff) %>% 
  filter(dwh$uploadstatus=='new upload' & dwh$status=='reduced txcurr')

table(dwh$status,dwh$uploadstatus)

write.csv(sr,"C:/Users/Admin/Desktop/Sites_Reupload.csv")
