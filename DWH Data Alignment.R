library(readxl)
library(tidyverse)
library(writexl)

Data_Alignment <- read_excel("C:/Users/Mi/Desktop/Data Alignment.xlsx")
Data_Alignment$Date_Uploaded<-as.numeric(Data_Alignment$Date_Uploaded)

piv<-Data_Alignment %>%
  pivot_wider(names_from = StartDate, values_from = c(StartedArt,Active))

piv<-piv %>%
  rename(
   TXNew_Sept2020= `StartedArt_2020-09-01`,
    TXNew_Oct2020= `StartedArt_2020-10-01`,
   TXNew_Nov2020=`StartedArt_2020-11-01`,
   TXCurr_Sept2020= `Active_2020-09-01`,
     TXCurr_Oct2020 =`Active_2020-10-01`,
   TXCurr_Nov2020 =`Active_2020-11-01`
  )

piv$Date_Uploaded=as.Date(piv$Date_Uploaded, origin = "1899-12-30")

write_xlsx(x = piv, path = "C:/Users/Mi/Desktop/data_Alignment_SepToNov.xlsx", col_names = TRUE)
