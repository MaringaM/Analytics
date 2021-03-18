setwd("D:/R Work/Arima CT MM")
library(forecast)
library(arima)
library(astsa)
library(vars)
library(dplyr)
library(stringr)

#Import the data
ct_1617 <- read.csv('./CT_Old_20162017.csv', stringsAsFactors = FALSE) %>%
  select(county, reportperiod, txcurr)
ct_1819_old <- read.csv('./CT_Old_20182019.csv', stringsAsFactors = FALSE) %>%
  select(facility,county, reportperiod, txcurr)
ct_1819_old$facility <- str_replace_all(ct_1819_old$facility, regex("\\W+"), " ")

ct_1819_new <- read.csv('./CT_New_20182019.csv', stringsAsFactors = FALSE) %>%
  select(facility,county, reportperiod, txcurr)
ct_1819_new$facility <- str_replace_all(ct_1819_new$facility, regex("\\W+"), " ")

ct_2021<- read.csv('./CT_New_20202021.csv', stringsAsFactors = FALSE) %>%
  select(county, reportperiod, txcurr)

#Merge the 2 datasets on County and Report Month Year
ct_1819 <- merge(ct_1819_old, ct_1819_new, by = c("facility", "reportperiod"), all = TRUE)

#Check data in both old 20182019 and New20182019
count(ct_1819 %>% filter(txcurr.x==txcurr.y))

#Keep the Newest set of data
ct_1819 <- ct_1819 %>%
  mutate(txcurr = ifelse(!is.na(txcurr.x), txcurr.x, txcurr.y),
         county = ifelse(!is.na(county.y), county.y, county.x)) %>%
  select(county, reportperiod, txcurr)

# Combine the 2016,2017,2018,2019,2020,2021 TXCUrr Data
ct_all <- rbind(ct_1617, ct_1819, ct_2021)
ct_all$Month <- as.numeric(substr(ct_all$reportperiod, 5, 6))
ct_all$Year <- substr(ct_all$reportperiod, 1, 4)
ct_all$county <- gsub(" ", "", ct_all$county)

#Get the TXCurr for each county by Month and Year
ct_sum <- ct_all %>%
  group_by(county, Year, Month) %>%
  summarize(mon_txcurr = sum(txcurr, na.rm = T))

# Create Shell - A blank df containing the counties, month and years
shell <- expand.grid(Year = 2016:2020, Month = 1:12, county = unique(ct_all$county), stringsAsFactors = FALSE)

#Merge the empty shell with the df containing txcurr sums for the counties.
#Only return where txcurr.x is true remember ct_sums has 2021 data which we haven't loaded
#(all.x eliminates it)
shell <- merge(shell, ct_sum, by = c("county", "Year", "Month"), all = TRUE)

#Replace all na with 0 (I had replaced in the API Call where summation is happening)
shell[is.na(shell)] <- 0

#Pick unique values for the counties
counties <- unique(shell$county)

forecasts_out <- list()

for(i in counties){

  print(i)
  dat <- shell %>% filter(county == i) %>% arrange(Year, Month)


# Arima model
  training <- ts(dat[1:48, 4], start = c(2016, 1), frequency = 12)
  validation <- ts(dat[49:nrow(dat), 4], start = c(2020, 1), frequency = 12)
  arima_optimal = auto.arima(training, approximation = FALSE, lambda = "auto")
  arima_forecast = forecast(arima_optimal, h = length(validation)*2)
  arima_mape <- Metrics::mape(validation, arima_forecast$mean[1:length(validation)])

# STL Models
stlf_naive_forecast <- stlf(training, method = "naive")
stlf_naive_mape <- Metrics::mape(validation, stlf_naive_forecast$mean[1:length(validation)])

stlf_ets_forecast <- stlf(training, method = "ets")
stlf_ets_mape <- Metrics::mape(validation, stlf_ets_forecast$mean[1:length(validation)])

stlf_arima_forecast <- stlf(training, method = "arima")
stlf_arima_mape <- Metrics::mape(validation, stlf_arima_forecast$mean[1:length(validation)])

forecasts_out[[i]] <- list('arima_forecast' = data.frame(arima_forecast),
                           'arima_mape' = arima_mape,
                           'stlf_naive_forecast' = data.frame(stlf_naive_forecast),
                           'stlf_naive_mape' = stlf_naive_mape,
                           'stlf_ets_forecast' = data.frame(stlf_ets_forecast),
                           'stlf_ets_mape' = stlf_ets_mape,
                           'stlf_arima_forecast' = data.frame(stlf_arima_forecast),
                           'stlf_arima_mape' = stlf_arima_mape,
                           'num_txcurr' = dat$mon_txcurr)

forecasts_out
}

saveRDS(forecasts_out, './county_ct_forecasts.rds')
