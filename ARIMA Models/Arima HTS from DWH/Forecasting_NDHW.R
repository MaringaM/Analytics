#setwd("~/Kenya/Forecasting")
library(forecast) # arima and stl models
library(vars) # VAR model
library(dplyr) # Essentially enables writing SQL in R
library(stringr) # For string processing
library(lubridate)

hts <- read.csv('D:/OneDrive/R Work/Arima Models/HTS_FEB_2021.csv', stringsAsFactors = FALSE)
hts$County <- tolower(hts$County)
hts <- hts %>%
  filter(!grepl("centre|hospital|dispensary", hts$County))
hts <- hts %>%
  filter(FinalTestResult %in% c("Positive", "Negative")) %>%
  mutate(FinalTestResult = ifelse(FinalTestResult == 'Positive', 'Positive', 'Negative'))
hts$TestDate <- ymd(hts$TestDate)
hts$Month <- month(hts$TestDate)
hts$Year <- year(hts$TestDate)
hts_sum <- hts %>%
  group_by(County, Year, Month) %>%
  summarize(mon_pos = sum(FinalTestResult == "Positive"),
            mon_test = n()) %>%
  ungroup()

counties_sparse <- hts_sum %>%
  group_by(County) %>%
  summarize(pos = sum(mon_pos)) %>%
  filter(pos < 500) %>%
  .$County

hts_sum <- hts_sum %>%
  filter(!County %in% counties_sparse)

# Create Shell - will merge on shell to ensure we're not missing any months in our series
shell <- expand.grid(Year = 2018:2020,
                     Month = 1:12,
                     County = unique(hts_sum$County),
                     stringsAsFactors = FALSE)

# shell <- shell[!(shell$Year == 2021 & shell$Month > 1), ]

shell <- merge(shell, hts_sum, by = c("County", "Year", "Month"), all.x = TRUE) # left join on shell
shell[is.na(shell)] <- 0 # if no values for any month, then comes through as NA after join - set to zero

# Get vector of country names to loop through
counties <- unique(shell$County)
# Set up list to hold outputs
forecasts_out <- list()

# Loop through counties
for(i in counties){

  print(i)
  # Filter data for county and sort by year and month in chronological order
  dat <- shell %>% filter(County == i) %>% arrange(Year, Month)

  period_train <- 30
  period_validation <- 6

  # dat_ts <- apply(dat[, 4:5], 2, function(x) ts(x, start = c(2016, 1), frequency = 12))

  # Arima model
  training <- ts(dat[1:period_train, 'mon_pos'], start = c(2018, 1), frequency = 12)
  validation <- ts(dat[(period_train+1):nrow(dat), 'mon_pos'], start = c(2020, 7), frequency = 12)
  arima_optimal = auto.arima(training, approximation = FALSE, lambda = "auto")
  arima_forecast = forecast(arima_optimal, h = period_validation*2)
  arima_mape <- Metrics::mape(validation, arima_forecast$mean[1:period_validation])

  # STL Models
  stlf_naive_forecast <- stlf(training, method = "naive")
  stlf_naive_mape <- Metrics::mape(validation, stlf_naive_forecast$mean[1:period_validation])

  stlf_ets_forecast <- stlf(training, method = "ets")
  stlf_ets_mape <- Metrics::mape(validation, stlf_ets_forecast$mean[1:period_validation])

  stlf_arima_forecast <- stlf(training, method = "arima")
  stlf_arima_mape <- Metrics::mape(validation, stlf_arima_forecast$mean[1:period_validation])

  # VAR Model
  training <- ts(dat[1:period_train, 4:5], start = c(2018, 1), frequency = 12)
  validation <- ts(dat[(period_train+1):nrow(dat), 4], start = c(2020, 7), frequency = 12)
  var_optimal <- VAR(training, ic = "AIC")
  var_forecast <- predict(var_optimal, n.ahead=period_validation*2)$fcst$mon_pos
  var_mape <- Metrics::mape(validation, var_forecast[1:period_validation, 1])
  # Rename outputs so that they match models above (will be easier later if all dataframes share variable names)
  var_forecast <- data.frame(var_forecast) %>%
    rename('Hi_95' = upper,
           'Lo_95' = lower,
           'Point_Forecast' = fcst)

  # Add forecasts, actuals, and mape to out list
  forecasts_out[[i]] <- list('arima_forecast' = data.frame(arima_forecast), #convert to dataframe (from forecast object)
                             'arima_mape' = arima_mape,
                             'stlf_naive_forecast' = data.frame(stlf_naive_forecast),
                             'stlf_naive_mape' = stlf_naive_mape,
                             'stlf_ets_forecast' = data.frame(stlf_ets_forecast),
                             'stlf_ets_mape' = stlf_ets_mape,
                             'stlf_arima_forecast' = data.frame(stlf_arima_forecast),
                             'stlf_arima_mape' = stlf_arima_mape,
                             'var_forecast' = var_forecast,
                             'var_mape' = var_mape,
                             'num_pos' = dat$mon_pos,
                             'num_tests' = dat$mon_test)
}

# Save outputs
saveRDS(forecasts_out, 'D:/OneDrive/R Work/Arima Models/county_hts_forecasts_ndwh.rds')
