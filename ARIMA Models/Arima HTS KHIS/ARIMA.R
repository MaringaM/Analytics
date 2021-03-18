setwd("D:/R Work/Arima Models")
library(forecast)
library(astsa)
library(vars)
library(dplyr)
library(stringr)


hts_1617 <- read.csv('./HTSOld2016_2017.csv', stringsAsFactors = FALSE) %>%
  rename('periodcode' = Periodcode) %>%
  select(County, periodcode, Positive, Tested)

hts_1819_old <- read.csv('./HTSOld2018_2019.csv', stringsAsFactors = FALSE) %>%
  rename('periodcode' = Periodcode) %>%
  select(County, periodcode, Positive, Tested, FacilityName)
hts_1819_old$FacilityName <- str_replace_all(hts_1819_old$FacilityName, regex("\\W+"), " ")

hts_1819_new <- read.csv('./HTS2018_2019.csv', stringsAsFactors = FALSE) %>%
  rename('FacilityName' = Facility.Name) %>%
  select(County, periodcode, Positive, Tested, FacilityName)
hts_1819_new$FacilityName <- str_replace_all(hts_1819_new$FacilityName, regex("\\W+"), " ")

hts_2021 <- read.csv('./HTS2020_2021.csv', stringsAsFactors = FALSE) %>%
  rename('FacilityName' = Facility.Name) %>%
  select(County, periodcode, Positive, Tested)


# 6,831 observations appear in both datasets, but the values are the same, so just take one
hts_1819 <- merge(hts_1819_old, hts_1819_new, by = c("FacilityName", "periodcode"), all = TRUE)
hts_1819 <- hts_1819 %>%
  mutate(Positive = ifelse(!is.na(Positive.y), Positive.y, Positive.x),
         Tested = ifelse(!is.na(Tested.y), Tested.y, Tested.x),
         County = ifelse(!is.na(County.y), County.y, County.x)) %>%
  select(County, periodcode, Positive, Tested)

hts_all <- rbind(hts_1617, hts_1819, hts_2021)
hts_all$Month <- as.numeric(substr(hts_all$periodcode, 5, 6))
hts_all$Year <- substr(hts_all$periodcode, 1, 4)
hts_all$County <- gsub(" ", "", hts_all$County)
hts_all$County <- gsub("County", "", hts_all$County)

hts_sum <- hts_all %>%
  group_by(County, Year, Month) %>%
  summarize(mon_pos = sum(Positive, na.rm = T),
            mon_test = sum(Tested, na.rm = T))

# Create Shell
shell <- expand.grid(Year = 2016:2020, Month = 1:12, County = unique(hts_all$County), stringsAsFactors = FALSE)

shell <- merge(shell, hts_sum, by = c("County", "Year", "Month"), all.x = TRUE)
shell[is.na(shell)] <- 0

counties <- unique(shell$County)
forecasts_out <- list()

for(i in counties){

  print(i)
  dat <- shell %>% filter(County == i) %>% arrange(Year, Month)

  # dat_ts <- apply(dat[, 4:5], 2, function(x) ts(x, start = c(2016, 1), frequency = 12))

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


  # VAR Model
  training <- ts(dat[1:48, 4:5], start = c(2016, 1), frequency = 12)
  validation <- ts(dat[49:nrow(dat), 4], start = c(2020, 1), frequency = 12)
  var_optimal <- VAR(training, ic = "AIC")
  var_forecast <- predict(var_optimal, n.ahead=length(validation)*2)$fcst$mon_pos
  var_mape <- Metrics::mape(validation, var_forecast[1:length(validation), 1])
  var_forecast <- data.frame(var_forecast) %>%
    rename('Hi.95' = upper,
           'Lo.95' = lower,
           'Point.Forecast' = fcst)

  forecasts_out[[i]] <- list('arima_forecast' = data.frame(arima_forecast),
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

  forecasts_out

}

saveRDS(forecasts_out, './county_hts_forecasts.rds')

