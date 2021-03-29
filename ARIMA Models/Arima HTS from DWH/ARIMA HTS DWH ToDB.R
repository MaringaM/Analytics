library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)
library(MLmetrics)
library(tidyr)
setwd("C:/Users/Admin/OneDrive/Analytics/ARIMA Models/Arima HTS from DWH")


forecasts <- readRDS('C:/Users/Admin/OneDrive/R Work/Arima Models/county_hts_forecasts_ndwh.rds')
countys <- names(forecasts)

#Initialize List and DataFrame
forecasts_kiherehere <- list()
df <- data.frame()

#Loop through the counties to get the best model and the forecasts, low&high,tests and pos for each
for(i in countys){
  print(i)
dat <- forecasts[[which(names(forecasts) == i)]]
# low_mape <-min(dat$arima_mape, dat$var_mape, dat$stlf_naive_mape, dat$stlf_ets_mape, dat$stlf_arima_mape)
# mod_low_mape <- which(sapply(dat[c(2,4,6,8,10)], function(x) x == low_mape))
low_mape <-min(dat$stlf_arima_mape, dat$stlf_naive_mape, dat$stlf_ets_mape)
mod_low_mape <- which(sapply(dat[c(4,6,8)], function(x) x == low_mape))
dat_forecasts <- dat[[which(names(dat) == names(mod_low_mape)) - 1]]

#Rename the columns to replace . with _
dat_forecasts <- data.frame(dat_forecasts) %>%
  rename('Hi_95' = Hi.95,
         'Lo_95' = Lo.95,
         'Hi_80' = Hi.80,
         'Lo_80' = Lo.80,
         'Point_Forecast' = Point.Forecast)

print(low_mape)
dates <- data.frame(Date = seq(as.Date("2018/01/01"), as.Date("2021/06/01"), by = "month"))

forecast <- cbind(Date = seq(as.Date("2020/07/01"), as.Date("2021/06/01"), by = "month"),
                  as.data.frame(dat_forecasts))

actuals <- cbind(Date = seq(as.Date("2018/01/01"), as.Date("2020/12/01"), by = "month"),
                 num_pos = dat$num_pos)

num_tests <- cbind(Date = seq(as.Date("2018/01/01"), as.Date("2020/12/01"), by = "month"),
                   num_tests = dat$num_tests)

combined <- merge(dates, forecast, by = "Date", all.x = TRUE) %>%
  merge(., actuals, by = "Date", all.x = TRUE) %>%
  merge(., num_tests, by = "Date", all.x = TRUE)

forecasts_kiherehere[[i]]<-combined
df<- rbind(df,forecasts_kiherehere[[i]]%>% mutate(county=i) )
}
#Write to csv
#write_csv(df,"Arima_dwh_output.csv")
saveRDS(df, file = "arimahtsdwh.rds")







# 
# 
# dat_long <- pivot_longer(df, cols = c("Point_Forecast", "num_pos"))
# 
# dat_plot <- dat_long %>%filter(county=='bungoma') %>% ggplot(aes(x = Date, y = value, color = name)) +
#   geom_line() +
#   scale_x_date(date_labels = "%Y %b") +
#   geom_ribbon(aes(ymin = Lo_95, ymax = Hi_95),
#               fill = "grey70",
#               alpha = 0.5,
#               color = NA) +
#   ggtitle("Twelve-Month Forecast of Positive HIV Tests") +
#   xlab("Month of Testing") +
#   ylab("Forecast vs Actuals") +
#   theme(legend.position = c(.9, .9),
#         legend.title = element_blank())
# 
# ggplotly(dat_plot)



