library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)
library(MLmetrics)
library(tidyr)

forecasts <- readRDS('./county_hts_forecasts.rds')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(
        title="Kenya HTS Forecasts"
    ),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(
        selectInput("county",
                    "Select a County:",
                    choices = names(forecasts))
    ),

    # Show a plot of the generated distribution
    dashboardBody(
        fluidRow(
            box(plotlyOutput("hts_forecast_plot"),
                title = "HTS Positive Forecasts",
                status = "primary",
                solidHeader = TRUE,
                width = 6),
            box(plotlyOutput("hts_plot"),
                title = "HTS Number of Tests",
                status = "primary",
                solidHeader = TRUE,
                width = 6))
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    dat_prep <- reactive({

        dat <- forecasts[[which(names(forecasts) == input$county)]]
        # low_mape <-min(dat$arima_mape, dat$var_mape, dat$stlf_naive_mape, dat$stlf_ets_mape, dat$stlf_arima_mape)
        # mod_low_mape <- which(sapply(dat[c(2,4,6,8,10)], function(x) x == low_mape))
        low_mape <-min(dat$stlf_arima_mape, dat$stlf_naive_mape, dat$stlf_ets_mape)
        mod_low_mape <- which(sapply(dat[c(4,6,8)], function(x) x == low_mape))
        dat_forecasts <- dat[[which(names(dat) == names(mod_low_mape)) - 1]]

        dates <- data.frame(Date = seq(as.Date("2016/01/01"), as.Date("2021/12/01"), by = "month"))

        forecast <- cbind(Date = seq(as.Date("2020/01/01"), as.Date("2021/12/01"), by = "month"),
                          as.data.frame(dat_forecasts))

        actuals <- cbind(Date = seq(as.Date("2016/01/01"), as.Date("2020/12/01"), by = "month"),
                         num_pos = dat$num_pos)

        num_tests <- cbind(Date = seq(as.Date("2016/01/01"), as.Date("2020/12/01"), by = "month"),
                           num_tests = dat$num_tests)

        combined <- merge(dates, forecast, by = "Date", all.x = TRUE) %>%
            merge(., actuals, by = "Date", all.x = TRUE) %>%
            merge(., num_tests, by = "Date", all.x = TRUE)

        combined

    })

    output$hts_forecast_plot <- renderPlotly({
        # generate bins based on input$bins from ui.R

        dat_long <- pivot_longer(dat_prep(), cols = c("Point.Forecast", "num_pos"))

        dat_plot <- dat_long %>% ggplot(aes(x = Date, y = value, color = name)) +
            geom_line() +
            geom_ribbon(aes(ymin = Lo.95, ymax = Hi.95),
                        fill = "grey70",
                        alpha = 0.5,
                        color = NA) +
            ggtitle("Twelve-Month Forecast of Positive HIV Tests") +
            xlab("Month of Testing") +
            ylab("Forecast vs Actuals") +
            theme(legend.position = c(.9, .9),
                  legend.title = element_blank())
        ggplotly(dat_plot)

    })

    output$hts_plot <- renderPlotly({

        dat_plot <- dat_prep() %>% ggplot(aes(x = Date, y = num_tests)) +
            geom_line() +
            ggtitle("Number of HIV Tests") +
            xlab("Month of Testing") +
            ylab("Number of Tests") +
            theme(legend.position = c(.9, .9),
                  legend.title = element_blank())


    })

}

# Run the application
shinyApp(ui = ui, server = server)
