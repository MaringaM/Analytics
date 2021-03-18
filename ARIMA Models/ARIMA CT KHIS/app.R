library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(DT)
library(MLmetrics)
library(tidyr)

#Read the data

forecasts <- readRDS('./county_ct_forecasts.rds')

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(
        title="Kenya TXCURR Forecasts"
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
            box(plotlyOutput("txcurr_plot"),
                title = "TXCURR",
                status = "primary",
                solidHeader = TRUE,
                width = 6))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    dat_prep <- reactive({

        dat <- forecasts[[which(names(forecasts) == input$county)]]
        low_mape <-min(dat$stlf_arima_mape, dat$stlf_naive_mape, dat$stlf_ets_mape)
        mod_low_mape <- which(sapply(dat[c(4,6,8)], function(x) x == low_mape))
        dat_forecasts <- dat[[which(names(dat) == names(mod_low_mape)) - 1]]

        dates <- data.frame(Date = seq(as.Date("2016/01/01"), as.Date("2021/12/01"), by = "month"))

        forecast <- cbind(Date = seq(as.Date("2020/01/01"), as.Date("2021/12/01"), by = "month"),
                          as.data.frame(dat_forecasts))

       actuals <- cbind(Date = seq(as.Date("2016/01/01"), as.Date("2020/12/01"), by = "month"),
                       num_txcurr = dat$num_txcurr)
        num_txcurr <- cbind(Date = seq(as.Date("2016/01/01"), as.Date("2020/12/01"), by = "month"),
                           num_txcurr = dat$num_txcurr)

        combined <- merge(dates, forecast, by = "Date", all.x = TRUE) %>%
           merge(., actuals, by = "Date", all.x = TRUE) #%>%
       #     merge(., num_txcurr, by = "Date", all.x = TRUE)

        combined

    })

# Define server logic required to draw a histogram
    output$txcurr_plot <- renderPlotly({

        dat_plot <- dat_prep() %>% ggplot(aes(x = Date, y = num_txcurr)) +
            geom_line() +
            ggtitle("Number of TXCurr") +
            xlab("Month of TXCurr") +
            ylab("Number of TXCURR") +
            theme(legend.position = c(.9, .9),
                  legend.title = element_blank())


    })

}

# Run the application
shinyApp(ui = ui, server = server)
