library(fpp3)
library(regclass)
library(stats)

# Path where data is
file_path <- "C:\\Users\\spens\\OneDrive\\Documents\\BAS 475\\BAS475Midterm\\TheBatmanTrends\\BAS 475 MIDTERM FINAL\\BatmanSearchData.csv"

# Data starts in 3rd row, skip first 2 rows
g_trends <- read.csv(file_path, skip = 2)
# Rename columns
names(g_trends) <- c("Month", "Interest")
# Convert Month to date
g_trends$Month <- yearmonth(g_trends$Month)
# Convert to tsibble
g_trends <- tsibble(g_trends)


train <- g_trends %>%
  filter_index("2004 Jan" ~ "2022 Mar")
# Fit the models
trends_fit <- train %>%
  model(
    Mean = MEAN(Interest),
    `Naive` = NAIVE(Interest),
    `Seasonal Naive` = SNAIVE(Interest),
    Drift = NAIVE(Interest ~ drift())
  )
# Generate forecasts for 14 quarters
trend_fc <- trends_fit %>% forecast(h = 14)

# Google labels low numbers as "<1"
# Convert those to 0s and ensure the column is numbers
g_trends$Interest <- as.numeric(
  ifelse(g_trends$Interest == "<1", 0, g_trends$Interest))


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)

introtab<- menuItem("Introduction",  tabName = "intro")
tseriestab <- menuItem("Batman Interest Time Series", tabName = "plot")
forecasttab <- menuItem("Forecast", tabName = "extra")
plotstab <- menuItem("Other Plots", tabName = "additional")
interptab <- menuItem("Interpretation", tabName = "interpret")
naivetab <- menuItem("Simple Models", tabName = "naive")
etstab <- menuItem("Exponential Smoothing", tabName = "ets")
arimatab <- menuItem("ARIMA Forecast", tabName = "arima")
sidebar <- dashboardSidebar(sidebarMenu(
  introtab,
  tseriestab,
  forecasttab,
  plotstab,
  interptab,
  naivetab,
  etstab,
  arimatab))


body <- dashboardBody(tabItems(
  tabItem( 
    tabName = "intro", h2("Hello and welcome to my app that analyzes the search trends on the term Batman. This simple app has Five different tabs that each have a 
                          different specific usage. The first page is this Introduction/Intstruction tab, tab 2 shows the time series for the search term data,
                          tab 3 is a forecast of the data, tab 4 allows you to choose from 3 different graphs to display, and tab 5 explains the results of 
                          tab 4." ))
  ,
  tabItem(
    tabName = "plot", h2("Batman Search Trends"),
    plotOutput("Plot")
  ),
  tabItem(
    tabName = "extra", h2("Batman Search Forecast"),
    plotOutput("Plot3")
    
  ),
  tabItem(
    tabName = "additional",
    radioButtons("radio", label = h3("Select Graph"),
                 choices = list("Seasonality" =1 , "Autocorrelation" = 2, "Decompsition" = 3), 
                 selected = 1),
    plotOutput("Plot2")
  ),
  tabItem(
    tabName = "interpret", 
    box(title= "Seasonality", column(width = 5), textOutput("Text1")),
    
    box(
      title = "Autocorrelation",
      textOutput("Text2")),
      box( title = "Decomposition", textOutput("Text3"))
    ),
  tabItem(
    tabName = "naive", h2("Naive, Seasonal Naive, Mean, and Drift Forecast"),
    plotOutput("Plot4")
   ),
  tabItem(
    tabName = "ets",
    radioButtons("radio1", label = h3("Select Graph"),
                 choices = list("Holts" = 1 , "Holts/Winters" = 2 ), selected = 1),
    plotOutput("Plot5")
  ),
  tabItem(
    tabName = "arima",
    radioButtons("radio2", label = h3("Select Graph"),
                 choices = list("Manual ARIMA" = 1 , "Auto ARIMA" = 2 ), selected = 1),
    plotOutput("Plot6")
  )
  
  
  )
)



# Define UI for application that draws a histogram
ui <- fluidPage(
  dashboardPage( skin = "yellow", 
    dashboardHeader(title = "Midterm Project"),
    sidebar,
    body
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    ggplot(g_trends, aes(x=Month, y=Interest)) +
      geom_line() + 
      xlab("Time") 
  })
    
    output$Plot2 <- renderPlot({
      if (input$radio == 1){
        gg_season(g_trends) 
      }
      else if (input$radio == 2) {
        ACF(g_trends) %>%
          autoplot() 
      }
      else if (input$radio ==3) {
        g_trends %>%
          model(
            classical_decomposition(Interest, type = "additive")
          ) %>%
          components() %>%
          autoplot() +
          labs(title = "Classical additive decomposition of Batman Interest")}
      
  })
    
    
    output$Plot3 <- renderPlot({
      
      fit_dcmp <- g_trends %>%
        model(stlf = decomposition_model(
          STL(Interest ~ trend(window = 7), robust = TRUE),
          NAIVE(season_adjust)
        ))
      fit_dcmp %>%
        forecast() %>%
        autoplot(g_trends)+
        labs(y = "Trend Forecast of The Batman Search terms"
        )
    })
    
    
    output$Plot4 <- renderPlot({
    
     
      trend_fc <- trends_fit %>% forecast(h = 14)
      trend_fc %>%
        autoplot(train, level = NULL) +
        autolayer(
          filter_index(g_trends, "2023 Jan" ~ .),
          colour = "black"
        ) +
      labs(
          y = "Interest",
          title = "Forecasts for Monthy Batman Searches"
        ) +
        guides(colour = guide_legend(title = "Forecast"))
      
     
        
    })
    
    
    output$Plot5 <- renderPlot({
      if (input$radio1 == 1){
      g_trends %>%
        model(
          `Holt's method` = ETS(Interest ~ error("A") +
                                  trend("A") + season("N")),
          `Damped Holt's method` = ETS(Interest ~ error("A") +
                                         trend("Ad", phi = 0.9) + season("N"))
        ) %>%
        forecast(h = 15) %>%
        autoplot(g_trends, level = NULL) +
        labs(title = "Batman Searches",
             y = "Interest") +
        guides(colour = guide_legend(title = "Forecast"))
      }
      
      else if (input$radio1 == 2) {
        fit1 <- g_trends %>%
          model(
            additive = ETS(Interest ~ error("A") + trend("A") +
                             season("A")),
            multiplicative = ETS(Interest ~ error("M") + trend("A") +
                                   season("M"))
          )
        fc1 <- fit1 %>% forecast(h = "3 years")
        
        fc1 %>%
          autoplot(g_trends, level = NULL) +
          labs(title="Holts/Winters Forecast",
               y="Interest") +
          guides(colour = guide_legend(title = "Forecast"))
      }
    })
      
      output$Plot6 <- renderPlot({
        if (input$radio2 == 1){
          fit2 <- g_trends %>%
            model(arima1 = ARIMA(Interest ~ pdq(2,1,0)),
                  arima2 = ARIMA(Interest ~pdq(0,1,3)),
                  arima3 = ARIMA(Interest ~pdq(2,0,2)),
                  arimastepwise = ARIMA(Interest),
                  searching = ARIMA(Interest, stepwise = FALSE))
          
          
          
          glance(fit2) %>% arrange(AICc)
          
          fit2 <- g_trends %>% model(ARIMA(Interest, stepwise = FALSE))
          report(fit2)
          
          fit2 %>% forecast(h=12) %>% autoplot(g_trends)
        }
        else if  (input$radio2 == 2){
          fit3 <- g_trends %>%
            model(
              auto = ARIMA(Interest, stepwise = FALSE, approx = FALSE)
            )
          forecast(fit3, h=36) %>%
            filter(.model=='auto') %>%
            autoplot(g_trends)
          
          
          
          
          
        }
           })
    
    output$Text1 <- renderText("In regards to the seasonality data, We can see that the data before 2022, the data 
                             peaked in June and July, corresponding with the releases of the three previous batman films released in 2005, 2008, and 2012. We 
                             see a small boost in February and March is likely due to the release of Batman vs. 
                             Superman in 2016. The final large jump in 2022 is from the release of The Batman in March.")
    output$Text2 <- renderText("The Autocorrelation graph shows that data is both trended and seasonal, seeing as it slopes downard as the lag increases. The overall 
                                coorelation is really good, all of the lags are significantly different from zero. The data is not overly impacted by randomness or 
                               white noise")
    output$Text3 <- renderText("The Decomposition shows that seasonality and trend do not have all that big of an impact compared to the randomness and 
                                white noise of the data.")
    output$Text4 <- renderText("The Decomposition shows that seasonality and trend do not have all that big of an impact compared to the randomness and 
                                white noise of the data.")
}

# Run the application 
shinyApp(ui = ui, server = server)
