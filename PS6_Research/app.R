library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
data <- read.csv("UAH-lower-troposphere-long.csv.bz2", sep="\t")
data$date <- ymd(paste0(data$year, "-", data$month, "-01"))

ui <- navbarPage("Problem Set 6",
                 tabPanel("About",
                          p("In this assignment, we will use the UAH lower troposhphere 
                            temperature data set to analysis the trend of temperature by year."),
                          p("The data set contains ", strong("14310"), " rows and ", strong("4"), " columns."),
                          p("The variables show as below:"),
                          tags$ul(
                            tags$li("Year"),
                            tags$li("Month"),
                            tags$li("Region"),
                            tags$li("Temp")
                          ),
                          plotlyOutput("p1")         
                 ),
                 tabPanel("Plot",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              selectInput("region", "Select the region", choices = unique(data$region),
                                          selected = "globe"),
                              checkboxInput("trend", "Add Trend Line")
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: Histogram ----
                              plotlyOutput(outputId = "p2"),
                              verbatimTextOutput("t")
                            )
                          )
                 ),
                 tabPanel("Table",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              em("Select the year, then calculate the summary statistic for the temperaure by region"),
                              sliderInput("year","Select the year", min=1978, max=2023, value=2000, sep="")
                              
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              verbatimTextOutput("t2"),
                              tableOutput("table")
                            )
                          )    
                 )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$p1 <- renderPlotly({
    data %>% filter(region=="globe") %>%
      ggplot(aes(x=date, y=temp)) + geom_line(color="red") +
      labs(x="Date", y="Lower troposhphere temperature", 
           title="Time trend of lower troposhphere temperature")
  })
  output$p2 <- renderPlotly({
    g <- data %>%
      filter(region==input$region) %>%
      ggplot(aes(x=date, y=temp)) + geom_line() +
      labs(x="Date", y="Lower troposhphere temperature", 
           title="Time trend of lower troposhphere temperature")
    if(input$trend){
      g <- g + geom_smooth(method="lm")
    }
    g
  })
  
  output$t <- renderText({
    df <- data %>%
      filter(region==input$region)
    paste0("The selected region is ", input$region, ", the mean temperature during this period is ", round(mean(df$temp), 2))
  })
  
  output$table <- renderTable({
    data %>%
      filter(year==input$year) %>%
      group_by(region) %>%
      summarise(mean=mean(temp), median=median(temp), min=min(temp), max=max(temp),
                                     sd=sd(temp), n=n())
  })
  
  output$t2 <- renderText({
    data %>% 
      filter(year==input$year) 
    paste0("The selected year is ", input$year, ", the number of observations is ", nrow(df))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)