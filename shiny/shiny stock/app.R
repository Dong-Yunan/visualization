#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
setwd("D:/DataScience/Quant/Task_0001_R")
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  
sidebarLayout(position = "right",
              
                sidebarPanel(
                  # Inputs excluded for brevity
                # dateInput("date4", "Date:", value = Sys.Date()-10),
                    sidebarLayout(position = "right",
                    sidebarPanel(""),
                    mainPanel("")
                  )
                ),
                mainPanel(
                  # Outputs excluded for brevity 
                  
                  # Output: Tabset w/ plot, summary, and table ----
                  tabsetPanel(type = "tabs",
                              tabPanel("RateTrend", plotlyOutput("plot")),
                             # tabPanel("sharpe", verbatimTextOutput("sharpe")),
                             # tabPanel("alpha", verbatimTextOutput("alpha")),
                           #   tabPanel("beta", verbatimTextOutput("beta")),
                              tabPanel("DataFace", tableOutput("table"))
                              )
                )
  )
  
   )



# Define server logic required to draw a histogram
server <- function(input, output) {
    #input$date4
    source('./Task0001_tracking.R')
    
    #dt<-data.table::data.table(label=c("df","tb","df"),num=c(1:3),random=runif(n=3))
    #dt<-results
    output$table=renderTable(results)  
    #output$plot=renderPlot(ggplot2::ggplot(cars,aes(x=speed,y=dist))+geom_point())
    output$plot=renderPlotly(p  )
    #output$summary <- renderText({ c("You have selected this","--",Sys.Date(),"--",input$date4)})
  #  output$sharpe <- renderPrint(results)
  #  output$alpha <- renderPrint(alpha)
   # output$beta <- renderPrint(beta)
    }

# Run the application da
shinyApp(ui = ui, server = server)




