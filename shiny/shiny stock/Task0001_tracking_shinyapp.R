library(shiny)
#library(ggplot2)

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Tracking Data"),
  
  h3("The indicators"),

  fluidRow(
    column(width = 6, offset = 3,class = "well",
           #h4("Brush and double-click to zoom"),
           tableOutput("table")
    )
    
  ),
 
  hr(),
  
  fluidRow(
    column(width = 5, offset = 1,class = "well",
           #h4("Brush and double-click to zoom"),
           p(strong('Tip1'),' be careful'),
           p(strong('Tip2'),' be cautious'),
           p(strong('Tip3'),' beware of the risk'),
           p(strong('Tip4'),' to share')
    )
    
  ),

#  verbatimTextOutput("advice") ,

  hr(),
  
  h3("The Plot"),
  
  plotlyOutput("plot"),
  
  hr()
  
)


#-----------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  #input$date4
  source('E:/orion_star/Task_0001_R/Task0001_tracking.R')
  
  #dt<-data.table::data.table(label=c("df","tb","df"),num=c(1:3),random=runif(n=3))
  #dt<-results
  output$table=renderTable(results,digits=8)  
  #output$plot=renderPlot(ggplot2::ggplot(cars,aes(x=speed,y=dist))+geom_point())
  output$plot=renderPlotly(p  )
  #output$summary <- renderText({ c("You have selected this","--",Sys.Date(),"--",input$date4)})
  #  output$sharpe <- renderPrint(results)
  #  output$alpha <- renderPrint(alpha)
  advice=paste(":-:")
   output$advice <- renderPrint(advice)
}

# Run the application da
shinyApp(ui = ui, server = server)




