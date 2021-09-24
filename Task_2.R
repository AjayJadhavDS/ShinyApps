
library(shiny)
library(ggplot2)
library(dplyr)

my_data <- as.data.frame(read.csv("C:/Users/ajjadhav/Downloads/Vaishnavi/grand_slam_data.csv"))

my_data$winningCount <- as.numeric(ave(my_data$winner, my_data$winner, FUN = length))

my_data$runnerupCount <- as.numeric(ave(my_data$runner_up, my_data$runner_up, FUN = length))

library(shiny)


ui <- fluidPage(
  titlePanel("Grand Slam Tournament Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select Year and Tournament to check the statistics"),
      
      selectInput("var", 
                  label = "Choose a Tournament",
                  choices = c("Australian Open",
                              "French Open",
                              "Wimbledon",
                              "U.S. Open"),
                  selected = "French Open"),
      
      sliderInput("range", 
                  label = "Choose a start and end year:",
                  min = min(my_data$year), max = max(my_data$year), value = c(1909, 1941),sep = "",)
    ),
    
    mainPanel(
      tableOutput("DataTable"),
      plotOutput("plot")
    )
  )
)
server <- function(input, output) {
  
  output$DataTable <- renderTable({
    MSL <-my_data[my_data$year >= input$range[1] & my_data$year <= input$range[2],]
    MSL <- MSL[MSL$tournament== input$var ,]
    
    MSL <-MSL[order(MSL$year),]
    
    head(MSL,5)
    },include.rownames=FALSE)
  
  
  output$plot <- renderPlot({

    MSL <-my_data[my_data$year >= input$range[1] & my_data$year <= input$range[2],]
    MSL <- MSL[MSL$tournament== input$var ,]
    
    MSL %>% 
      arrange(desc(winningCount)) %>%
      slice(1:10) %>%
      ggplot(., aes(x=winner, y=winningCount))+
      geom_bar(stat='identity', fill = "#FF6666")
  })
  
}
shinyApp(ui, server)