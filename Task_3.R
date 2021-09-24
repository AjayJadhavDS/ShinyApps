library(dplyr)
library(shiny)


my_data = read.csv("C:/Users/ajjadhav/Downloads/Vaishnavi/vic_covid19_data.csv")
my_data$index = NULL

my_data$diagnosis_date = format(as.Date(my_data$diagnosis_date, "%d/%m/%y"), "20%y-%m-%d")


my_data = my_data %>%
  group_by(diagnosis_date,postcode,acquired,local_government_area,age_group ) %>%
  mutate(count = n())

ui <- fluidPage(
  titlePanel("Covid 19 Dashboard"),
  
  column(3, wellPanel(
    dateRangeInput('dateRange',
                   label = 'Filter by date',
                   start = as.Date('2020-07-24') , end = as.Date('2020-07-30')
    ),
    
    selectInput("var", 
                label = "Choose a age_group",
                choices = unique(my_data$age_group),
                selected = "30-39"),
  )),
  column(8,
         
         plotOutput("plot"),
         plotOutput("plot2"),
         dataTableOutput('my_table')
  )
)

server <- function(input, output, session) {
  output$my_table  <- renderDataTable({
    
    my_data <- my_data[my_data$age_group== input$var ,]
    my_data = my_data  %>% filter(diagnosis_date >= input$dateRange[1] & diagnosis_date <= input$dateRange[2])
    head(my_data,2)
  })
  
  
  output$plot <- renderPlot({
    
    my_data <- my_data[my_data$age_group== input$var ,]
    
    my_data = my_data  %>% filter(diagnosis_date >= input$dateRange[1] & diagnosis_date <= input$dateRange[2])
    
    ggplot(my_data, aes(diagnosis_date)) +
      geom_bar(aes(fill = acquired), position = "dodge", stat="count") +
      labs(x = "Date") 
      
  })
  
  
  output$plot2 <- renderPlot({
    
    my_data <- my_data[my_data$age_group== input$var ,]
    
    my_data = my_data  %>% filter(diagnosis_date >= input$dateRange[1] & diagnosis_date <= input$dateRange[2])
    
    TAB = as.data.frame(table(my_data$local_government_area))
    TAB = TAB[order(TAB$Freq,decreasing = T),]
    TAB = TAB[1:10,]
    ggplot(TAB, aes(x=Var1, y=Freq)) +
      geom_bar(stat="identity") +
      labs(x = "local_government_area",y ="Counts") 
  })
  
}

shinyApp(ui = ui, server = server)