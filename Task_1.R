library(shiny)
library(leaflet)

my_data <- as.data.frame(read.csv("C:/Users/ajjadhav/Downloads/Vaishnavi/barrosa_valley_data.csv"))
my_data$X = NULL
ui <- fluidPage(
  titlePanel("Airbnb Properties Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose Options to filter the data"),
      
      selectInput("var", 
                  label = "Choose a Proprty Type",
                  choices = c("Entire home/apt",
                              "Hotel room",
                              "Private room"),
                  selected = "Entire home/apt"),
      
      sliderInput("range", 
                  label = "Select Price range ( $ ):",
                  min = min(my_data$price), max = max(my_data$price), value = c(275, 1000),sep = "",)
    ),
    
    mainPanel(
      leafletOutput('distPlot'),
      tableOutput("DataTable")
    )
  )
)
server <- function(input, output) {
  
  output$DataTable <- renderTable({
    dt <- my_data[my_data$price >= input$range[1] & my_data$price <= input$range[2],]
    dt = dt[dt$room_type==input$var,]
    head(dt,3)
  },include.rownames=FALSE)
  
  
  output$distPlot <- renderLeaflet({
    
    library(leaflet)
    dt <- my_data[my_data$price >= input$range[1] & my_data$price <= input$range[2],]
    dt = dt[dt$room_type==input$var,]
    
    pal <- colorNumeric(palette = "YlOrRd",
                        domain = dt$price)
    
    p = leaflet(data = dt) %>%
      addTiles() %>%
      addCircleMarkers(lat = ~latitude, lng = ~longitude, popup = ~name, 
                       color = ~pal(price), stroke = FALSE, fillOpacity = 0.6) %>%
      addLegend(position = "bottomleft", pal = pal, values = ~price)
    
    p
  })
  
  
}
shinyApp(ui, server)