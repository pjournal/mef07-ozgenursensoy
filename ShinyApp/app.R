library(shiny)


ui <- fluidPage(
  titlePanel("Air Quality Explorer"),
  sidebarLayout(
    sidebarPanel(
    
      selectInput("month", "Select Month:", choices = unique(airquality$Month)),
      sliderInput("wind", "Wind Speed Filter:", min = 0, max = 20, value = 10)
    ),
    mainPanel(

      plotOutput("scatterplot")
    )
  )
)


server <- function(input, output) {
  

  filtered_data <- reactive({
    month <- input$month
    wind <- input$wind
    filtered <- airquality[airquality$Month == month & airquality$Wind > wind, ]
    return(filtered)
  })
  
  
  output$scatterplot <- renderPlot({
    data <- filtered_data()
    plot(data$Wind, data$Ozone, xlab = "Wind Speed", ylab = "Ozone Level",
         main = "Wind Speed and Ozone Level", pch = 19)
  })
}


shinyApp(ui, server)
