ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputID = "y",
                  label = "Y-axis:",
                  choices = c("MARHOMO", "COLHOMO", "LIBHOMO"),
                  selected = "MARHOMO"),
      selectInput(inputID = "x",
                  label = "X-axis:",
                  choices = c("YEAR", "AGE", "MARITAL", "SEX", "INCOME", "SATCITY"),
                  selected = "YEAR")
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = gss1, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)  
