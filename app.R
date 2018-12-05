#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(readr)
gss1 <- readRDS("data1.rds")

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  h1("Sentiments towards homosexuals of different societal niches"),
  "The graph below allows you to choose between different variables and see which demographics of people have different views about gay rights issues.",
  br(),
  br(),
  # Sidebar with a slider input for axis inputs 
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Marital" = "MARITAL", 
                              "Age" = "AGE", 
                              "Sex" = "SEX", 
                              "Income" = "INCOME"), 
                  selected = "AGE"),
      
      # Select variable for y axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Right to marry" = "MARHOMO", 
                              "Right to public speach" = "SPKHOMO", 
                              "Right to teach" = "COLHOMO", 
                              "Books held in libraries" = "LIBHOMO"), 
                  selected = "MARHOMO")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel(title = "Plot",
                 plotOutput(outputId = "scatterplot")),
        tabPanel(title = "Summary",
                 h5("In this analysis, I have concluded that young age increases the liklihood that someone will strongly agree that homosexuals should have the right to marry. It is also worth noting that in sentiments between Strongly Agree and Strongly Disagree, the effect of age is not as visible.
                    "))
      )
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$scatterplot <- renderPlot({
      read_rds("data1.rds") %>% 
      ggplot(aes_string(x = input$x, y = input$y)) + 
      geom_jitter(alpha = 0.1) +
      xlab("Demographic") +
      ylab("Factor") +
      ggtitle("Homosexuality Sentiment Scores")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)