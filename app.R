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
library(shinythemes)
library(plotly)
gss1 <- readRDS("data1.rds")

# Define UI for application that draws a scatterplot
ui <- fluidPage(theme = shinytheme("sandstone"),
                
                h1("Sentiments towards homosexuals of different societal niches"),
                h5("The graph below allows you to choose between different variables and see which demographics of people have different views about gay rights issues."),
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
                                            "Income" = "INCOME",
                                            "Year" = "YEAR"), 
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
                                         plotOutput(outputId = "barplot")),
                                tabPanel(title = "Summary",
                                         h5("In this analysis, I have concluded that young age increases the liklihood that someone will strongly agree that homosexuals should have the right to marry. It is also worth noting that in sentiments between Strongly Agree and Strongly Disagree, the effect of age is not as visible.
                                            "))
                                         )
                    )
                  )
                )

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  y_label <- reactive({
    req(input$y)
    if(input$y == "MARITAL"){
      y_label <- "Marital Status"
    } else if(input$y == "AGE"){
      y_label <- "Age"
    } else if(input$y == "SEX"){
      y_label <- "Sex"
    } else if(input$y == "INCOME"){
      y_label <- "Income"
    } else if(input$y == "YEAR"){
      y_label <- "Year"
    }})
  
  x_label <- reactive({
    req(input$x)
    if(input$x == "MARHOMO"){
      x_label <- "Should homosexual marriage be legal?"
    } else if(input$x == "SPKHOMO"){
      x_label <- "Should homosexuals have the right to public speach?"
    } else if(input$x == "COLHOMO"){
      x_label <- "Should homosexuals have the right to teach a college or university?"
    } else if(input$x == "LIBHOMO"){
      x_label <- "Should books written by homosexual authors be removed from public libraries?"
    }})
  
  output$barplot <- renderPlot({
    ggplot(data=gss1, aes_string(x = input$x, y = input$y)) +
      stat_summary(fun.y = "mean", geom = "bar") +
      labs(x = x_label(), y = y_label()) +
      ggtitle("Homosexuality Sentiment Scores")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)