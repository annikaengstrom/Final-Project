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
                
                h1("Sentiments towards homosexuals by different societal niches"),
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
  
  x_label <- reactive({
    req(input$x)
    if(input$x == "MARITAL"){
      x_label <- "Marital Status"
    } else if(input$x == "AGE"){
      x_label <- "Age"
    } else if(input$x == "SEX"){
      x_label <- "Sex"
    } else if(input$x == "INCOME"){
      x_label <- "Income"
    } else if(input$x == "YEAR"){
      x_label <- "Year"
    }})
  
  plot_label <- reactive({
    req(input$y)
    if(input$y == "MARHOMO"){
      plot_label <- "Same sex marriage should be legal"
    } else if(input$y == "SPKHOMO"){
      plot_label <- "Homosexuals should have the right to public speach"
    } else if(input$y == "COLHOMO"){
      plot_label <- "Homosexuals should have the right to teach a college or university"
    } else if(input$y == "LIBHOMO"){
      plot_label <- "Books written by homosexual authors should be kept in public libraries, rather than removed"
    }})

  
  output$barplot <- renderPlot({
    ggplot(data=gss1, aes_string(x = input$x, y = input$y)) +
      expand_limits(y=c(-1,1)) +
      stat_summary(fun.y = "mean", geom = "bar", fill = "#E7FDFF", color = "#ACACAC") +
      labs(x = x_label(), y = "Average response") +
      geom_hline(yintercept=0, colour="#ACACAC", linetype = "dashed") +
      geom_hline(yintercept=1, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=1.05,size=4,label=c('  STRONGLY AGREE'), hjust = 0, colour="#999999") +
      geom_hline(yintercept=-1, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=0.55,size=4,label=c('  SOMEWHAT AGREE'), hjust = 0, colour="#999999") +
      geom_hline(yintercept=0.5, colour="#ACACAC", linetype = "dashed") +
      geom_hline(yintercept=-0.5, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=-0.45,size=4,label=c('  SOMEWHAT DISAGREE'), hjust = 0, colour="#999999") +
      annotate("text",x=0.3, y=-0.95,size=4,label=c('  STRONGLY DISAGREE'), hjust = 0, colour="#999999") +
      ggtitle(plot_label()) +
      theme(plot.title = element_text(color="black", size=14, face="bold", lineheight = 5),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)