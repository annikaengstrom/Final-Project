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

# Define UI for application that draws a bar graph
ui <- fluidPage(theme = shinytheme("sandstone"), 
                
                h1("Opinions on gay rights across the American population"),
                h5("The graph below allows you to see differences in views about gay rights issues by social demographic, including age, marital status, sex and income."),
                br(),
                br(),
                # Sidebar with an input selection option for the axes
                sidebarLayout(
                  sidebarPanel(
                   # I chose these demographic details because they gave the most interesting data 
                    selectInput(inputId = "x", 
                                label = "Choose the characteristic by which you would like dissect the population:",
                                choices = c("Marital" = "MARITAL", 
                                            "Age" = "AGE", 
                                            "Sex" = "SEX", 
                                            "Income" = "INCOME"), 
                                selected = "AGE"),
                    
                    # Select variable for y axis. I have included the four questions included in the GSS dataset. I gave them brief topic names for the selection input, rather than full questions, as these will be evident upon clicking.
                    selectInput(inputId = "y", 
                                label = "Choose the topic you want to explore within gay rights:",
                                choices = c("Right to marry" = "MARHOMO", 
                                            "Right to public speach" = "SPKHOMO", 
                                            "Right to teach" = "COLHOMO", 
                                            "Books held in libraries" = "LIBHOMO"), 
                                selected = "MARHOMO")
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
# I created separate panels for the graph and some background information / discussion of results. I wanted people not to have a crowded first panel and so I decided to separate it this way
# rather than by having a reactive paragraph below. 
# The plot label text ouput is created below, and was originally intended for the ggtitle but I decided it was more aesthetically pleasing to place it here as a tab title.                    
                     tabsetPanel(type = "tabs",
                                tabPanel(title = "Plot",
                                         br(),
                                         h4(textOutput(outputId = "plot_label")),
                                         br(),
                                         plotOutput(outputId = "barplot"),
                                         br()),
                                tabPanel(title = "Summary by Age",
                                         br(),
                                         h4("Age:"),
                                         h5("There is an extremely clear trend when it comes to looking at the effect of age on views on gay rights. The younger the respondent, the more liberal and accepting the views become. When it comes to the question of same-sex marriage, the responses transition from averaging positive to averaging negative at roughly age 45, which means these people would have been born in the early 1970’s. 
                                          "),
                                          h5("The 70’s in the United States had many important moments in gay history, ignited by the Stonewall Riots in New York City in 1969, both in politics with the first openly homosexual man elected to public office and in pop culture, with celebrities like Andy Warhol and Freddie Mercury “coming out” during this decade.
                                            ")),
                                tabPanel(title = "Summary by Marital Status",
                                         br(),
                                         h4("Marital Status:"),
                                         h5("Perhaps obviously, those who have never been married are more likely to be in favor of all types of gay rights. This is most likely because homosexuals themselves are most likely to fall into this category, with same-sex marriage only legalized nationally by a series of Supreme Court rulings in mid-2015. Furthermore, younger people, who we also see are more pro-gay rights, are likely to fall into this category.  
                                          "),
                                         h5("Conversely, the least socially liberal category, when it comes to gay rights is, overwhelmingly, the widowed population. However, I would assume that this is due to the average age of this population. Those who are married is the category which follows closely behind widows and widowers. One could attribute this to the connection between religious piety and views on the sanctity of marriage, as well as average marriage rates in rural communities, with historically greater levels of social conservativism.  
                                            ")),
                                tabPanel(title = "Summary by Sex",
                                         br(),
                                         h4("Sex:"),
                                         h5("On all issues apart from same-sex marriage, the approval rates on these gay rights issues are fairly even across the sexes. Female respondents did report slightly higher rates of agreement, but not noticeably so.
                                            "),
                                         h5("When it comes to views on same-sex marriage, however, female respondents averaged a positive response, whereas male respondents averaged a negative response. It has been suggested that reasons for this could include a certain relatability between women’s rights issues and gay rights issues, which both hinge upon ending discrimination based on biological factors. 
                                            ")),
                                tabPanel(title = "Summary by Income",
                                         br(),
                                         h4("Income:"),
                                         h5("There is only a very weak positive relationship to be seen between income and support for gay rights issues. This is most often attributed to higher levels of education, which most often leads to greater social liberalism. 
                                            "),
                                         h5("Interestingly, those of the lowest income bracket, <$1000 per year, never has the lowest acceptance rating. I would, however, attribute this to the fact that young people who are students and are not yet earning an income are included in this bracket. This means that a highly educated, young population is pulling this average down.  
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

#  
    
  output$plot_label <- reactive({
    req(input$y)
    if(input$y == "MARHOMO"){
      plot_label <- "Statement: Same sex marriage should be legal"
    } else if(input$y == "SPKHOMO"){
      plot_label <- "Statement: Homosexuals should have the right to public speach"
    } else if(input$y == "COLHOMO"){
      plot_label <- "Statement: Homosexuals should have the right to teach in colleges and universities"
    } else if(input$y == "LIBHOMO"){
      plot_label <- "Statement: Books written by homosexual authors should be kept in public libraries, rather than removed"
    }})
# This output is a barplot that changes the axis labels based on the inputs for x and y as shown above.
# I ensured that the y axis goes all the way from -1 to +1 in order to make the average levels of acceptance clear.
  
  output$barplot <- renderPlot({
    ggplot(data=gss1, aes_string(x = input$x, y = input$y)) +
      expand_limits(y=c(-1,1)) +
# By creating a statistical sumary which included the mean, I could show the mean for each reactive column.     
      stat_summary(fun.y = "mean", geom = "bar", fill = "#E7FDFF", color = "#ACACAC") +
      labs(x = x_label(), y = "Average response") +
# Here I decided to add a label to the positions on the y axis which correspond to varying degrees of agreement with the statement.
# It allows a more visual representation of the data and people can see what the average response was per demographic. 
# I wanted a faint font so that it would not visually interfere with the graph too much but still be clear.    
# Had I not included this, I fear that the numeric measurement would not be immediately clear.    
      geom_hline(yintercept=0, colour="#ACACAC", linetype = "dashed") +
      geom_hline(yintercept=1, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=1.05,size=4,label=c('  STRONGLY AGREE'), hjust = 0, colour="#999999") +
      geom_hline(yintercept=-1, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=0.55,size=4,label=c('  SOMEWHAT AGREE'), hjust = 0, colour="#999999") +
      geom_hline(yintercept=0.5, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=-0.45,size=4,label=c('  SOMEWHAT DISAGREE'), hjust = 0, colour="#999999") +
      geom_hline(yintercept=-0.5, colour="#ACACAC", linetype = "dashed") +
      annotate("text",x=0.3, y=-0.95,size=4,label=c('  STRONGLY DISAGREE'), hjust = 0, colour="#999999") +
      theme(plot.title = element_text(color="black", size=20, lineheight = 5),
            axis.title.x = element_text(color="black", size=12, face="bold"),
            axis.title.y = element_text(color="black", size=12, face="bold"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)