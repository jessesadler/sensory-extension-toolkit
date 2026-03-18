# Test shiny app

library(shiny)
library(tidyverse)
library(bayesrules)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tabsetPanel(
      tabPanel("So you want to run a sensory test",
                  includeMarkdown("intro.md")
                  ),
      tabPanel("Recommended test procedure",
                  includeMarkdown("tetrad.md")
                  ),
      tabPanel("Data analysis",
                  includeMarkdown("analysis.md")
                  ),
      tabPanel("Let's try it out",
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                    sidebarPanel(
                      numericInput("N", 
                                   label = "Number of participants",
                                   value = 0, min = 0),
                      numericInput("X",
                                   label = "Number of participants who thought they were different",
                                   value = 0, min = 0),
                      selectInput("prior", "Your expectation:",
                                  choices = prior_tbl$certainty,
                                  width = "100%"
                      ),
                      actionButton("simulate", "Simulate!")
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      textOutput("reportText"),
                      plotOutput("reportPlot")
                    )
                  )
      ),
      tabPanel("Distribution",
               # Sidebar with a slider input for number of bins 
               sidebarLayout(
                 sidebarPanel(
                   numericInput("N", 
                                label = "Number of participants",
                                value = 0, min = 0),
                   numericInput("X",
                                label = "Number of participants who thought they were different",
                                value = 0, min = 0),
                   selectInput("prior", "Your expectation:",
                               choices = prior_tbl$certainty,
                               width = "100%"
                   ),
                   actionButton("simulate", "Simulate!")
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   textOutput("ab"),
                   plotOutput("bayesPlot")
                 )
               )
      )
    )
)

# Define server logic
server <- function(input, output) {
  
  priors <- reactive(prior_ab(prior_tbl, input$prior))
  
  output$ab <- renderText(
    paste("Alpha is currently:", priors()[[1]], "and",
          "Beta is currently:", priors()[[2]]))
  
  

  output$bayesPlot <- renderPlot({
    bayesrules::plot_beta_binomial(alpha = priors()[[1]], beta = priors()[[2]], y = input$X, n = input$N)
  })
  
  N <- eventReactive(input$simulate, input$N)
  X <- eventReactive(input$simulate, input$X)
  
  
  output$reportText <- renderText(
    report_txt(N(), X())
  )
  
  output$reportPlot <- renderPlot({
    report_plt(N(), X())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
