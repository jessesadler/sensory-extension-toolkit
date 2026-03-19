# Shiny app with bslib

library(shiny)
library(tidyverse)
library(bayesrules)
library(bslib)

sidebar_selectors <- sidebar(
  numericInput("N", 
               label = "Number of participants",
               value = 0, min = 0),
  numericInput("X",
               label = "Number of participants who thought they were different",
               value = 0, min = 0),
  selectInput("prior", "Your expectation:",
              choices = prior_tbl$certainty,
              width = "100%"),
  actionButton("simulate", "Simulate!")
)

sidebar_plot <- layout_sidebar(
  sidebar = sidebar_selectors,
  textOutput("reportText"),
  plotOutput("reportPlot")
)

sidebar_distribution <- layout_sidebar(
  sidebar = sidebar_selectors,
  plotOutput("bayesPlot")
)

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Sensory toolkit",
  
  nav_panel("So you want to run a sensory test",
            includeMarkdown("intro.md")),
  nav_panel("Recommended test procedure",
            includeMarkdown("tetrad.md")),
  nav_panel("Data analysis",
            includeMarkdown("analysis.md")),
  nav_panel("Let's try it out",
            navset_card_tab(
             nav_panel("Plot",
                       sidebar_plot),
             nav_panel("Distribution plot",
                       sidebar_distribution)
             )
            )
  )

## This works but sidebar is reset for each tab

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
