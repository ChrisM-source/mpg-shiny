#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# 1.0 Libraries

library(shiny)
library(bslib)
library(plotly)
library(tidyverse)


# 2.0 load data set

data("mpg")
help("mpg")
str(mpg)
summary(mpg)

# 3.0 Define UI
ui <- fluidPage(
  titlePanel("MPG Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis Variable:",
                  choices = names(mpg)[sapply(mpg, is.numeric)],
                  selected = "cty"),
      selectInput("yvar", "Y-axis Variable:",
                  choices = names(mpg)[sapply(mpg, is.numeric)],
                  selected = "hwy"),
    
      
      checkboxInput("show_lm", "Add regression line", FALSE),
      checkboxInput("show_data", "Show data table", FALSE),
      checkboxInput("show_summary", "Show summary statistics", FALSE)
    ),
    
    mainPanel(
      plotlyOutput("scatterplot"),
      conditionalPanel(
        "input.show_data == true",
        tableOutput("data_table")
      ),
      conditionalPanel(
        "input.show_summary == true",
        verbatimTextOutput("summary_stats")
        
    )
  )
)
)

# 4.0 Define server logic
server <- function(input, output) {
  output$scatterplot <- renderPlotly({
    req(input$xvar, input$yvar)
    
    p <- ggplot(mpg, aes(x = .data[[input$xvar]], 
                         y = .data[[input$yvar]])) +
      geom_point(size = 3, alpha = 1/2, color ="blue") +
      labs(title = "Car Fuel Economy Analysis",
           x = input$xvar,
           y = input$yvar) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"),
            panel.border = element_rect(color = "black", fill = NA, size = 1.4))
    
    if (input$show_lm) {
      p <- p + geom_smooth(method = "lm", color = "red")
    }
    
    ggplotly(p)
    
  })
  
  output$data_table <- renderTable({
    req(input$xvar, input$yvar)
    mpg[, c(input$xvar, input$yvar)]
  })
  output$summary_stats <- renderPrint({
    req(input$xvar, input$yvar)
    summary(mpg[, c(input$xvar, input$yvar)])
  })
}

# 5.0 Run the app
shinyApp(ui = ui, server = server)
