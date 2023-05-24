# ASK: Ok, now let's do a new Shiny app. Imagine we have "mtcars" dataframe in R environment already. I want to build a Shiny app that allows users to pick two variables from "mtcars". Then, let's Shiny app to draw a scatter plot between two variables and add a regression abline to the plot.

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Variable 1", choices = names(mtcars)),
      selectInput("variable2", "Variable 2", choices = names(mtcars))
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$variable1, y = input$variable2)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(x = input$variable1, y = input$variable2) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)


# ASK: Good. But I want to have a button with the label "Draw Plot" that will trigger the plot only after I click the button

library(shiny)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Variable 1", choices = names(mtcars)),
      selectInput("variable2", "Variable 2", choices = names(mtcars)),
      actionButton("drawPlotButton", "Draw Plot")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  plotData <- eventReactive(input$drawPlotButton, {
    data.frame(
      x = mtcars[[input$variable1]],
      y = mtcars[[input$variable2]]
    )
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(plotData(), aes(x, y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(x = input$variable1, y = input$variable2) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

