# ASK: Ok, imagine now we have a "mtcars" dataframe in R. I want to build a Shiny app that allows users to pick two variables from "mtcars" and run a regression. Then, the Shiny app will show the results to the output panel in "ui". 

library(shiny)
data("mtcars")
ui <- fluidPage(
  titlePanel("Regression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Variable 1", choices = names(mtcars)),
      selectInput("variable2", "Variable 2", choices = names(mtcars)),
      actionButton("runRegression", "Run Regression")
    ),
    mainPanel(
      verbatimTextOutput("regressionResult")
    )
  )
)
# run immediately
server <- function(input, output) {
  regressionModel <- reactive({
    lm(as.formula(paste(input$variable1, "~", input$variable2)), data = mtcars)
  })
  
  output$regressionResult <- renderPrint({
    summary(regressionModel())
  })
}

shinyApp(ui = ui, server = server)

# allows user to push the button
# ASK: can we allows the app to wait a bit: just run the regression after the user pushed the button

server <- function(input, output) {
  regressionModel <- eventReactive(input$runRegression, {
    req(input$variable1, input$variable2)
    lm(as.formula(paste(input$variable1, "~", input$variable2)), data = mtcars)
  })
  
  output$regressionResult <- renderPrint({
    req(input$runRegression)
    summary(regressionModel())
  })
}


