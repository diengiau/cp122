# ASK: Ok, now let's do a new Shiny app. Imagine we have "mtcars" dataframe in R environment already. I want to build a Shiny app that allows users to pick two variables from "mtcars". Then, let's Shiny app to run the regression and draw a scatter plot between two variables chosen by the user. Then, the Shiny app will show the regression results and the scatter plot to the output panel in "ui". 


library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Scatter Plot with Regression Line"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X Variable:", choices = names(mtcars)),
      selectInput("yvar", "Y Variable:", choices = names(mtcars)),
      actionButton("runRegression", "Run Regression")
    ),
    mainPanel(
      plotOutput("scatterPlot"),
      verbatimTextOutput("regOutput")
    )
  )
)


# Define server logic
server <- function(input, output) {
  # Perform regression analysis and generate scatter plot
  observeEvent(input$runRegression, {
    x <- mtcars[[input$xvar]]
    y <- mtcars[[input$yvar]]
    
    lm_model <- lm(y ~ x, data = mtcars)
    
    output$scatterPlot <- renderPlot({
      plot(x, y, main = "Scatter Plot with Regression Line",
           xlab = input$xvar, ylab = input$yvar)
      
      abline(lm_model, col = "red")
    })
    
    output$regOutput <- renderPrint({
      summary(lm_model)
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

