# ASK 1: Can you write a Shiny app in R: input is an integer number from user. Output is a random order of a vector from 1 to the number from user


library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Random Order Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("number", "Enter a Number:", value = 10, min = 1),
      actionButton("generate", "Generate Random Order")
    ),
    mainPanel(
      verbatimTextOutput("randomOrder")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Generate random order
  observeEvent(input$generate, {
    number <- input$number
    random_order <- sample(1:number)
    output$randomOrder <- renderPrint({
      random_order
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

# ASK 2:can you print the output into two rows if the number from users is larger than 10

library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Random Order Generator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("number", "Enter a Number:", value = 10, min = 1),
      actionButton("generate", "Generate Random Order")
    ),
    mainPanel(
      fluidRow(
        column(width = 6,
               verbatimTextOutput("randomOrder1")
        ),
        column(width = 6,
               verbatimTextOutput("randomOrder2")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Generate random order
  observeEvent(input$generate, {
    number <- input$number
    random_order <- sample(1:number)
    if (number <= 10) {
      output$randomOrder1 <- renderPrint({
        random_order
      })
      output$randomOrder2 <- renderPrint({
        NULL
      })
    } else {
      output$randomOrder1 <- renderPrint({
        random_order[1:(number/2)]
      })
      output$randomOrder2 <- renderPrint({
        random_order[(number/2 + 1):number]
      })
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

