library(shiny)
library(callr)

# functions

long_job <- function() {
  Sys.sleep(10)
  return(TRUE)
}

# modules
# 1. sync

sync_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Run long job:"),
      actionButton(ns("start"), "synchronously"),
      textOutput(ns("did_it_work"))
    )
  )
}

sync_srv <- function(input, output, session) {
  long_run <- eventReactive(input$start, {
    long_job()
    return("Sync job completed")
  })

  output$did_it_work <- renderText({
    long_run()
  })
}

# 2. async background

background_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      h4("Run long job:"),
      actionButton(ns("start"), "in background"),
      textOutput(ns("did_it_work"))
    )
  )
}

background_srv <-
  function(input, output, session) {
    long_run <- eventReactive(input$start, {
      x <- r_bg(
        func = long_job,
        supervise = TRUE
      )
      return(x)
    })

    check <- reactive({
      if (long_run()$is_alive()) {
        invalidateLater(millis = 1000, session = session)

        x <- "Job running in background"
      } else {
        x <- "Async job in background completed"
      }
      return(x)
    })

    output$did_it_work <- renderText({
      check()
    })
  }


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      tags$hr(),
      sync_ui("sync"),
      tags$hr(),
      background_ui("bg")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  callModule(sync_srv, id = "sync")
  callModule(background_srv, id = "bg")
}

# Run the application
shinyApp(ui = ui, server = server)
