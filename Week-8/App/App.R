library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Rocks, Pressure and Cars"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "pick one ONLY!!!:",
                  choices = c("rock", "pressure", "CARS")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "number of data you want to see:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      img(src = "rstudio.png", height = 140, width = 400),
      br(),
      img(src = "rock.png", height = 200, width = 400),
      img(src = "pressure.gif", height = 200, width = 200),
      img(src = "car.gif", height = 200, width = 300),
      br(),
      br(),
      p("Here is a summary of some data", style = "font-family: 'Comic Sans MS'"),
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      p("Here is the",
        span("data that you have requested", style = "color:blue"),
        "for, enjoy!"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)