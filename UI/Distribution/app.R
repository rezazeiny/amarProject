library("shiny")
source("distributionFunctions.R")
# Define UI ----
ui <- fluidPage(
  titlePanel("Select your distribution"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Uniform",
                           numericInput("uniIN1", "a", 0),
                           numericInput("uniIN2", "b", 100)),
                  tabPanel("Bernoulli",
                           numericInput("berIN1", "par (0,1)", 0.42)),
                  tabPanel("Binomial",
                           numericInput("binIN1", "p (0,1)", 0.3),
                           numericInput("binIN2", "n", 20)),
                  tabPanel("Geometric",
                           numericInput("geoIN1", "p (0,1)", 0.4)),
                  tabPanel("Exponential",
                           numericInput("expIN1", "landa", 3)),
                  tabPanel("Gamma",
                           numericInput("gamIN1", "landa", 3),
                           numericInput("gamIN2", "k", 20)),
                  tabPanel("Poisson",
                           numericInput("poiIN1", "landa", 3),
                           numericInput("poiIN2", "k", 20)),
                  tabPanel("Normal",
                           numericInput("norIN1", "mean", 0),
                           numericInput("norIN2", "var", 4))
      ),
      actionButton("go", "Plot"),br(),
      h3("Estimation Feature"),
      fileInput("file",
                helpText("upload your file here to stimate")),
      actionButton("estimate", "Estimate"),
      textOutput("estimate")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server <- function(input, output){
  v <- reactiveValues(doPlot = FALSE)

  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doPlot <- input$go
  })

  observeEvent(input$tabset, {
    v$doPlot <- FALSE
  })

  output$plot <- renderPlot({
    if (v$doPlot == FALSE) return()
    isolate({
      switch (input$tabset,
        "Uniform" = uniform_visualization(input$uniIN1, input$uniIN2),
        "Bernoulli" = bernoulli_visualization(input$berIN1),
        "Binomial" = binomial_visualization(input$binIN1,input$binIN2),
        "Geometric" = geometric_visualization(input$geoIN1),
        "Exponential" = exponential_visualization(input$expIN1),
        "Gamma" = gamma_visualization(input$gamIN1,input$gamIN2),
        "Poisson" = poisson_visualization(input$poiIN1,input$poiIN2),
        "Normal" = normal_visualization(input$norIN1,input$norIN2)
      )
    })
  })
  output$estimate <- renderText({
    paste("Ali akbar complete it estimate function ",input$tabset)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
