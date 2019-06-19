library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Cluster analysis techniques - base"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of individuals ----
      sliderInput(inputId = "nind",
                  label = "Number of individuals:",
                  min = 11,
                  max = 1000,
                  value = 100), 
      # Input: Slider for the number of individuals ----
      sliderInput(inputId = "nspp",
                  label = "Number of species:",
                  min = 1,
                  max = 10,
                  value = 2), 
      
      # Input: Slider for mean of first cluster ----
      sliderInput(inputId = "mean1",
                  label = "Mean 1:",
                  min = 10,
                  max = 250,
                  value = 100),
      
      # Input: Slider for sd of first cluster ----
      sliderInput(inputId = "sd1",
                  label = "Sd 1:",
                  min = 0,
                  max = 100,
                  value = 10),
      
      # Input: Slider for mean of second cluster ----
      sliderInput(inputId = "mean2",
                  label = "Mean 2:",
                  min = 10,
                  max = 250,
                  value = 200),
      # Input: Slider for sd of second cluster ----
      sliderInput(inputId = "sd2",
                  label = "Sd 2:",
                  min = 0,
                  max = 100,
                  value = 30),
      radioButtons(inputId = "method",
                   label = "Species assignment method",
                   choices = list(random = 1, 
                                  ranked = 2)
      )
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "datPlot"),
      
      plotOutput(outputId = "clustPlot"),
      
      tableOutput(outputId = "speciesAssignments")
      
    ),
    position = c("left", "right"), fluid = TRUE)
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  source(here::here("functions", "utilities.R"))
  source(here::here("functions", "quantile_walk.R"))
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$datPlot <- renderPlot({
    
    dat <- make_dat(input = input)
    
    hist(dat, breaks = 50)
  }
  )
  
  output$clustPlot <- renderPlot({
    
    dat <- make_dat(input = input)
    clust <- fit_gmm(dat)
    plot(clust, what = "density")
  }
  )
  
  output$speciesAssignments <- renderTable({
    dat <- make_dat(input = input)
    community_df <- assign_species(dat = dat, input = input)
    community_df
  })
  
}


shinyApp(ui = ui, server = server)
