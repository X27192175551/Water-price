ui <- fluidPage(
  
  # App title ----
  titlePanel("Water Price"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput(
        "vari","year",
        c("2015"="yr2015",
          "2016"="yr2016",
          "2017"="yr2017",
          "2018"="yr2018",
          "2019"="yr2019")
      ),
      numericInput("prbase", 
                   label = "Numeric input", 
                   value = 1.92, step=0.01),
    
      sliderInput(inputId = "ratio1",
                  label = "Number of ratio1:",
                  min = 1.00,
                  max = 2.00,
                  value = 1.72
                 ),
      sliderInput(inputId = "ratio2",
                  label = "Number of ratio2:",
                  min = 2.00,
                  max = 3.00,
                  value = 2.24
      ),
      sliderInput(inputId = "voln1",
                  label = "Number of voln1:",
                  min = 150,
                  max = 220,
                  value = 220),
      sliderInput(inputId = "voln2",
                  label = "Number of voln2:",
                  min = 220,
                  max = 300,
                  value = 300)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
 tabsetPanel(
   tabPanel("plot",plotOutput(outputId = "distPlot"),
    tabPanel("percent",tableOutput("percent"))
  )
 )
   

      
      # Output: Histogram ----
      
    )
  )
)
