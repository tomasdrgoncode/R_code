library(shiny)

# draws a gamma distribution plot based on shape parameters
ui <- fluidPage(
  
  # App title ----
  titlePanel("Gamma distribution"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "shape",label = "Shape:", step=0.01, min = 0, max = 4,
                  value = 2),

      sliderInput(inputId = "rate",
                  label = "Rate:", step=0.01, min = 0, max = 1,
                  value = 2)

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        textOutput(outputId="text"),
        plotOutput(outputId = "plot1")
    )
  )
)


# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  # plot of gamma distribution
  # 
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$plot1 <- renderPlot({

    shape<-input$shape
    rate<-input$rate
    
    n<-10000 #arbitrary
    
    dataA<-(rgamma(n, shape, rate))
    
    hist(dataA, freq=FALSE, breaks=50 ,xlim=c(0,max(dataA)))
    
    
  }, height=700, width=700)

  output$text<-renderText({
    options(scipen = 999) #disable scientific notation of numbers for display
    
    shape<-input$shape
    rate<-input$rate
    
    n<-10000 #arbitrary
    
    dataA<-(rgamma(n, shape, rate))
    model_mean<-round(mean(dataA), digits=2)
    model_sd<-round(sd(dataA), digits=2)
    paste("Mean:", model_mean, ", SD: ",model_sd, ".")

  })
  
}

shinyApp(ui = ui, server = server)




