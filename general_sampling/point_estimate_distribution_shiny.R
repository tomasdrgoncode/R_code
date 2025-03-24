library(shiny)

# draws a beta distribution plot based on rbeta() and alpha and beta representing hits or misses. Clarify # of samples...----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Distribution of Point Estimates"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "pos",label = "Scenario A - positives:", value = 1),

      numericInput(inputId = "samp",
                  label = "Scenario A - # of units analyzed:", value = 10),

      numericInput(inputId = "posB",label = "Scenario B - positives:", value = 1),
      
      numericInput(inputId = "sampB",
                   label = "Scenario B - # of units analyzed:", value = 10)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("density", plotOutput(outputId = "plot1")),
        tabPanel("boxplot", plotOutput(outputId = "plot2"))
      
      )
    )
  )
)

# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  # plot of beta distribution
  # 
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$plot1 <- renderPlot({

    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos 
    posB<-input$posB
    sampB<-input$sampB
    negB<-sampB-posB
    
    n<-100000 #arbitrary
    
    dataA<-density(rbeta(n, pos, neg))
    dataB<-density(rbeta(n, posB, negB))
    maximum<-max(dataA$y, dataB$y)
    
    plot(dataA, xlim=c(0,1), ylim=c(0, maximum), main = "Beta distribution", xlab="% of adulterated units", ylab="probability density")
    
    lines(dataB, col="red")
    
    
  }, height=700, width=700)
  
  output$plot2 <- renderPlot({
    
    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos
    
    posB<-input$posB
    sampB<-input$sampB
    negB<-sampB-posB
    
    n<-100000 #arbitrary
    
    a<-rbeta(n, pos, neg)
    b<-rbeta(n, posB, negB)
    
    boxplot(a, b, ylim=c(0,1), names=c("scenario A","scenario B"), ylab="% of adulterated units")

      }, height=700, width=700)
  
    
}


shinyApp(ui = ui, server = server)






