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
      numericInput(inputId = "pos", label = "Positives:", value = 1),

      numericInput(inputId = "samp", label = "Total # of units analyzed:", value = 10)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( 
        (textOutput(outputId = "text")),
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

  output$plot1 <- renderPlot({

    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos 

    n<-100000 #arbitrary
    
    a<-density(rbeta(n, pos, neg))
    maximum<-max(a$y)
    plot(a, xlim=c(0,1), ylim=c(0, maximum), main = "Beta distribution", xlab="Possible point estimates", ylab="Probability density")

  }, height=700, width=700)
  
  output$plot2 <- renderPlot({
    
    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos
    
    n<-100000 #arbitrary
    
    a<-rbeta(n, pos, neg)
    
    boxplot(a, ylim=c(0,1), ylab="Possible point estimates")

      }, height=700, width=700)
  
  output$text <- renderText({
    
    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos
    
    n<-100000 #arbitrary
    
    a<-rbeta(n, pos, neg)
    #95% confidence interval (arbitrary, can be any conf interval...)
    c95<-round(quantile(a,probs=c(.025,.975)), digits=3) #calculate 95% confidence interval
    paste ("95% confidence range for point estimates: ",min(c95), " to ",max(c95))

  })
    
}

shinyApp(ui = ui, server = server)
