library(shiny)

# draws a beta distribution plot based on rbeta() and alpha and beta representing hits and misses, modified by the total number of servings in the article, and penetration of adulterant in precipitation of adverse events. Clarify # of samples...----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Distribution of Point Estimates of Public Health Cost ($)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: numerical inputs ----
      numericInput(inputId = "pos", label = "Positives:", value = 1), #number of positive servings (with adulterant)

      numericInput(inputId = "samp", label = "Total # of servings analyzed:", value = 10), #number of servings analyzed
      
      numericInput(inputId = "n", label = "Total # of servings in the article:", value = 10000), #total number of servings in the article - for total # of adverse events calculation
      
      numericInput(inputId = "adverseeventvalue", label = "Adverse event mitigation $ value:", value = 1500), # public health dollars used to mitigate a single adverse event
      
      sliderInput(inputId = "AEpenetration", label = "Probability of adverse event due to single exposure (serving):", 0, 1, value = 0.01) #likelihood of a single exposure (serving) in generation of an adverse event
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel( 
        (textOutput(outputId = "text")), #
      tabsetPanel(
        tabPanel("density", plotOutput(outputId = "plot1")), # density plot of frequencies of point estimates
        tabPanel("boxplot", plotOutput(outputId = "plot2")) # box plot of the distribution of point estimates
      )
    )
  )
)

# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  # plot of beta distribution
  # 

  output$plot1 <- renderPlot({

    pos<-input$pos # nunmber of positive servings
    samp<-input$samp # number of servings analysed
    neg<-samp - pos # number of negative servings (out of analysed servings)
    n<-input$n #total number of servings
    adverseeventvalue<-input$adverseeventvalue # public health dollars per adverse event 
    penetration<-input$AEpenetration #likelihood of a single exposure generating an adverse event
    nn<-10000 #arbitrary number of trials for the generation of the distribution
    
    a<-density((rbeta(nn, pos, neg) * n * adverseeventvalue*penetration)) #beta distribution of dollar outcomes
    maximum<-max(a$y) #maximum for plot scaling
    
    options(scipen=999) #turn off scientific number format
    
    plot(a, ylim=c(0, maximum), main = "$ risk profile", xlab="Possible $ value point estimates", ylab="Probability density") #density plot of dollars of public health cost point estimates
    

  }, height=700, width=1000)
  
  output$plot2 <- renderPlot({
# annotation as above
    
    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos
    n<-input$n
    adverseeventvalue<-input$adverseeventvalue
    penetration<-input$AEpenetration
        
    nn<-10000 #arbitrary number of trials
    
    a<-(rbeta(nn, pos, neg) * n * adverseeventvalue*penetration)
    
    boxplot(a, ylim=c(0,max(a)), ylab="Possible $ value point estimates") #box plot of dollar estimates

      }, height=700, width=700)
  
  output$text <- renderText({
    
#annotation as above    
    
    pos<-input$pos
    samp<-input$samp
    neg<-samp - pos
    n<-input$n
    adverseeventvalue<-input$adverseeventvalue
    penetration<-input$AEpenetration
    
    nn<-10000 #arbitrary number of trials
    
    a<-(rbeta(nn, pos, neg) * n * adverseeventvalue*penetration)

    #95% confidence interval (arbitrary, can be any conf interval...)
    c95<-round(quantile(a,probs=c(.025,.975)), digits=3) #calculate 95% confidence interval
    paste ("95% confidence range for $ value point estimate: $",min(c95), " to $",max(c95))

  })
    
}

shinyApp(ui = ui, server = server)

