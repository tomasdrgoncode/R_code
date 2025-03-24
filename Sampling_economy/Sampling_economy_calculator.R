#shiny app

library(shiny) 

ui <- fluidPage(
  titlePanel("Sampling calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "servings", label = "Number of servings in the article", value = 1000),
      sliderInput(inputId = "prevalence", label = "Anticipated portion of adulterated servings", value = 0.1, min = 0, max = 1),
      sliderInput(inputId = "sensitivity", label = "Sensitivity of detection method", value = 1, min = 0, max = 1),
      numericInput(inputId = "samplecost", label = "Cost of sampling and analysis ($ per sample)", value = 100),
      numericInput(inputId = "aecost", label = "Cost of adverse event ($ per event)", value = 1000),
      sliderInput(inputId = "penetrance", label = "Likelihood that adulterated servings will trigger an adverse event", value = 1, min = 0, max = 1)
    ),

      mainPanel(
      textOutput(outputId="text"),      
      plotOutput(outputId="plot")
    )
  )
)

server <- function(input, output){
  
  output$plot<-renderPlot({
    sampling<-seq(0:10000) #sampling schedules to model
    confid<-(-(1-input$prevalence*input$sensitivity)^sampling)+1 #conficence of binomial sampling given prevalence, sensitivity and # of samples
    result<-(1-confid) #convert to probability of missing adulteration
    
    tcostSA<-sampling*input$samplecost # total cost of sample analysis
    tcostAE<-result*input$servings*input$prevalence*input$penetrance*input$aecost #total cost of all adverse events
    ptcostAE<-1*input$servings*input$prevalence*input$penetrance*input$aecost #potential AE $ if we don't sample at all
    tcostall<-tcostSA+tcostAE #ttotal cost of sample analysis and all adverse events
    
    data1<-data.frame(sampling,tcostall,tcostSA, tcostAE)
    data1<-subset(data1, data1$tcostall==min(data1$tcostall))
    optimal<-data1$sampling
    optAE<-data1$tcostAE #AE cost at optimal sampling
    optSA<-data1$tcostSA #SA cost at optimal sampling
    
    coordinates<-data1
        
    plot(sampling, tcostSA, ylim=c(0,coordinates$tcostall*4), xlim=c(0,coordinates$sampling*2), type="l", xlab="Servings", ylab="Total cost ($)" )
    par(new=TRUE)
    plot(sampling, tcostAE, ylim=c(0,coordinates$tcostall*4), xlim=c(0,coordinates$sampling*2), type="l", xlab="Servings", ylab="Total cost ($)" )
    par(new=TRUE)
    plot(sampling, tcostall, ylim=c(0,coordinates$tcostall*4), xlim=c(0,coordinates$sampling*2), type="l", xlab="Servings", ylab="Total cost ($)", col="red" )
    
  }, height=600, width=700)
  
  output$text<-renderText({
    options(scipen = 999) #disable scientific notation of numbers for display
    sampling<-seq(0:10000) #sampling schedules to model
    confid<-(-(1-input$prevalence*input$sensitivity)^sampling)+1 #conficence of binomial sampling given prevalence, sensitivity and # of samples
    result<-(1-confid) #convert to probability of missing adulteration
    
    tcostSA<-sampling*input$samplecost # total cost of sample analysis
    tcostAE<-result*input$servings*input$prevalence*input$penetrance*input$aecost #total cost of all adverse events
    ptcostAE<-1*input$servings*input$prevalence*input$penetrance*input$aecost #potential AE $ if we don't sample at all
    tcostall<-tcostSA+tcostAE #ttotal cost of sample analysis and all adverse events
    
    data1<-data.frame(sampling,tcostall,tcostSA, tcostAE)
    data1<-subset(data1, data1$tcostall==min(data1$tcostall))
    optimal<-data1$sampling
    optAE<-round(data1$tcostAE, digits=0) #AE cost at optimal sampling
    optSA<-data1$tcostSA #SA cost at optimal sampling
    
    paste("Economically optimized sampling strategy:", optimal, " servings. Sampling cost: $",optSA, ". Public health impact: $", optAE,". Public health impact if no sampling performed: $", format(ptcostAE, big.mark=","))
  })
}

shinyApp(ui = ui, server = server)

