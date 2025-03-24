#data

total<-226790 #total number of servings
portion<-0.10 #estimate of the portion of adulterated servings

bad<-total*portion
good<-total-bad

good
bad

a<-c(rep(0, times = good), rep(1, times = bad))# custom vector of observations


# code for plotting power curves for different sampling strategies
# for bunomial distributions 
# zero out vectors
result<-vector(mode="numeric", length=0)
b<-vector(mode="numeric", length=0)
sampling<-vector(mode="numeric", length=0)



for (j in seq(from=10, to=1000, by=5)) { # loop through a sequence of sampling strategies
      
    #zero out positive/negative counters
    negative<-0 
    positive<-0
    print (j)
    sampling<-append(sampling, j)
    
    for (i in 1:10000) { # loop through trials of each sampling strategy  
      
      b<-sample(a, (j), replace=TRUE) #sampling, make sure to set the "replacement"
      
      if (sum(b)<1) { #what is the minimum of hits for a positive?
        negative<-negative+1
      }
      
      else {
        positive<-positive+1
      } 
      
    }
    
    result<-append(result, (negative/(positive+negative))) 
  }
  
  tcostSA<-sampling*1500
  tcostAE<-result*11339*500000
  tcostall<-tcostSA+tcostAE

plot(sampling, tcostall, type="l")

plot(sampling, tcostAE, ylim=c(0, 100000000), type="l")
par(new=TRUE)
plot(sampling, tcostSA, ylim=c(0, 100000000), type="l")
  




#code for binomial sampling


servings<-226790 #total # of servings in the article
sampling<-seq(1:10000) #sampling schedules to model
prevalence<-0.05 #prevalence of adulteration in article
sensitivity<-1 #sensitivity of detection (probability of misidentification of adulteration)
confid<-(-(1-prevalence*sensitivity)^sampling)+1 #conficence of binomial sampling given prevalence, sensitivity and # of samples
result<-(1-confid) #convert to probability ofd missing adulteration
samplecost<-1500 #cost of sampling and analysis per sample
aecost<-500000 #cost of adverse event per event
penetrance<-1 #for every adulterated serving how many adverse events actually precipitate


tcostSA<-sampling*samplecost # total cost of sample analysis
tcostAE<-result*servings*prevalence*penetrance*aecost #total cost of all adverse events
tcostall<-tcostSA+tcostAE #ttotal cost of sample analysis and all adverse events

data<-data.frame(sampling,tcostall)
data<-subset(data, data$tcostall==min(data$tcostall))
data$sampling
coordinates<-data

# full plot
plot(sampling, tcostall, type="l", xlim=c(0, coordinates$sampling*2))
par(new=TRUE)
plot(sampling, tcostall, type="l",xlim=c(0, coordinates$sampling*2),col="red")

#zoomed on the minimum
plot(sampling, tcostall, ylim=c(0,coordinates$tcostall*4), xlim=c(0,coordinates$sampling*2), type="l")
par(new=TRUE)
plot(sampling, tcostall, ylim=c(0,coordinates$tcostall*4), xlim=c(0,coordinates$sampling*2), type="l", col="red")


#standalone binomial sampling equation
confid<-0.95
result<-(1-confid)
p<-0.05 #prevalence
sampling<-log(1-confid)/log(1-p)


#shiny app

library(shiny) 
ui <- fluidPage(
  titlePanel("Sampling calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "servings", label = "Number of servings in the article", value = 1000, min = 0, max = 500000),
      sliderInput(inputId = "prevalence", label = "Anticipated portion of adulterated servings", value = 0.1, min = 0, max = 1),
      sliderInput(inputId = "sensitivity", label = "Sensitivity of detection method", value = 1, min = 0, max = 1),
      sliderInput(inputId = "samplecost", label = "Cost of sampling and analysis (per sample)", value = 1500, min = 0, max = 100000),
      sliderInput(inputId = "aecost", label = "Cost of adverse event (per event)", value = 10000, min = 0, max = 500000),
      sliderInput(inputId = "penetrance", label = "Likelihood that adulterated servings will trigger an adverse event", value = 1, min = 0, max = 1),
      sliderInput(inputId = "articleprevalence", label = "Anticipated portion of articles containing adulterated servings", value = 1, min = 0, max = 1)
      ),
    mainPanel(
      textOutput(outputId="text"),
      plotOutput(outputId="plot", width="100%")
    )
  )
)
server <- function(input, output){
  
  output$plot<-renderPlot({
    sampling<-seq(1:10000) #sampling schedules to model
    confid<-(-(1-input$prevalence*input$sensitivity)^sampling)+1 #conficence of binomial sampling given prevalence, sensitivity and # of samples
    result<-(1-confid) #convert to probability ofd missing adulteration
    
    tcostSA<-sampling*input$samplecost # total cost of sample analysis
    tcostAE<-result*input$servings*input$prevalence*input$penetrance*input$aecost*input$articleprevalence #total cost of all adverse events
    tcostall<-tcostSA+tcostAE #ttotal cost of sample analysis and all adverse events
    
    data1<-data.frame(sampling,tcostall)
    data1<-subset(data1, data1$tcostall==min(data1$tcostall))
    optimal<-data1$sampling
    coordinates<-data1
    plot(sampling, tcostall, ylim=c(0,coordinates$tcostall*4), xlim=c(0,coordinates$sampling*2), type="l", xlab="Servings", ylab="Total cost ($)" )
  }, height=600, width=700)
  
  output$text<-renderText({
    sampling<-seq(1:10000) #sampling schedules to model
    confid<-(-(1-input$prevalence*input$sensitivity)^sampling)+1 #conficence of binomial sampling given prevalence, sensitivity and # of samples
    result<-(1-confid) #convert to probability ofd missing adulteration
    
    tcostSA<-sampling*input$samplecost # total cost of sample analysis
    tcostAE<-result*input$servings*input$prevalence*input$penetrance*input$aecost*input$articleprevalence #total cost of all adverse events
    tcostall<-tcostSA+tcostAE #ttotal cost of sample analysis and all adverse events
    
    data1<-data.frame(sampling,tcostall)
    data1<-subset(data1, data1$tcostall==min(data1$tcostall))
    optimal<-data1$sampling
    
    paste("Economically optimized sampling strategy:", optimal, " servings")})
} 
shinyApp(ui = ui, server = server)





