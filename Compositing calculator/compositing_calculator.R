library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Compositing calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "LOC",
                   label = "Limit of concern:",
                   value = 20),
      numericInput(inputId = "LOD",
                   label = "Limit of detection:",
                   value = 1),
      numericInput(inputId = "SD",
                   label = "Standard deviation:",
                   value = 1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: text ----
      textOutput("result")
      
    )
  )
)

# Define server logic required to compute result ----
server <- function(input, output) {

  
  #the max number of composits to prevent dillution of a violative sub-sample with too many clean sub-samples
  output$result<-renderText({paste("Max number of subs: ", floor((input$LOC/(input$LOD +(2*input$SD)))))})
  
  
}

shinyApp(ui = ui, server = server)