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
                  value = 0),
      numericInput(inputId = "LOD",
                   label = "Limit of detection:",
                   value = 0),
      numericInput(inputId = "SD",
                   label = "Standard deviation:",
                   value = 0)
            
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
  
  
  
  output$result<-renderText({paste("Max number of subs: ", 
                                   
                                   (input$LOC/input$LOD)-1)})
  
  
}

shinyApp(ui = ui, server = server)
