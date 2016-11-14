
library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Logistic stochastic and deterministic model"),
  
  # Sidebar with a slider input for the number of bins
  sidebarPanel(
#    actionButton("go", "Simulate!"),
      tags$h3("Simulation parameters "),
      sliderInput("tt",
                  "Simulation steps:",
                  min = 10,
                  max = 1000,
                  value = 50),
      numericInput("nsims", "Number of simulations:",5,min=1,max=10),
      numericInput("rseed", "Random number seed   :",333,min=1,max=1000),
      
      tags$h3("Model parameters "),
      
      numericInput("N0","Initial population   :",1,min=1,max=1000),
      numericInput("r" ,"Growth rate (r)      :",2,min=0,max=100),
      numericInput("s" ,"Death rate slope (s) :",0.2,min=0,max=100),
      numericInput("a" ,"Amplitude of oscillation of r :",0,min=0,max=100),
      numericInput("T" ,"Period of oscillation of r",1,min=0,max=100),
      
     tags$h3("Model info "),
     textOutput("K"),
     textOutput("omega")
     
),
    
    
  # Show a plot of the generated distribution
  mainPanel(
      plotOutput("modelPlot"),
      tableOutput("datatable")
      
  )
  
))


