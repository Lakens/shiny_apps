library(shiny)
library(TOSTER)

ui <- fluidPage(
  
  # Application title
  titlePanel("Plot P-Values for the TOST procedure and Second Generation P-Value"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(numericInput("N", "Participants per group:", 30, min = 1, max = 1000),
                 sliderInput("sd", "Standard Deviation:", min = 0.5, max = 5, value = 2, step= 0.5),
                 sliderInput("range", "Equivalence Range:", min = -5, max = 5, value = c(-2,2), step= 0.1),              
                 sliderInput("mu", "Mean To Test Against:", min = 140, max = 150, value = 145, step= 0.5),
                 sliderInput("alpha", "Alpha level:", min = 0.01, max = 0.10, value = 0.05, step= 0.01),
                 h4("Created by Daniel Lakens and Marie Delacre. For code and the accompanying manuscript, see ", a("GitHub", href="https://github.com/Lakens/TOST_vs_SGPV")),
                 br()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      h4(textOutput("text1")),
      plotOutput("plot2"),
      h4(textOutput("text2"))
      
    )
  )
)