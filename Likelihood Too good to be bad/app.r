library(shiny)
library(shinythemes)
ui <- fluidPage(theme=shinytheme("flatly"),
  titlePanel("Likelihood Ratio for Mixed Results"),
  sidebarLayout(
    sidebarPanel(numericInput("n", "Number of studies :", 3, min = 1, max = 100),
                 numericInput("x", "Number of successes:", 2, min = 0, max = 100),
                 sliderInput("H0", "Type 1 error rate:", min = 0, max = 1, value = 0.05, step= 0.01),
                 sliderInput("H1", "Assumed Power:", min = 0, max = 1, value = 0.8, step= 0.01)
                 ),
    mainPanel(plotOutput("coolplot"),br(),
              h4("Shiny app accompanying: Lakens, D., & Etz, A. J. (2017). Too true to be bad: When sets of studies with significant and non-significant findings are probably true. Social Psychological and Personality Science. https://osf.io/preprints/psyarxiv/nnkg9"),br(),
              h4(textOutput("text1")), br(),
              h4(textOutput("text2")),br(),
              h4("When the observed results are equally likely under H0 and H1, the likelihood ratio is 1. Benchmarks to interpret Likelihood Ratios suggest that when 1<LR<8 there is weak evidence, when 8<LR<32 there is moderate evidence, and when LR>32, there is strong evidence."),br(),
              h4(textOutput("text3")), br(),
              dataTableOutput("table")
    )
  )
)
server <- function(input, output) {
  output$coolplot <- renderPlot({
    n<-input$n
    x<-input$x
    H0<-input$H0
    H1<-input$H1
    LR<-round(max(dbinom(x,n,H0)/dbinom(x,n,H1),dbinom(x,n,H1)/dbinom(x,n,H0)),digits=2) #Calculate LR (H1 over H0 and H0 over H1, pick max)
    H0H1<-ifelse(dbinom(x,n,H0)>dbinom(x,n,H1),"The data are more likely under the null hypothesis than the alternative hypothesis","The data are more likely under the alternative hypothesis than the null hypothesis") 
    theta<- seq(0,1,len=1000)
    like <- dbinom(x,n,theta)
    plot(theta,like,type='l',xlab=expression(theta), ylab='Likelihood', lwd=2)
    points(H0,dbinom(x,n,H0), lwd=2)
    points(H1,dbinom(x,n,H1), lwd=2)
    segments(H0, dbinom(x,n,H0), x/n, dbinom(x,n,H0), lty=2, lwd=2)
    segments(H1, dbinom(x,n,H1), x/n, dbinom(x,n,H1), lty=2, lwd=2)
    segments(x/n, dbinom(x,n,H0), x/n, dbinom(x,n,H1), lwd=2)
    abline(v=H0, col="gray40", lty=3, lwd=2)
    abline(v=H1, col="gray40", lty=3, lwd=2)
    title(paste('Likelihood Ratio:',LR))
    
    #create Table
    studies<-c(0:n)
    ProbH0<-dbinom(studies,n,H0)
    ProbH1<-dbinom(studies,n,H1)
    table<-cbind(studies,ProbH0,ProbH1)
    colnames(table) <- c("Number of significant studies", "Probability of observing x out of n significant studies when H0 is true","Probability of observing x out of n significant studies when H1 is true")
    
    output$text1 <- renderText({ 
      paste("When H1 is true, in the long run, the probability of observing", x," out of ",n, " significant studies, given" ,H1, "power, is:", dbinom(x,n,H1),". If all studies examine true effects with",100*H1,"% power,",100*round(dbinom(x,n,H1),digits=4),"% of published articles with",n,"studies should observe",x,"significant results. When power is less than",100*round(0.5^(1/n), digits=4),"% observing",n,"out of",n,"significant results is less likely (i.e., occurs less than 50% of the time) than observing mixed results.")
    })
    output$text2 <- renderText({ 
      paste("When H0 is true, in the long run, the probability of observing", x," out of ",n, " significant studies, given a" ,H0, "Type 1 error rate, is:", round(dbinom(x,n,H0),digits=10),". When all studies examine null-effects with a",100*H0,"% Type 1 error rate,",100*round(dbinom(x,n,H0),digits=4),"% of published articles with",n,"studies should observe",x,"significant results.")
    })
    output$text3 <- renderText({ 
      paste(H0H1, "with a likelihood ratio of",LR,".")
    })
    output$table = renderDataTable(
      table,
      options = list(paging = FALSE, searching = FALSE)
    )
  })
}
shinyApp(ui = ui, server = server)