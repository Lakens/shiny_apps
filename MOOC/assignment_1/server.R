library(shiny)
library(pwr)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    sims <- input$nSims
    M    <- input$M
    SD   <- input$s
    a    <- input$alpha
    p    <- numeric(sims) #set up empty variable to store all simulated p-values
    bars <- input$bars
    n    <- input$n
    #Run simulation
    for(i in 1:sims){ #for each simulated experiment
      x<-rnorm(n = n, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
      z<-t.test(x, mu=100,conf.level = 1 - a) #perform the t-test against mu (set to value you want to test against)
      p[i]<-z$p.value #get the p-value and store it
    }
    
    #Check power by summing significant p-values and dividing by number of simulations
    x <- sum((p < a)/sims) #power
    #Calculate power formally by power analysis
    power<-pwr.t.test(d=(M-100)/SD, n=n,sig.level= a,type="one.sample",alternative="two.sided")$power 
    #determines M when power > 0. When power = 0, will set  M = 100.
    z <- input$zoom
    x_max <- 1
    y_max <- sims
    labels  <- seq(0,1,0.1)
    if( z ){
      x_max<-0.05
      y_max<-sims/5
      labels <- seq(0,1,0.01)
    }
    g <- ggplot(data = as.data.frame(p),aes(p))
    g <- g + geom_histogram(binwidth=1/bars,fill=rgb(1, 0, 0,0.5),col='black',boundary=0)
    g <- g + xlab('p-values') + ylab('Frequency of p-values')
    g <- g + coord_cartesian(xlim=c(0,x_max),ylim=c(0,y_max)) + theme_minimal(base_size = 18)
    g <- g + ggtitle(paste("P-value distribution for ",format(sims, nsmall=0)," one-sample two-sided t-tests with ",format(power*100, digits=3),"% power.",sep=""))
    g <- g + geom_hline(aes(yintercept = (0.05/a)*sims/bars, alpha = "Type 1 error rate", label = ""),lwd = 1.2, show.legend = T) + theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank(), legend.position = "top") 
    
    output$text1    <- renderText(
      paste('The power we get from the simulations is',toString(100*x),'%,the theoritical power is written in 
the title of the plot.',sep = ' ')
    )
    g

    
  })
  
})
