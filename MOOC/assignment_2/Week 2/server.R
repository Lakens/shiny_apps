# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(latex2exp)
library(tidyr)
library(RColorBrewer)
library(gridExtra)
# Define server logic 
shinyServer(function(input, output, session) {
  withMathJax()
  #theta same for every instance
  theta <- seq(0,1,by = 0.001)
  ####### Tab 1
  ###############################frame 1
  ######Plot 
  output$distPlot1 <- renderPlot({
    n     <- input$n
    x     <- input$p
    distr <- dbinom(x,n,theta)
    g     <- ggplot(data = cbind.data.frame(theta,distr))
    g     <- g + geom_line(aes(x=theta,y=distr),col='red',lwd=2)
    g     <- g + theme_minimal()
    observe({
      z <- input$z
      updateCheckboxInput(session, "z", value = z)
    })
    if(input$z){
      g <- g + geom_vline(xintercept = input$v,lty=3,lwd=2)
      g <- g + geom_point(aes(x = input$v,y = max(distr)),colour = 'blue',size = 2)
    }
    g <- g + ggtitle(paste('Likelihood plot for',toString(x),'successes out of',
                           toString(n),'trials.'))
    g <- g + xlab(expression(theta)) + xlim(c(0,1))
    g <- g + ylab(expression(paste('likelihood -','L(',theta,'|','k,n)')))
    
    g
    })
  ###### Text frame 1
  output$text1 <- renderText("Binomial Probability")
  output$text2 <- renderUI({
    withMathJax(
      helpText("The binomial probability function is,"),
      helpText("$$\\mathbb{P}\\left(k\\big|\\theta,n\\right)=\\binom{n}{k}\\theta^{k}(1-\\theta)^{n-k}$$"),
      helpText("whereas the likelihood function for \\(\\theta\\) is"),
      helpText("$$\\mathfrak{L}_{k,n}(\\theta)=\\mathbb{P}\\left(\\theta\\big|k,n\\right)=\\binom{n}{k}
               \\theta^{k}(1-\\theta)^{n-k}$$"),'which is the 
                maximum likelihood estimate (MLE) for \\(\\max_{\\theta}\\mathfrak{L}_{k,n}(\\theta)
                =\\frac{k}{n}\\) (a proof follows).')})
  output$text3 <- renderText(HTML('<u>proof</u>'))
  output$text4 <- renderUI({
    withMathJax(
      helpText("Suppose we have conducted n trials and we observed k successes, in order 
                 to find the maximum of the likelihood function the following steps provide the way."),
      helpText("$$\\begin{eqnarray*}
                 \\frac{\\mathbb{d}}{\\mathbb{d}\\theta}\\log\\mathfrak{L}_{k,n}(\\theta) &=& 
                 \\frac{\\mathbb{d}}{\\mathbb{d}\\theta}\\log\\left[\\binom{n}{k}\\theta^{k}(1-\\theta)^{n-k}\\right]\\\\
                 &=&\\frac{\\mathbb{d}}{\\mathbb{d}\\theta}
                 \\left[\\log\\left(\\binom{n}{k}\\right)+k\\log(\\theta)+(n-k)\\log(1-\\theta)\\right]\\\\
                 &=&\\frac{k}{\\theta}-\\frac{n-k}{1-\\theta}
                 \\end{eqnarray*}$$In order to find the maximum likelihood estimate we will set the last formula equal 
                 to zero to find the \\(\\theta\\) for which the first derivative of the likelihood distribution is equal
                 to zero and denote the \\(\\theta\\) for which we get the maximum of the likelihood with 
                 \\(\\hat{\\theta}\\).
                 $$\\begin{eqnarray*}\\frac{k}{\\hat{\\theta}}-\\frac{n-k}{1-\\hat{\\theta}} & = & 0\\\\
                 \\frac{k}{\\hat{\\theta}} &=& \\frac{n-k}{1-\\hat{\\theta}}\\\\
                 \\hat{\\theta} &=&\\frac{k}{n}
                 \\end{eqnarray*}$$")
    )
  })
  ###############################frame 2
  ######plot
  output$distPlot2 <- renderPlot({
    n1 <- input$n1
    p1 <- input$p1
    n2 <- input$n2
    p2 <- input$p2
    distr1 <- dbinom(p1,n1,theta)
    distr2 <- dbinom(p2,n2,theta)
    distr  <- dbinom(p1+p2 ,n1+n2 ,theta)
    g  <- {}
    g  <- ggplot(data = cbind.data.frame(theta,distr1,distr2,distr))
    g  <- g + geom_line(aes(x=theta,y=distr1,colour ='first likelihood distribution'))
    g  <- g + geom_line(aes(x=theta,y=distr2,colour = 'second likelihood distribution'),lwd=1.5,alpha=0.5)
    g  <- g + geom_line(aes(x=theta,y=distr ,colour = 'combined likelihood distribution'),lwd=2,alpha=0.3)
    g  <- g + xlab(expression(theta)) + xlim(c(0,1))
    g  <- g + ylab(expression(paste('likelihood -','L(',theta,'|','k,n)')))
    g  <- g + theme_minimal() + labs(color ='Likelihood Distribution Color')
    g  <- g + ggtitle(
          TeX('The combined likelihood for $\\theta$: $L_{combined} = L_{\\theta_{1}}\\cdot L_{\\theta_{2}}$'))
    d  <- cbind.data.frame('1st MLE' = p1/n1,'2nd MLE'=p2/n2,
                           "Combined MLE" = (p1+p2)/(n1+n2))
    output$text5 <- renderTable(d)
    g
  }
  )
  ###############################frame 3
  ######plot
  output$distrPlot3  <- renderPlot({
    s    <- c(10,100,250,500,1000)
    r    <- 0.5 * s
    data <- cbind.data.frame(theta,
                            's10'   =  (dbinom(r[1],10,theta)/dbinom(r[1],10,0.5)),
                            's100'  =  (dbinom(r[2],100,theta)/dbinom(r[2],100,0.5)),
                            's250'  =  (dbinom(r[3],250,theta)/dbinom(r[3],250,0.5)),
                            's500'  =  (dbinom(r[4],500,theta)/dbinom(r[4],500,0.5)),
                            's1000' =  (dbinom(r[5],1000,theta)/dbinom(r[5],1000,0.5)))
    data      <- data %>% gather(key = size,value = d,-theta)
    data$size <- factor(data$size, levels=c("s10", "s100","s250","s500","s1000"),
           labels=c("N = 10", "N = 100", "N = 250","N = 500","N = 1000"))
    g    <- ggplot(data = data)
    CLS  <- brewer.pal(5,'Reds')
    g    <- g + geom_line(aes(x=theta,y=d,colour = size))
    g    <- g + theme_minimal()
    g    <- g + scale_colour_manual(values = CLS)
    g    <- g + ggtitle('likelihood of various thetas against theta=0.5 for different
                        sample sizes')
    g    <- g + ylab('likelihood') + xlab(TeX('$\\theta$'))
    g
    
  })
  ###### text frame 3
  output$text6 <- renderUI(withMathJax(helpText("Here we plot 
  $$\\frac{\\mathfrak{L}\\left\\{\\theta\\left|k/n=0.5)\\right.\\right\\}}
    {\\mathfrak{L}\\left\\{\\theta=0.5\\left|k/n=0.5)\\right.\\right\\}}$$ and it is obvious 
   that as the sample size becomes bigger the likelihood's graph becomes steeper.")))
  ###############################frame 4
  ######plot frame 4
  output$distrPlot4 <- renderPlot({
    l     <- input$u
    t1    <- input$theta1
    t2    <- input$theta2
    distr <- dbinom(l[1],l[2],theta)
    g     <- ggplot(data = cbind.data.frame(theta,distr))
    g     <- g + geom_line(aes(x=theta,y=distr),lwd = 1.5 , alpha = 0.8 , colour = 'orange')
    g     <- g + xlab(expression(theta)) + xlim(c(0,1))
    g     <- g + ylab(expression(paste('likelihood -','L(',theta,'|','k,n)'))     )
    g     <- g + geom_segment(aes(x=t1,y=dbinom(l[1],l[2],t1),xend=l[1]/l[2],yend=dbinom(l[1],l[2],t1)),lty=2)
    g     <- g + geom_segment(aes(x=l[1]/l[2],xend=l[1]/l[2],y = dbinom(l[1],l[2],t1),yend=dbinom(l[1],l[2],t2)),colour = 'cyan')
    g     <- g + geom_segment(aes(x=t2,y=dbinom(l[1],l[2],t2),xend=l[1]/l[2],yend=dbinom(l[1],l[2],t2)),lty=2)
    g     <- g + theme_minimal()
    g     <- g + ggtitle('Likelihood Ratios For Different Thetas')
    output$text7 <- renderUI({
      withMathJax(helpText("We compare the ratio of two likelihoods, 
                           $$\\mathcal{H}=\\frac{\\mathbb{B}(k,n,\\theta_1)}{\\mathbb{B}(k,n,\\theta_2)}$$                     
                           where \\(\\mathbb{B}\\) is the binomial distribution for the given inputs. The values for 
                           \\(\\mathcal{H}\\) and its opposite for each input of \\(\\theta_1,\\;\\theta_2\\) is given 
                           in the following table."))})
    d <- cbind.data.frame("H"=dbinom(l[1],l[2],t1)/dbinom(l[1],l[2],t2),
                          "1/H" = dbinom(l[1],l[2],t2)/dbinom(l[1],l[2],t1))
    output$text8 <- renderTable(d)
    g
    })
  ###############################frame 5
  ######plot frame 5
  output$distrPlot5 <- renderPlot({
    data <- cbind.data.frame(theta,
                             'd0'   = dbinom(0,3,theta),
                             'd1'   = dbinom(1,3,theta),
                             'd2'   = dbinom(2,3,theta),
                             'd3'   = dbinom(3,3,theta))
    
    data           <- data %>% gather(key = successes,value = d,-theta)
    data$successes <- factor(data$successes, levels=c("d0", "d1","d2","d3"),
                        labels=c("0 out of 3", "1 out of 3",
                                 "2 out of 3","3 out of 3"))
    
    g <- ggplot(data = data)
    g <- g + geom_line(aes(x=theta,y=d,colour = successes))
    g <- g + geom_segment(aes(x=0.8,y=0,xend = 0.8,yend = 1)) 
    g <- g + annotate('text',x = 0.9,y = 0.8,label = 'power = 80%')
    g <- g + geom_segment(aes(x=0.05,y=0,xend=0.05,yend=1))
    g <- g + annotate('text',x=0,y=0.8,label = '5% C.I.')
    g <- g + theme_minimal() + xlab(TeX("$\\theta$")) + ylab('likelihood')
    g
  })
  ######text frame 5
  output$distrPlot6 <- renderPlot({
    # help function
    f <- function(k,t){return(dbinom(k,3,t))}
    g <- ggplot()
    g <- g + geom_line(aes(x=theta,y=dbinom(1,3,theta)))
    g <- g + geom_segment(aes(x=0.8,y=0,xend = 0.8,yend = 1)) 
    g <- g + annotate('text',x = 0.9,y = 0.8,label = 'power = 80%')
    g <- g + geom_segment(aes(x=0.05,y=0,xend=0.05,yend=1))
    g <- g + annotate('text',x=0,y=0.8,label = '5% C.I.')
    g <- g + theme_minimal() + xlab(TeX("$\\theta$")) + ylab('likelihood')
    g <- g + geom_segment(aes(x=0.05,y=f(1,0.05),xend=1/3,yend=f(1,0.05)))
    g <- g + geom_segment(aes(x=1/3,y=f(1,0.05),xend=1/3,yend=f(1,0.8)),colour = 'red')
    g <- g + geom_segment(aes(x=1/3,f(1,0.8),xend=0.8,yend=f(1,0.8)))
    g <- g + ggtitle('Likelihood Ratio 0.71')
    h <- ggplot()
    h <- h + geom_line(aes(x=theta,y=dbinom(2,3,theta)))
    h <- h + geom_segment(aes(x=0.8,y=0,xend = 0.8,yend = 1))
    h <- h + annotate('text',x = 0.9,y = 0.8,label = 'power = 80%')
    h <- h + geom_segment(aes(x=0.05,y=0,xend=0.05,yend=1))
    h <- h + annotate('text',x=0,y=0.8,label = '5% C.I.')
    h <- h + theme_minimal() + xlab(TeX("$\\theta$")) + ylab('likelihood')
    h <- h + geom_segment(aes(x=0.05,y=f(2,0.05),xend=2/3,yend=f(2,0.05)))
    h <- h + geom_segment(aes(x=2/3,y=f(2,0.05),xend=2/3,yend=f(2,0.8)),colour = 'red')
    h <- h + geom_segment(aes(x=2/3,f(2,0.8),xend=0.8,yend=f(2,0.8)))
    h <- h + ggtitle('Likelihood Ratio 53.89')
    grid.arrange(g,h,nrow=2)
  })
  output$text9 <- renderUI({HTML("<p style='text-align:justify;'>
    Let’s imagine we have two bags. There are twenty marbles in each bag. In one bag, we
    know there are 19 blue marbles, and one red marble. The red marble represents a Type
    1 error, the blue marbles represent true negatives, and this bag represents a situation
    where we perform a statistical test where the null-hypothesis is true. In the other bag,
    there is a number of blue and red marbles. The red marbles represent true positives, blue
    marbles represent false negatives, and this bag represents a situation where the
    alternative hypothesis is true. We don’t know the true power, or the percentage of red
    marbles, but we can make a guess. For example, we might believe 16 out of 20 (or 80%)
    of the marbles are red.</p>
    <p style='text-align:justify;'>
    We perform 3 studies, by drawing a marble three times (with replacement) from one of
    the two bags. We don’t know which of the bags we are drawing from. We could be drawing
    from the bag where the null hypothesis is true or the bag where the alternative
    hypothesis is true. There are 4 possible outcomes. Either 0 out of 3, 1 out of 3, 2 out of 3,
    or 3 out of 3 red balls are drawn. We can plot these four likelihood curves (see the figure
    below – you can plot these curves one at a time using the R script).
    Each curve has the maximum likelihood estimate at the outcome: θ = 0 for 0 out of 3 red
    balls, θ = 0.33 for 1 out of 3, θ = 0.66 for 2 out of 3, and θ = 1 from 3 out of 3. We know
    that for the bag where the null hypothesis is true, θ = 0.05, or the Type 1 error rate. If we
    believe our studies would have 80% power when the alternative hypothesis is true, we
    can assume θ = 0.8. In the plot below, we see the four likelihood curves, and two vertical
    lines at θ = 0.05 and θ = 0.8.</p>")})

  output$text10 <- renderUI({HTML("
    <p style='text-align:justify;'>
    We can calculate likelihood ratios for θ = 0.05 vs. θ = 0.80 for the different outcomes.
    Below, the likelihood ratios are visualized for 1 out of 3 and 2 out of 3 red balls, or
    significant results. We see that only 1 out of 3 findings (below, left) is slightly more likely
    when the null hypothesis is true, but the likelihood ratio of 0.71 is not very far from 1.
    However, with 2 out of 3 significant results (below, right), it is clear that this result is much
    more likely when studies are performed with 80% power than if we assume we observed
    two out of three Type 1 errors. Indeed, when two out of three studies are significant, the
    likelihood ratio provides pretty strong relative evidence for a situation where the
    alternative hypothesis is true, even when the assumed power is much lower.</p>
")})
  output$text11<-renderUI({HTML("
    <p style='text-align:justify;'>
    Although we can’t formally evaluate the probability that the alternative hypothesis is true
    based on these likelihood ratios (we instead need Bayesian statistics), we can see that
    when multiple studies are performed, but not all studies are statistically significant, we
    are gathering more and more evidence for the hypothesis that a true effect is examined,
    rather than that all significant studies are Type 1 errors.</p>
    <p style='text-align:justify;'>
    The graphs above with the four likelihood curves also shows when a researcher is more
    likely to observe mixed results than consistent results. This occurs for all values of θ
    where the likelihood curve for mixed results is higher than the likelihood curves for 0 out
    of n, or n out of n significant results. More formally, mixed results are more likely than
    consistent results when power drops below n/(n+1) percent, or increases above 1-
    (n/(n+1)). For example, when performing three studies, it is more likely to observe mixed
    results than only significant or only non-significant effects when power is lower than
    3/(3+1)=0.75, and higher than 1-(3/(3+1))=0.25.</p>
    <p style='text-align:justify;'>
    We have seen how likelihood functions allow us to evaluate the relative likelihood of the
    data we have collected under different possible true values of the parameter (or different
    hypotheses). In this assignment, we have focused on binomial likelihoods, but likelihood
    functions exist for many different distributions (e.g., Poisson, normal, etc.). We have
    applied this basic understanding of binomial likelihoods to the likelihood that a set of
    studies with mixed results (e.g., 2 out of 3 significant results) occurs when we are making
    Type 1 errors compared to when we have a specific level of power. We will continue using
    binomial likelihoods when we learn about Bayesian statistics.
    </p>")})
  ########################################TAB 2##########################################
  ########################################  OK ##########################################
  ###########F R A M E 1
  ####### text till graph with TeX
  output$text12 <- renderUI({
    withMathJax(HTML("
<p style='text-align:justify;'>
When we do research, we often start with a prior belief that a hypothesis is true. Then,
we collect data, and we use this data to update our belief that a theory is true. Bayesian
statistics allows you to update prior beliefs into posterior probabilities in a logically
consistent manner. Before we have collected data, the prior odds of Hypothesis 1 (H1)
over the null-hypothesis (H0) are P(H1)/P(H0), After we have collected data, we have the
posterior odds P(H1|D)/P(H0|D), which you can read as the probability of H1, given the
data, divided by the probability of H0, given the data. There are different approaches to
Bayesian statistics. In this assignment we will first discuss Bayes factors, and then
Bayesian estimation.
</p>
<p style='text-align:center;'><u>Bayes factor</u></p>
<p style='text-align:justify;'>
One approach in Bayesian statistics focusses on the comparison of different models that
might explain the data (referred to as model comparison). In Bayesian statistics, the
probability of data under a specified model (D|P(H0) is a number that expressed what is
sometimes referred to as the absolute evidence, and more formally referred to as a
marginal likelihood. The marginal likelihood uses prior probabilities to average the
likelihood across the parameter space. For example, assume we have a simple model M
that is based on a single parameter, that can take on two values, X and Y, and that a-prior
we believe the probability of both values is p(X) = 0.4 and p(Y) = 0.6. We collect data, and
calculate the likelihood for both these parameter values, which is p(D|X) = 0.02 and
p(D|Y) = 0.08. The marginal likelihood of our model M is then P(D|M) = 0.4 × 0.02 + 0.6 ×
0.08 = 0.056. Most often, models have continuously varying parameters, and the marginal
likelihood formula is based on an integral, but the idea remains the same.</p>
<p style='text-align:justify;'>
When comparing models, we do not use the absolute evidence for a model, but the
relative evidence for models we are comparing. The relative evidence is calculated by
dividing the marginal likelihood for one model by the marginal likelihood for another
model, and this ratio of relative evidence based on these marginal likelihoods is called
the Bayes factor. The Bayes factor represents how much we have updated our beliefs,
based on observing the data. We can express Bayes factors to indicate how much more 
likely H1 is given the data compared to H0 (often indicated by B10) or as how much more
likely H0 has become compared to H1 (B01), and B10 = 1/B01. Similar to likelihood ratios, a
Bayes factor of 1 did not change our beliefs for one model compared to the other model.
A very large Bayes factor for H1 over H0 has increased our belief in H1, and a Bayes Factor
close for H1 over H0 to 0 has increased our belief in H0. If our prior belief in H1 was very,
very low (e.g., your belief in unicorns) even a large Bayes factor that supports the
presence of a unicorn might not yet convince you that unicorns are real – but you have
updated your belief in unicorns, and now believe they are at least more likely then they
were before (even if you still think unicorns are very unlikely to exist). The contribution of
the Bayes Factor and the prior in calculating the posterior odds is clear in the following
formula:
$$\\underbrace{\\frac{\\mathbb{P}\\left(H_1|D\\right)}{\\mathbb{P}\\left(H_0|D\\right)}}_{
\\text{posterior probability}} =
\\underbrace{\\frac{\\mathbb{P}\\left(D|H_1\\right)}{\\mathbb{P}\\left(D|H_0\\right)}}_{
\\text{prior probability}} \\times 
\\underbrace{\\frac{\\mathbb{P}\\left(H_1\\right)}{\\mathbb{P}\\left(H_0\\right)}}_{
\\text{prior probability}}
$$
A Bayesian analysis of data requires specifying the prior. Here, we will continue our
example based on a binomial probability, such as a coin flip. In the likelihood example,
we compared two point hypotheses (e.g., θ = 0.5 vs. θ = 0.8). In Bayesian statistics,
parameters are considered to be random variables, and the uncertainty or degree of
belief with respect to the parameters is quantified by probability distributions.</p>
<p style='text-align:justify;'>
A binomial probability lies between 0 and 1. You could draw any probability density you
want over 0 and 1, and turn it into a prior, but for good reasons (simplicity, mostly) a beta-
prior is often used for binomial probabilities. The shape of the beta-prior depends on two
parameters, α and β. Note that these are the same Greek letters as used for the Type 1
error rate and Type 2 error rate, but that is purely coincidental! The α and β in binomial
probabilities are unrelated to error rates, and the use of the same letters is mainly due
to a lack of creativity among statisticians and the limited choice the alphabet gives us. It
also does not help that β is one of the parameters of the Beta distribution. Try to keep
these different Beta’s apart! The probability density function is:
$$\\mathbb{P}(x)=\\frac{1}{\\text{B}(\\alpha,\\beta)}x^{\\alpha-1}(1-x)^{\\beta-1}$$
where B(α,β) is the beta function. Understanding the mathematical basis of this function
is beyond the scope of this assignment, but you can read more on <a href =
'https://en.wikipedia.org/wiki/Beta_distribution'>
Wikipedia</a> or in 
<a href = 'https://sites.google.com/site/doingbayesiandataanalysis/'>John Kruschke’s book</a> if interested. 
The beta-prior for a variety of values for α and β can be
seen in the figure below.
</p>"))
  })
  output$distrPlot7 <- renderPlot({
    grid.arrange(
      qplot(x=theta,y=dbeta(theta,1,1),main='New Born Baby Prior',xlab=TeX('$\\theta$'),
           ylab="B(1,1)",geom='line'),
      qplot(x=theta,y=dbeta(theta,1,1/2),main='True Believer Prior',xlab=TeX('$\\theta$'),
            ylab="B(1,1/2)",geom='line'),
      qplot(x=theta,y=dbeta(theta,4,4),main='Slightly Skeptical Believer Prior',xlab=TeX('$\\theta$'),
            ylab="B(4,4)",geom='line'),
      qplot(x=theta,y=dbeta(theta,100,100),main='Extremely Skeptical Believer Prior',xlab=TeX('$\\theta$'),
            ylab="B(100,100)",geom='line'),nrow=2)
  })
  output$text13 <- renderUI({withMathJax(HTML(paste(
"<p style='text-align:justify;'>
These beta densities reflect different types of priors. Let’s assume you are approached
by a street merchant who tries to sell you a special coin with heads and tails that, when
flipped, will almost always turn up heads. The α = 1, β = 1 prior is what a newborn baby
would have as a prior, without any idea of what to expect when you flip a coin, and thus
every value of θ is equally likely. The α = 1, β = 1/2 prior is what a true believer would 
have as a prior. The sales merchant tells you the coin will turn up heads almost every time, and
thus, you believe it will turn up heads almost every time. The α = 4, β = 4, and the α = 100,
β = 100 priors are for slightly and extremely skeptical people. With an α = 4, β = 4 prior,
you expect the coin will be fair, but you are willing to believe a wide range of other true
values is possible (the curve is centered on 0.5, but the curve is wide, allowing for very
high and low values of θ). With the α = 100, β = 100 prior you are really convinced coins
are fair, and believe there will be only a very slight bias, at most (the curve is again
centered on 0.5, and a skeptic believes the θ will lie between 0.4 and 0.6 – a much
narrower range compared to the slightly skeptic individual).</p>
<p style='text-align:justify;'>
Let’s assume the newborn baby, the true believer, the slightly skeptic and the extreme
skeptic all buy the coin, flip it n = 20 times, and observe x = 10 heads. This outcome can
be plotted as a binomial distribution with 10 heads out of 20 trials, or as a Beta(11, 11)
distribution.
The newborn baby had a prior Beta distribution with α = 1 and β = 1, which equals a
binomial likelihood distribution for 0 heads out of 0 trials. The posterior is a Beta
distribution with Beta(α*, β*), where:
$$\\begin{eqnarray}\\alpha* &=& \\alpha + x = 1 + 10 = 11
\\\\\\\\\\beta* &=& \\beta + n - x = 1 + 20 - 10 = 11\\end{eqnarray}$$
Thus, the posterior distribution for the newborn is a Beta(11,11) distribution. This equals
a binomial likelihood function for 10 heads out of 20 trials, or Beta(11,11) distribution. In
other words, the posterior distribution is identical to the likelihood function when a
uniform prior is used.<br><br>
Q1: The true believer had a prior of Beta(1,0.5). After observing 10 heads out of 20 coin
flips, what is the posterior distribution, given that α* = α + x and β* = β + n – x?
<br><br>
A) Beta(10, 10)<br>
B) Beta(11, 10.5)<br>
C) Beta(10, 20)<br>
D) Beta(11, 20.5)<br><br>
",
toString(if(!(input$Q1 %in% c('A','B','C','D'))){print('Check Answer here!')}
         else if(input$Q1 == 'B'){print('correct!')}
         else print('wrong!'))
,"
<br><br>
Q2: The strong skeptic had a prior of Beta(100,100). After observing 50 heads out of 100
coin flips, what is the posterior distribution, given that α* = α + x and β* = β + n – x?
<br>
A) Beta(50, 50)<br>
B) Beta(51, 51)<br>
C) Beta(150, 150)<br>
D) Beta(151, 151)<br><br>",
toString(if(!(input$Q2 %in% c('A','B','C','D'))){print('Check Answer here!')}
         else if(input$Q2 == 'C'){print('correct!')}
         else print('wrong!')),"<br><br>
Take a look at the top left graph below. Given 10 heads out of 20 coin flips, we see the
prior distribution of the newborn (the horizontal grey line), the likelihood (the blue dotted
line) and the posterior (the black line).<br><br>
For the true believer the posterior distribution is not centered on the maximum likelihood
of the observed data, but just a bit in the direction of the prior. The slightly skeptic and
the strong skeptic end up with a much stronger belief in a fair coin after observing the
data, but mainly because they already had a stronger prior that the coin was fair.
</p>
")

))})
  output$distrPlot8 <- renderPlot({
    data <- cbind.data.frame(theta=theta,prior=dbeta(theta,1,1),
                             data_info = dbeta(theta,10,10),
                             posterior = dbeta(theta,11,11))
    data <- data %>% gather(key = distribution,value = d,-theta)
    g    <- ggplot(data,aes(x=theta,y=d,colour=distribution))
    g    <- g + geom_line(lwd=1.5) + xlab(TeX('$\\theta$')) + ylab('density values')
    g    <- g + ggtitle('New Born Baby Updated Beliefs')
    data <- cbind.data.frame(theta=theta,prior=dbeta(theta,1,1/2),
                             data_info = dbeta(theta,10,10),
                             posterior = dbeta(theta,11,10.5))
    data <- data %>% gather(key = distribution,value = d,-theta)
    h    <- ggplot(data,aes(x=theta,y=d,colour=distribution))
    h    <- h + geom_line(lwd=1.5) + xlab(TeX('$\\theta$')) + ylab('density values')
    h    <- h + ggtitle('True Believer Updated Beliefs')
    
    data <- cbind.data.frame(theta=theta,prior=dbeta(theta,4,4),
                             data_info = dbeta(theta,10,10),
                             posterior = dbeta(theta,14,14))
    data <- data %>% gather(key = distribution,value = d,-theta)
    j    <- ggplot(data,aes(x=theta,y=d,colour=distribution))
    j    <- j + geom_line(lwd=1.5) + xlab(TeX('$\\theta$')) + ylab('density values')
    j    <- j + ggtitle('Slightly Skeptical Updated Beliefs')
    
    data <- cbind.data.frame(theta=theta,prior=dbeta(theta,100,100),
                             data_info = dbeta(theta,10,10),
                             posterior = dbeta(theta,110,110))
    data <- data %>% gather(key = distribution,value = d,-theta)
    k    <- ggplot(data,aes(x=theta,y=d,colour=distribution))
    k    <- k + geom_line(lwd=1.5) + xlab(TeX('$\\theta$')) + ylab('density values')
    k    <- k + ggtitle('Extremely Skeptical Updated Beliefs')
    
    grid.arrange(g,h,j,k,nrow=2)
  })
  
  
  })