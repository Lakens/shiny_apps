#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme('flatly'),
  
  # Application title
  title = "P-Values Histrogram Of One Sample Two Sided Tests",
  
  # siderbar inputs
  sidebarLayout(
    sidebarPanel(
      numericInput('nSims',
                   'Number of Simulations:',
                   min = 10,
                   max = 1e5,
                   value = 1e4, 
                   step = 10),
      numericInput('n',
                   'Enter the sample size:',
                   min = 4,
                   max = 100000,
                   value = 26,
                   step = 1),
      sliderInput('M',
                   'Enter the mean of the population (target):',
                   min = 80,
                   max = 120,
                   value = 106,
                   step  = 1
      ),
      sliderInput('s',
                  'Enter the sd of the population (target):',
                  min = 1,
                  max = 30,
                  value = 15,
                  step  = 1
      ),
      sliderInput('bars',
                  'Set the number of bars for the histogram:',
                  min = 20,
                  max = 100,
                  value = 20,
                  step  = 10
                  ),
      sliderInput('alpha',
                  'Enter the alpha level (Type I error rate):',
                  min = 0.01,
                  max = 0.05,
                  value = 0.05,
                  step  = 0.01
                  ),
      checkboxInput('zoom',
                  'Zoom graph to the p < 0.05 area.',
                  value = F
      )
      ,br(),
      h4("This code simulates one-sample t-tests and plots the p-value distribution."),
      h4("It is Assignment 1.1 in my free Coursera MOOC ", a("Improving Your Statistical Inferences", href="https://www.coursera.org/learn/statistical-inferences")),
      h4("Initial version of this Shiny app created by ", a("George Papadopoulos", href="mailto:pgeorgios8@gmail.com")),
      h4("Get the code at ", a("GitHub", href="https://github.com/Lakens/p-curves"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      br(),
       plotOutput("distPlot"),
      br(),
      h1("Which P-values can you expect?"),
      br(),
      h4("Which p-values can you expect to observe if there is a true effect, and you repeat the same study one-hundred thousand times? And which p-values can you expect if there is no true effect, and you repeat the same study one-hundred thousand times? Take a moment to try to answer these two questions for yourself, before you will learn the answer in this assignment."),
      h4("In your life, you might never do enough studies to get a feel for which p-values you should expect. Luckily, we can easily simulate studies, calculate a p-value for each simulated study, and see what happens. Understanding which p-values you can expect is very important, because it will help you to better interpret p-values."),
      h4("Which p-values you can expect is completely determined by the statistical power of the study, or the probability that you will observe a significant effect, if there is a true effect. The statistical power ranges from 0 to 1. Let's get started by performing some simulations. You can set the number of simulations (the higher the number the more accurate the results, but the longer it takes – leaving them at 1000 for this assignment should be ok). This shiny app simulates one-sample t-tests. The idea is that we simulate IQ scores for a group of people (you can change the sample size, which is set to 26 by default). We know the standard deviation of IQ scores is 15 (specified as well by moving the slider). For now, we will set the mean IQ score in the simulated group to 106, which we will compare to the average IQ score of all people (which is known to be 100 – that's how IQ tests are normalized). We are testing if the people in our simulated sample have an IQ that differs from the average (and we know the correct answer is 'yes', because we made it so in the simulation)."),
      h4("In the simulation, we generate n (by default 26) normally distributed IQ scores with a mean of M (106 by default) and a standard deviation of 15. We then perform a one-sample t-test and store the p-value."),
      h4("The simulation can take a while to run (especially when simulating more than 1000 studies), and will display a plot of all observed p-values."),
      h4("On the x-axis we see p-values from 0 to 1 in 20 bars, and on the y-axis we see how frequently these p-values were observed. There is a horizontal grey line that indicates an alpha of 5% – but you can ignore this line for now. In the title of the graph, the statistical power that is achieved in the simulated studies is given (assuming an alpha of 0.05): The studies have 50% power."),
      br(),
      h4("Q1: Since the statistical power is the probability of observing a statistically significant result, if there is a true effect, we can also see the power in the figure itself. Where?"),
      tags$div(tags$ul(
        tags$li("A) We can calculate the number of p-values larger than 0.5, and divide them by the number of simulations."),
        tags$li("B) We can calculate the number of p-values in the first bar (which contains all 'significant' p-values from 0.00 to 0.05) and divide the p-values in this bar by the total number of simulations."),
        tags$li("C) We can calculate the difference between p-values above 0.5 minus the p-values below 0.5, and divide this number by the total number of simulations."),
        tags$li("D) We can calculate the difference between p-values above 0.5 minus the p-values below 0.05, and divide this number by the number of simulations.")),
      br(),
      h4("Q2: Change the sample size to 51 (by typing in the number). The simulation will update the plot. What is the power in the simulation now that we have increased the sample size from 26 people to 51 people?"),
      tags$div(tags$ul(
        tags$li("A) 55%"),
        tags$li("B) 60%"),
        tags$li("C) 80%"),
        tags$li("D) 95%"))),
      br(),
      h4("Q3) If you look at the distribution of p-values, what do you notice?"),
      tags$div(tags$ul(
        tags$li("A) The p-value distribution is exactly the same as with 50% power"),
        tags$li("B) The p-value distribution is much steeper than with 50% power"),
        tags$li("C) The p-value distribution is much flatter than with 50% power"),
        tags$li("D) The p-value distribution is much more normally distributed than with 50% power"))),
      br(),
      h4("Feel free to increase and decrease the sample size and see what happens if you run the simulation."),
      h4("Q4) What would happen when there is no true difference between our simulated samples and the average IQ score? In this situation, we have no probability to observe an effect, so you might say we have 0 power. Some people prefer to say power is not defined when there is no true effect. I tend to agree, but we can casually refer to this as 0 power. Change the mean IQ score in the sample to 100 by moving the slider. There is now no difference between the average IQ score, and the mean IQ in our simulated sample. Run the script again. What do you notice? "),
      br(),
      tags$div(tags$ul(
        tags$li("A) The p-value distribution is exactly the same as with 50% power"),
        tags$li("B) The p-value distribution is much steeper than with 50% power"),
        tags$li("C) The p-value distribution is basically completely flat (ignoring some minor variation due to random noise in the simulation)"),
        tags$li("D) The p-value distribution is normally distributed"))),
      br(),
      h4("The question below builds on the simulation above where there was no true difference between the groups."),
      br(),
      h4("Q5) Look at the leftmost bar in the plot, and look at the frequency of p-values in this bar What is the formal name for this bar?"),
      tags$div(tags$ul(
        tags$li("A) The power (or true positives)"),
        tags$li("B) The true negatives"),
        tags$li("C) The Type 1 error (or false positives)"),
        tags$li("D) The Type 2 error (or false negatives)"))),
      br(),
      h4("When there is no true effect, p-values are what is called ",
        strong("'uniformly distributed under the null'"),
        ". Every p-value is equally likely when the null hypothesis is true, and every bar in the graph will contain 5% of all the p-values (as indicated by the grey line). When there is no true effect, a p-value of 0.08 is just as likely as a p-value of 0.98. This is important to realize. When there is no true effect, p-values are uniformly distributed. When there is a true effect, the p-value distribution depends on the power, and the higher the power, the more p-values fall below 0.05, and the steeper the p-value distribution becomes."),
      h4("Lets take a look at just the p-values below 0.05. The goal of this part of this assignment is to cure you from a bi-polar p-value disorder, where people incorrectly think all p-values > 0.05 are support for the null-hypothesis, and all p-values below 0.05 are support for the alternative hypothesis. Bear with me for the next few steps – it will be worth it. Change the number of bars to 100. We will now get 1 bar for p-values between 0 and 0.01, one bar for p-values between 0.01 and 0.02, and 100 bars in total. The grey line will now indicate the frequency of p-values when the null hypothesis is true, where every bar contains 1% of the total number of p-values. We only want to look at p-values below 0.05, and we will cut off the plot at 0.05. Click the button that will zoom the graph to the p < 0.05 area. Instead of seeing all p-values between 0 and 1, we will only see p-values between 0 and 0.05. Re-run the simulation (still with a mean of 100). We see the same uniform distribution, but now every bar contains 1% of the p-values, so the p-value distribution is very flat and almost impossible to see. The grey line now clearly gives the frequency for each bar, assuming the null hypothesis is true."),
      h4("Change the mean in the simulation to 107 (remember the sample size is still 51). The simulation will re-run. It's clear we have very high power. Most p-values are in the left-most bar, which contains all p-values between 0.00 and 0.01."),
      br(),
      h4("Q6) The plot from the last simulation tells you we have 90.5% power. This is the power if we use an alpha of 5%. But we can also use an alpha of 1%. What is the statistical power we have in the simulated studies when we would use an alpha of 1%, looking at the graph? Pick the answer closest to the answer from your simulations."),
      tags$div(tags$ul(
        tags$li("A) ~90%"),
        tags$li("B) ~75%"),
        tags$li("C) ~50%"),
        tags$li("D) ~5%"))),
      br(),
      h4("Change the mean in our sample to 108 in line 9 (M<-108), and leave the sample size at 51. Run the simulation. Look at how the distribution has changed compagrey to the graph above. Look at the fifth bar from the left. This bar now contains all the p-values between 0.04 and 0.05. You will notice something peculiar. Remember that the grey line indicates the frequency in each bar, assuming the null hypothesis is true. See how the bar with p-values between 0.04 and 0.05 is lower than the grey line. We have simulated studies with 96% power. When power is very high, p-values between 0.04 and 0.05 are very rare – they occur less than 1% of the time (most p-values are smaller than 0.01). When the null hypothesis is true, p-values between 0.04 and 0.05 occur exactly 1% of the time (because p-values are uniformly distributed). Now ask yourself: When you have very high power, and you observe a p-value between 0.04 and 0.05, is it more likely that the null-hypothesis is true, or that the alternative hypothesis is true? Given that you are more likely to observe p-values between 0.04 and 0.05 when the null hypothesis is true, than when the alternative hypothesis is true, you should interpret a p-value significant with an alpha of 0.05 as more likely when the null hypothesis is true, than when the alternative hypothesis is true. I said I'd cure you from your bi-polar p-value disorder, didn't I?"),
      h4("In our simulations, we know there is a true effect or not, but in the real world, you don't know. When you have very high power, use an alpha level of 0.05, and find a p-value of p = .045, the data is surprising, assuming the null hypothesis is true, but it is even more surprising, assuming the alternative hypothesis is true. This shows how a significant p-value is not always evidence for the alternative hypothesis."),
      br(),
      h4("Q7) When you know you have very high (e.g., 98%) power for the smallest effect size you care about, and you observe a p-value of 0.045, what is the correct conclusion?"),
      tags$div(tags$ul(
        tags$li("A) The effect is significant, and provides strong support for the alternative hypothesis."),
        tags$li("B) The effect is significant, but it is without any doubt a Type 1 error."),
        tags$li("C) With high power, you should use an alpha level that is smaller than 0.05, and therefore, this effect can not be considered significant."),
        tags$li("D) The effect is significant, but the data are more likely under the null hypothesis than under the alternative hypothesis."))),
      br(),
      h4("If this sounds counterintuitive, that's understandable. This is known a Lindley's paradox. A result can be unlikely when the null hypothesis is true, but it can be even more unlikely assuming the alternative hypothesis is true, and power is very high. For this reason, some researchers have suggested using lower alpha levels in very large sample sizes, and this is probably sensible advice. Other researchers have suggested using Bayesian statistics (which we will encounter in assignment 2.2), which is also sensible advice. Note that it is quite unlikely to find a paradoxically high p-value (e.g., of p = 0.045) when the alternative hypothesis is true – but it will happen."),
      br(),
      h4("Q8) Play around with the sample size and the mean IQ in the group (lines 9 and 10, and thus, with the statistical power in the simulated studies). Look at the simulation result for the bar that contains p-values between 0.04 and 0.05. The grey line indicates how many p-values would be found in this bar if the null-hypothesis was true (and is always at 1%). At the very best, how much more likely is a p-value between 0.04 and 0.05 to come from a p-value distribution representing a true effect, than it is to come from a p-value distribution when there is no effect? You can answer this question by seeing how much higher the bar of p-values between 0.04 and 0.05 can become. If at best the bar in the simulation is five times as high at the grey line (so the bar shows 5% of p-values end up between 0.04 and 0.05, while the grey line remains at 1%), then at best p-values between 0.04 and 0.05 are five times as likely when there is a true effect than when there is no true effect."),
      tags$div(tags$ul(
        tags$li("A) At best, p-values between 0.04 and 0.05 are equally likely under the alternative hypothesis, and under the null hypothesis."),
        tags$li("B) At best, p-values between 0.04 and 0.05 are approximately 4 times more likely under the alternative hypothesis, than under the null hypothesis."),
        tags$li("C) At best, p-values between 0.04 and 0.05 are ~10 times more likely under the alternative hypothesis, than under the null hypothesis."),
        tags$li("D) At best, p-values between 0.04 and 0.05 are ~30 times more likely under the alternative hypothesis, than under the null hypothesis."))),
      br(),
      h4("For this reason, statisticians warn that p-values just below 0.05 (e.g., between 0.04 and 0.05) are at the very best weak support for the alternative hypothesis. If you find p-values in this range, consider replicating the study, or if that's not possible, interpret the result at least a bit cautiously. If you are interested in the mathematical explanation of p-value distributions, instead of the current explanation that is based on simulations, you can read ", a("Hung, O'Neill, Bauer, & Kohne, 1997", href="https://www.jstor.org/stable/2533093?seq=1#page_scan_tab_contents"),".")
    )
    )
  )
))
