library(shiny)
library(shinyjs)
library(pwr)
library(shinythemes)

# jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page
# constants ---
sd <- 1
n <- 1
min_x <- -7
max_x <- 7
effect_size <- sample(c(0, 0.5, 0.8), 1, 0)
direction <- sample(c(-1, 1), 1, 0)
shift_es <- sample(c(0, 0.5, 1), 1, 0)

# Define UI ----
ui <- fluidPage(theme= shinytheme("lumen"),

  useShinyjs(), #add useShinyjs to be able to disable buttons upon making a choice.
  # extendShinyjs(text = jsResetCode),
  # Application title
  titlePanel("Guess The Effect"),

  # Show a plot of the generated distribution
  sidebarPanel(
    p("Your task is to guess whether there is a real difference between two groups, one represented by circles, and one represented by squares. To inform your guess, you will sample individual data points from each group."),
    p("The real difference between the two groups will be randomly decided by the app (and shown after you made your decision). The difference is sometimes zero. The true means for each group lie between -3 and +3 and are randomly determined."),
    p("Once you are sufficiently certain about whether there is a real difference or not, click one of the buttons at the bottom to submit your choice. Afterwards, the app will reveal whether you were correct. If you want to try again, reload the page."),
    tags$br(),
    #actionButton("trialButton", "Start a New Data Collection Trial"),
#    h4(uiOutput("effectsize")),
    actionButton("sampleButton", "Sample a new datapoint"),
  h4(uiOutput("displayCounter"))
  ),
  mainPanel(
#    h4(uiOutput("display_group")),
    actionButton("yesButton", "I think the circle and square groups are equal"),
    actionButton("noButton", "I think the circle and square groups are different"),
    plotOutput("Plot"),
    p(uiOutput("results"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  values <- reactiveValues(means = list(),
                           grouplist = list())

  output$displayCounter <- renderText({
    c("Number of Datapoints Sampled:", input$sampleButton)
  })
  output$effectsize <- renderText({
    c("Effect Size:", effect_size)
  })


  # output$display_group <- renderText({
  #   if (length(values$grouplist) == 0) {
  #     g <- ""
  #   } else {
  #     g <- values$grouplist[input$sampleButton][[1]]
  #   }
  #   c("Group:", g)
  # })

  #Result from the final choice ----
  output$results <- renderText({ data_results() })
  
  # start response buttons disabled until enough data points are generated
  shinyjs::disable("noButton")
  shinyjs::disable("yesButton")

  #Disable buttons (except new trial button) after choice is made ----
  observeEvent(input$noButton,  {
    shinyjs::disable("noButton")
    shinyjs::disable("yesButton")
    shinyjs::disable(id = "sampleButton")
  })
  observeEvent(input$yesButton,  {
    shinyjs::disable("noButton")
    shinyjs::disable("yesButton")
    shinyjs::disable(id = "sampleButton")
  })

  #Clicking the answer buttons will end the trial and store the data ----

  #save data
  data_results <- eventReactive(c(input$noButton, input$yesButton),  {
    if (length(values$means) == 0) { return("Results will appear here once you have clicked one of the two buttons below. The results will tell you the true effect size and group means that the simulation is based on, the observed difference in your sample, and whether the observed difference differs from zero (p < .05).") }
    means <- values$means
    grouplist <- values$grouplist
    
    #bind data into dataframe
    data_results <- data.frame(as.numeric(unlist(means)), as.numeric(unlist(grouplist)))
    colnames(data_results) <- c("means", "grouplist")
    #Perform t-test and save as z
    z <- t.test(means ~ grouplist, data_results, var.equal = TRUE)
    #Is test significant or not?
    testoutcome<-ifelse(z$p.value<.05,"significant","non-significant")
    #Calculate Cohen's d
    d <- z$stat[[1]] * sqrt(sum(grouplist==1)+sum(grouplist==2))/sqrt(sum(grouplist==1)*sum(grouplist==2))
    obs_power <- pwr.t.test(d=d,
                            n=round((sum(grouplist==1)+sum(grouplist==2))/2),
                            sig.level=0.05,
                            type="two.sample")$power
    #set judgment to 0 if no is pressed, to 1 if yes is pressed.
    judgement <- ifelse(input$noButton == 0,0,1) 
    correct <- ifelse(judgement == (effect_size>0),
                      "You made the correct choice.",
                      "You did not make the correct choice.")
    #Give results
    out <- paste0(correct,
                  " The true effect size in the population we are simulating data from was ",
                  effect_size*direction,
                  ". The population mean in the circle group was ",
                  (0 + shift_es) * direction,
                  " (the observed mean was ",
                  round(z$estimate[[1]],2),
                  " and the population mean in the square group was ",
                  (effect_size + shift_es) * direction,
                  " (the observed mean was ",round(z$estimate[[2]],2),
                  ". The null hypothesis significance test was ",
                  testoutcome,
                  ", t(",round(z$parameter[[1]], digits=2),") = ",
                  format(z$stat[[1]], digits = 3, nsmall = 3, scientific = FALSE),
                  ", p = ",format(z$p.value[[1]], digits = 3, nsmall = 3, scientific = FALSE),
                  ", given an alpha of 0.05. Based on the data you sampled you had ",
                  100*round(obs_power,2),
                  "% power to detect a difference, based on an observed effect size of d = ",
                  round(d,2),
                  ". Reload this website if you want to do this task again.")
    
    #results <- list(out = out, d = d, obs_power = obs_power)
    return(out)
  })
  
  observeEvent(c(input$noButton, input$yesButton),  {
    if (length(values$means) == 0) { return(F) }
    means <- values$means
    grouplist <- values$grouplist
    
    outputDir <- "responses"
    #set judgment to 0 if no is pressed, to 1 if yes is pressed.
    judgement <- ifelse(input$noButton == 0,0,1) 
    data <- data.frame(judgement, effect_size, means, grouplist)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.table(
      x = data,
      file = file.path(outputDir, fileName),
      row.names = FALSE,
      col.names = FALSE,
      quote = FALSE
    )
    
    output$Plot <- renderPlot({
      plot(NA,
           ylim = c(0, 1),
           xlim = c(min_x, max_x),
           yaxt = "n",
           xaxt = "n",
           ylab = "",
           xlab = "Observed Score (on a scale from -7 to 7)")
        
      axis(1, at = seq(min_x, max_x), labels = seq(min_x, max_x, 1), las = 1)
      
      abline(v = seq(min_x, max_x, 1),
             lty = 2,
             col = "grey")
      
      abline(v = c((0 + shift_es) * direction, (effect_size + shift_es) * direction),
             lty = 2,
             lwd = 2,
             col = "red")
      
      points(x = (0 + shift_es) * direction,
             y = 0.5,
             pch = 16,
             cex = 3,
             col = rgb(1,0,0, alpha = 0.5))
        
      points(x = (effect_size + shift_es) * direction,
             y = 0.5,
             pch = 15,
             cex = 3,
             col = rgb(1,0,0, alpha = 0.5))
    })
  })
  
  # Sample button actions ----
  observeEvent(input$sampleButton, {
    # enable response buttons after 3 observations
    if (length(values$means) > 1) {
      shinyjs::enable("noButton")
      shinyjs::enable("yesButton")
    }
    
    ## set group based on value for sample button (great suggestion by Nick Coles)
    if((input$sampleButton %% 2) == 0) {
      group <- 1
    } else {
      group <- 2
    }
    
    values$grouplist[input$sampleButton] <- group
    
    ## generate a data point from the group
    if(group == 1){
      x <- rnorm(n, (0 + shift_es) * direction, sd)
      if(x > 7){x <- 7} #prevent values more extreme than 7
      if(x < -7){x <- -7}
    } else if(group == 2){
      x <- rnorm(n, (effect_size + shift_es) * direction, sd)
      if(x > 7){x <- 7}
      if(x < -7){x <- -7}
    }
    values$means[input$sampleButton] <- mean(x)
  })

  output$Plot <- renderPlot({
    plot(NA,
         ylim = c(0, 1),
         xlim = c(min_x, max_x),
         yaxt = "n",
         xaxt = "n",
         ylab = "",
         xlab = "Observed Score (on a scale from -7 to 7)")
    axis(1, at = seq(min_x, max_x), labels = seq(min_x, max_x, 1), las = 1)
    abline(v = seq(min_x, max_x, 1),
           lty = 2,
           col = "grey")
    
    # only plot data if there's something to plot
    if (length(values$means) > 0) {
      dif <- values$means[input$sampleButton]
      group <- values$grouplist[input$sampleButton]
      
      if(group == 1){
        points(x = dif,
               y = 0.5,
               pch = 16,
               cex = 2)
      }
      if(group == 2){
        points(x = dif,
               y = 0.5,
               pch = 15,
               cex = 2)
      }
    }
  })


  # #reset the app completely
  # observeEvent(input$trialButton,  {
  #   js$reset()
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

