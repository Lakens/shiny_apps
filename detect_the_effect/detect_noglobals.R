library(shiny)
library(shinyjs)
library(pwr)
library(shinythemes)

# Define UI ----
ui <- fluidPage(theme= shinytheme("lumen"),
                
                useShinyjs(), #add useShinyjs to be able to disable buttons upon making a choice.
                # extendShinyjs(text = jsResetCode),
                # Application title
                titlePanel("Guess The Effect"),
                
                # Show a plot of the generated distribution
                sidebarPanel(
                  textInput("ID", "Fill in your student ID in the field below", 1234567),
                  h5("Your task is to guess whether there is a real difference between two groups, one represented by circles, and one represented by squares. To inform your guess, you will sample individual data points from each group."),
                  p("The real difference between the two groups will be randomly decided by the app (and shown after you made your decision). The difference is either an effect size of 0, 0.2, 0.5, or 0.8. If there is an effect, it can be positive or negative (i.e., squared can have a higher or lower means than circles)."),
                  h5("You should sample data until you are 80% certain about your decision about whether there is a real difference or not. If you do this task 30 times, you should guess correctly 24 of the 30 times."),
                  p("Click the 'Start A New Trial' button to start, and click the 'Sample A New Datapoint' button until you are 80% certain of your choice. Then click one of the two buttons below the figure to submit your choice. Afterwards, the app will reveal whether you were correct or not. You can click the 'Start A New Trial' button to start again. The app will keep track of your performence."),
                  tags$br(),
                  actionButton("resetButton", "Start a New Trial", 
                               style = "padding:20px; font-size:140%"),
                  tags$br(),
                  tags$br(),
                  actionButton("sampleButton", "Sample a new datapoint", 
                               style = "padding:20px; font-size:140%"),
                  h4(uiOutput("displaycount")),
                  h4(uiOutput("displayNTrials")),
                  h4(uiOutput("displayCorrectTrials"))
                ),
                mainPanel(
                  plotOutput("Plot"),
                  actionButton("yesButton", "I think the circle and square groups are equal", 
                               style = "float:right; padding:5px; font-size:105%"),
                  actionButton("noButton", "I think the circle and square groups are different", 
                               style = "padding:5px; font-size:105%"),
                  p(uiOutput("results"))
                )
)

# Set reactiveValues ----
values <- reactiveValues(
  means = list(),
  grouplist = list(),
  sd = 1,
  n = 1,
  min_x = -7,
  max_x = 7,
  effect_size = sample(c(0, 0, 0, 0.2, 0.5, 0.8), 1, 0),
  direction = sample(c(-1, 1), 1, 0),
  shift_es = sample(c(0, 0.5, 1), 1, 0),
  count = 0,
  trials = 0,
  correct_trials = 0,
  judgement = 0
)

server <- function(input, output, session) {
  ## Display number of datapoints ----
  output$displaycount <- renderText({
    c("Datapoints Sampled:", values$count)
  })
  
  ## Display number of trials ----
  output$displayNTrials <- renderText({
    c("Number of Trials:", values$trials)
  })
  
  ## Display correct trials ----
  output$displayCorrectTrials <- renderText({
    c("Correct Trials:", values$correct_trials)
  })
  
  ## Display Effect Size ----
  output$effectsize <- renderText({
    c("Effect Size:", values$effect_size)
  })
  
  ## Display result from the final choice ----
  output$results <- renderText({ data_results() })
  
  # start response buttons disabled until enough data points are generated
  shinyjs::disable("noButton")
  shinyjs::disable("yesButton")
  shinyjs::disable("sampleButton")
  
  #Disable buttons (except new trial button) after choice is made ----
  observeEvent(input$noButton,  {
    shinyjs::disable("noButton")
    shinyjs::disable("yesButton")
    shinyjs::disable(id = "sampleButton")
    shinyjs::enable("resetButton")
    values$judgement <- 1
  })
  observeEvent(input$yesButton,  {
    shinyjs::disable("noButton")
    shinyjs::disable("yesButton")
    shinyjs::disable(id = "sampleButton")
    shinyjs::enable("resetButton")
    values$judgement <- 0
  })
  
  observeEvent(c(input$resetButton),  {
    shinyjs::enable("sampleButton")
    shinyjs::disable("resetButton")
    
    values$effect_size <- sample(c(0, 0, 0, 0.2, 0.5, 0.8), 1, 0)
    values$direction <- sample(c(-1, 1), 1, 0)
    values$shift_es <- sample(c(0, 0.5, 1), 1, 0)
    values$count <- 0
    values$means <- list()
    values$grouplist <- list()
    values$trials <- values$trials + 1
    
    output$results <- renderText({ data_results() })
    
    output$Plot <- renderPlot({
      plot(NA,
           ylim = c(0, 1),
           xlim = c(values$min_x, values$max_x),
           yaxt = "n",
           xaxt = "n",
           ylab = "",
           xlab = "Observed Score (on a scale from -7 to 7)")
      axis(1, at = seq(values$min_x, values$max_x), 
           labels = seq(values$min_x, values$max_x, 1), las = 1)
      abline(v = seq(values$min_x, values$max_x, 1),
             lty = 2,
             col = "grey")
      
      # only plot data if there's something to plot
      if (length(values$means) > 0) {
        dif <- values$means[values$count]
        group <- values$grouplist[values$count]
        
        points(x = dif, y = 0.5, cex = 2, 
               pch = ifelse(group == 1, 16, 15))
      }
    })
  })
  
  ## Save data ----
  data_results <- eventReactive(c(input$noButton, 
                                  input$yesButton, 
                                  input$resetButton),  {
    if (values$count == 0 | length(values$means) == 0) { return("Results will appear here once you have clicked one of the two buttons below. The results will tell you the true effect size and group means that the simulation is based on, the observed difference in your sample, and whether the observed difference differs from zero (p < .05).") }
    
    means <- values$means
    grouplist <- values$grouplist
    
    data <- data.frame(
      "means" = as.numeric(unlist(means)), 
      "grouplist" = as.numeric(unlist(grouplist))
    )

    #Perform t-test and save as z
    z <- t.test(means ~ grouplist, data, var.equal = TRUE)
    
    #Is test significant or not?
    testoutcome<-ifelse(z$p.value<.05,"significant","non-significant")
    
    #Calculate Cohen's d
    d <- z$stat[[1]] * sqrt(sum(grouplist==1)+sum(grouplist==2))/
                       sqrt(sum(grouplist==1)*sum(grouplist==2))
    obs_power <- pwr.t.test(d=d,
                            n=round((sum(grouplist==1)+sum(grouplist==2))/2),
                            sig.level=0.05,
                            type="two.sample")$power
    correct <- ifelse(values$judgement == (values$effect_size > 0),
                      "You made the correct choice.",
                      "You did not make the correct choice.")
    values$correct_trials <- values$correct_trials + 
      ifelse(values$judgement == (values$effect_size > 0), 1, 0)
    
    #Results text
    out <- paste0(correct,
                  " The true effect size in simulation was ",
                  values$effect_size*values$direction,
                  ". The population mean in the circle group was ",
                  (0 + values$shift_es) * values$direction,
                  " and the population mean in the square group was ",
                  (values$effect_size + values$shift_es) * values$direction,
                  ". The observed mean in the circle group was ",
                  round(z$estimate[[1]],2),
                  " and the observed mean in the square group was ",
                  round(z$estimate[[2]],2),
                  " (thus, the observed difference was ",
                  round(z$estimate[[2]],2)-round(z$estimate[[1]],2),
                  "). The null hypothesis significance test was ",
                  testoutcome,
                  ", t(",round(z$parameter[[1]], digits=2),") = ",
                  format(z$stat[[1]], digits = 3, nsmall = 3, scientific = FALSE),
                  ", p = ",format(z$p.value[[1]], digits = 3, nsmall = 3, scientific = FALSE),
                  ", given an alpha of 0.05. You can click the 'Start A New Trial' button to start again. The app will keep track of your performance.")
    
    #results <- list(out = out, d = d, obs_power = obs_power)
    return(out)
  })
  
  ## Generate Plot after Guess
  observeEvent(c(input$noButton, input$yesButton),  {
    if (length(values$means) == 0) { return(F) }
    
    means <- values$means
    grouplist <- values$grouplist
    
    #do t-test
    data <- data.frame(
      "means" = as.numeric(unlist(means)), 
      "grouplist" = as.numeric(unlist(grouplist))
    )
    
    #Perform t-test and save as z
    z <- t.test(means ~ grouplist, data, var.equal = TRUE)
    d <- z$stat[[1]] * sqrt(sum(grouplist==1)+sum(grouplist==2))/
                       sqrt(sum(grouplist==1)*sum(grouplist==2))
    obs_power <- pwr.t.test(d=d,
                            n=round((sum(grouplist==1)+sum(grouplist==2))/2),
                            sig.level=0.05,
                            type="two.sample")$power
    
    outputDir <- "responses"
    
    data <- c(input$ID, 
              length(values$grouplist), 
              values$judgement, 
              values$effect_size, 
              values$effect_size*values$direction, 
              (0 + values$shift_es) * values$direction, 
              (values$effect_size + values$shift_es) * values$direction,
              z$estimate[[1]], z$estimate[[2]], 
              (z$estimate[[2]]-z$estimate[[1]]), 
              z$parameter[[1]], 
              z$stat[[1]], 
              z$p.value[[1]], 
              obs_power, 
              d, 
              as.numeric(unlist(means)), 
              as.numeric(unlist(grouplist))
            )
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", 
                        as.integer(Sys.time()), 
                        digest::digest(data))
    # Write the file to the local system
    write.table(
      x = data,
      file = file.path(outputDir, fileName),
      col.names = FALSE,
      quote = FALSE,
      eol=" "
    )
    
    output$Plot <- renderPlot({
      plot(NA,
           ylim = c(0, 1),
           xlim = c(values$min_x, values$max_x),
           yaxt = "n",
           xaxt = "n",
           ylab = "",
           xlab = "Observed Score (on a scale from -7 to 7)")
      axis(1, at = seq(values$min_x, values$max_x), 
           labels = seq(values$min_x, values$max_x, 1), las = 1)
      abline(v = seq(values$min_x, values$max_x, 1),
             lty = 2,
             col = "grey")
      abline(v = c((0 + values$shift_es) * values$direction, (values$effect_size + values$shift_es) * values$direction),
             lty = 2,
             lwd = 2,
             col = "red")
      points(x = (0 + values$shift_es) * values$direction,
             y = 0.5,
             pch = 16,
             cex = 3,
             col = rgb(1,0,0, alpha = 0.5))
      points(x = (values$effect_size + values$shift_es) * values$direction,
             y = 0.5,
             pch = 15,
             cex = 3,
             col = rgb(1,0,0, alpha = 0.5))
    })
    
  })
  
  ## Sample button actions ----
  observeEvent(input$sampleButton, {
    message("sampleButton()")
    values$count <- values$count + 1
    
    # enable response buttons after 3 observations
    if (length(values$means) > 1) {
      shinyjs::enable("noButton")
      shinyjs::enable("yesButton")
    }
    
    ## set group based on value for sample button (great suggestion by Nick Coles)
    if((length(values$grouplist) %% 2) == 0) {
      group <- 1
    } else {
      group <- 2
    }
    
    values$grouplist[values$count] <- group
    
    ## generate a data point from the group
    if(group == 1){
      x <- rnorm(values$n, (0 + values$shift_es) * values$direction, values$sd)
      if(x > 7){x <- 7} #prevent values more extreme than 7
      if(x < -7){x <- -7}
    } else if(group == 2){
      x <- rnorm(values$n, (values$effect_size + values$shift_es) * values$direction, values$sd)
      if(x > 7){x <- 7}
      if(x < -7){x <- -7}
    }
    values$means[values$count] <- mean(x)
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)