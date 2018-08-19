library(shiny)
library(shinyjs)
library(pwr)
library(V8)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

sd <- 1
n <- 1
min_x <- -10
max_x <- 10


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(), #add useShinyjs to be able to disable buttons upon making a choice.
  extendShinyjs(text = jsResetCode),  
  # Application title
   titlePanel("Guess The Effect"),
   
      # Show a plot of the generated distribution
      mainPanel(
        h4("Sample a new datapoint until you feel sufficiently certain that there is a real difference or not"),
        actionButton("trialButton", "Start a New Data Collection Trial"),
        h4(uiOutput("display_effectsize")),
        h4(uiOutput("display_condition")),
        actionButton("sampleButton", "Sample a new datapoint"),
        h4(uiOutput("displayCounter")),
        h4(uiOutput("display_group")),
        h4(uiOutput("means")),
        h4(uiOutput("grouplist")),
        plotOutput("Plot"),
        actionButton("yesButton", "I think the circle and square groups are equal"),
        actionButton("noButton", "I think the circle and square groups are different"),
        h4(uiOutput("results"))
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$trialButton,  {
    js$reset()
  })
 
  # condition <- eventReactive(input$trialButton, {
  #   values$condition <- sample(c(0,1),1,1)
  #   return(values$condition)
  # })
  condition <- reactiveValues(condition = sample(c(0,1),1,1)) 
  values <- reactiveValues(effect_size = 9, 
                           condition = 1, 
                           group = 1, 
                           means = list(),
                           grouplist = list())

  output$displayCounter <- renderText({
    c("Number of Datapoints Sampled:", updateCounter())
  })
  output$display_effectsize <- renderText({
    c("Effect Size:", effect_size())
  })
  output$display_condition <- renderText({
    c("Condition (if 0, no difference, if 1, difference):", condition())
  })
  output$display_group <- renderText({
    c("Group:", group())
  })
  
  #Result from the final choice
  output$display_result1 <- renderText({
    c("Result:", result1())
  })
  output$display_result2 <- renderText({
    c("Result:", result2())
  })
  output$means <- renderText({
    c("Means:", unlist(means()))
  })
  output$grouplist <- renderText({
    c("Group List:", unlist(grouplist()))
  })
  output$results <- renderText({
    c("Results:", data_results())
  })
  
  updateCounter <- reactive({
    if (input$sampleButton == 0) {k <- 0}
    else {k <- input$sampleButton}
    return(k)
  })
  
  #Disable buttons (except new trial button) after choice is made
  observeEvent(input$noButton,  {
    shinyjs::disable("yesButton")
    shinyjs::disable(id = "sampleButton")
  })
  observeEvent(input$yesButton,  {
    shinyjs::disable("noButton")
    shinyjs::disable(id = "sampleButton")
  })
  
  #Clicking the answer buttons will end the trial and store the data
  result1 <- eventReactive(input$yesButton, {
    answer<-1
    return(answer)
  })
  result2 <- eventReactive(input$noButton, {
    answer<-2
    return(answer)
  })
  #save data
  observeEvent(input$yesButton,  {
    outputDir <- "responses"
    judgement <- 1
    data <- data.frame(judgement, condition(), effect_size(), means(), grouplist())
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
  })
  observeEvent(input$noButton,  {
    outputDir <- "responses"
    judgement <- 0
    data <- data.frame(judgement, condition(), effect_size(), means(), grouplist())
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
  })
  
  data_results <- eventReactive(input$noButton,  {
    #bind data into dataframe
    data_results <- data.frame(as.numeric(unlist(means())), as.numeric(unlist(grouplist())))
    colnames(data_results) <- c("means", "grouplist")
    #Perform t-test and save as z
    z <- t.test(means ~ grouplist, data_results, var.equal = TRUE)
    #Is test significant or not?
    testoutcome<-ifelse(z$p.value<.05,"significant","non-significant")
    #Calculate Cohen's d
    d <- z$stat[[1]] * sqrt(sum(grouplist()==1)+sum(grouplist()==2))/sqrt(sum(grouplist()==1)*sum(grouplist()==2))
    obs_power <- pwr.t.test(d=d,n=round((sum(grouplist()==1)+sum(grouplist()==2))/2),sig.level=0.05,type="two.sample")$power
    #Give results
    out <- paste("The null hypothesis significance test was ",testoutcome,", t(",round(z$parameter[[1]], digits=2),") = ",format(z$stat[[1]], digits = 3, nsmall = 3, scientific = FALSE),", p = ",format(z$p.value[[1]], digits = 3, nsmall = 3, scientific = FALSE),", given an alpha of 0.05. Based on the data you sampled you had ",round(obs_power,2),"% power to detect a difference, based on an observed effect size of d = ",round(d,2),".",sep="")
    #results <- list(out = out, d = d, obs_power = obs_power)
    return(out)
  })
  
  group <- eventReactive(input$sampleButton, {
    values$group <- sample(c(1,2),1,1)
    return(values$group)
  })
  
  effect_size <- eventReactive(input$trialButton, {
    sd*1
  })
  
  dif <- eventReactive(input$sampleButton, {
    
    if(values$condition == 0){
      x <- rnorm(n, 0, sd)
    }
    if(values$condition == 1){
      x <- rnorm(n, effect_size(), sd)
    }
    
    y <- rnorm(n, 0, sd)
    mean(x) - mean(y)
  })
  
  means <- eventReactive(input$sampleButton, {
    values$means[input$sampleButton] <- dif()
    return(values$means)
  })
  
  grouplist <- eventReactive(input$sampleButton, {
    values$grouplist[input$sampleButton] <- group()
    return(values$grouplist)
  })
  
    
    
  output$Plot <- renderPlot({
    plot(NA, 
         ylim = c(0, 1), 
         xlim = c(min_x, max_x),
         yaxt = "n",
         xaxt = "n",
         ylab = "",
         xlab = "Observed Difference")
    axis(1, at = seq(min_x, max_x), labels = seq(min_x, max_x, 1), las = 1)
    abline(v = seq(min_x, max_x, 1),
           lty = 2,
           col = "grey")
    if(group() == 1){
      points(x = dif(), 
             y = 0.5, 
             pch = 16, 
             cex = 2)
    }
    if(group() == 2){
      points(x = dif(), 
             y = 0.5, 
             pch = 15, 
             cex = 2)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

