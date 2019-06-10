# Libraries ----
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)

# Define UI ----

# . Sidebar ----
sidebar <- dashboardSidebar(
  h4("How much larger is group B than group A?"),
  # . . d_guess slide ----
  sliderInput("d_guess", "My effect size (d) guess",
              min = -3, max = 3, value = 0, step = 0.05),
  # . . submit_guess button ----
  actionButton("submit_guess", "Submit Guess"),
  # . . new_sim button ----
  actionButton("new_sim", "Simulate a new dataset"),
  
  # . . plot display options ----
  h4("Display Options"),
  checkboxInput("show_violin", "Violin Plot",value = T),
  checkboxInput("show_boxplot", "BoxPlot",value = F),
  checkboxInput("show_points", "Points",value = T)
)

# . Body ----
body <- dashboardBody(
  p("This app will show you a graph of simulated data with a random number of observations in each of two groups and a random effect size. The effect size will be between -3 and 3, your job is to guess the size of the effect."),
  fluidRow( # start row 1
    column( # start column 1
      width = 6,
      box(
        width = NULL,
        # . . current_n output ----
        textOutput("current_n"),
        # . . current_plot plot ----
        plotOutput("current_plot"),
        # . . current_es output ----
        textOutput("current_es")
      )
    ), # end column 1
    column( # start column 2
      width = 6,
      box(
        title = "Your guessing performance",
        width = NULL,
        # . . performance_plot plot ----
        plotOutput("performance_plot")
      )
    )
  )
)

# . dashboardPage ----
ui <- dashboardPage(
  dashboardHeader(title = "Guess"),
  sidebar,
  body
)

# Set reactiveValues ----
app_vals <- reactiveValues(
  guess_text = "",
  guess = c(),
  real = c()
)

# Define server logic ----
server <- function(input, output, session) {

  # . Set random parameters and simulate data ----
  simdata <- eventReactive(input$new_sim, {
    # reset guess_text
    app_vals$guess_text <- ""

    # set sample N between 10 and 500
    Ns <- c(seq(10,100,by = 10),seq(200,500,by = 100))
    app_vals$n <- sample(Ns, 1)

    # set sample effect size
    app_vals$es <- sample(seq(-3,3,by = 0.05), 1)

    # set offset (so one group isn't always at 0)
    app_vals$offset <- sample(seq(-1,1,by = 0.1), 1)

    # simulate data
    dat <- data.frame(
      A = rnorm(app_vals$n, app_vals$offset, 1),
      B = rnorm(app_vals$n, app_vals$offset + app_vals$es, 1)
    ) %>%
      gather(group, val, A:B)

    return(dat)
  }, ignoreNULL = FALSE)

  # . Show current_n ----
  output$current_n <- renderText(
    paste0("The graph below shows ",
           app_vals$n,
           " observations in each group.")
  )

  # . Show current_es ----
  output$current_es <- renderText(app_vals$guess_text)

  # . Save guess ----
  save_guess <- eventReactive(input$submit_guess, {
    app_vals$guess_text <- paste0("You guessed the effect size was d = ",
                             input$d_guess, ". ",
                             "The graph above shows an effect size of d =",
                             app_vals$es, ".")

    app_vals$guess[input$submit_guess] <- input$d_guess
    app_vals$real[input$submit_guess] <- app_vals$es

    dat <- data.frame(
      guess = app_vals$guess,
      real = app_vals$real
    )

    return(dat)
  })

  # . Generate current_plot ----
  output$current_plot <- renderPlot({
    p <- simdata() %>%
      ggplot(aes(group, val, color = group)) +
      ylim(-6, 6) +
      ylab("") +
      scale_colour_manual(values = c("red", "steelblue3")) +
      theme_minimal()

    if (input$show_points) {
      p <- p + geom_jitter(show.legend = F)
    }
    if (input$show_violin) {
      p <- p + geom_violin(draw_quantiles = 0.5,
                           alpha = 0.3, show.legend = F)
    }
    if (input$show_boxplot) {
      p <- p + geom_boxplot(width = 0.25, alpha = 0.3, show.legend = F)
    }

    p
  })

  # . Generate performance_plot ----
  output$performance_plot <- renderPlot({
    save_guess() %>%
      ggplot(aes(real, guess)) +
      geom_abline(slope = 1, intercept = 0, color = "grey30") +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("The actual effect size (d)") +
      ylab("Your guessed effect size (d)") +
      coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)

