library(shiny)
library(pwr)
library(ggplot2)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Justify Your Alpha"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Balance/Minimize Error Rates", tabName = "minimize", icon = icon("calculator")),
            menuItem("N as a Function of Sample Size", tabName = "alpha_sample_size", icon = icon("calculator")),
            menuItem("About", tabName = "about", icon = icon("info"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "minimize",
                    fluidRow(
                        box(
                            title = "Input parameters and press 'Calculate'",
                            selectInput("error", "Minimize or Balance Error Rates?:",
                                        c("Balance" = "balance",
                                          "Minimize" = "minimal"
                                          )),
                            numericInput("costT1T2", "Relative cost Type 1 and Type 2 errors:", 4),
                            numericInput("priorH1H0", "Prior Probability of H1 compared to H0:", 1),
                            textAreaInput("power_function", "Power function:", "pwr::pwr.t.test(d = 0.5, n = 64, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power", width = '400px', height = '200px'),
                            actionButton("power_start", "Calculate")
                        ),
                        infoBoxOutput("alpha1Box"),
                        infoBoxOutput("beta1Box"),
                        box(plotOutput("plot1")),
                        box(title = "Explanation",
                            status = "warning", 
                            solidHeader = TRUE, collapsible = TRUE, 
                            "Cohen (1988) considered a Type 1 error rate of 5% and a Type 2 error rate balanced. The reason for this was that instead of weighing both types of errors equally, he felt 'Type I errors are of the order of four times as serious as Type II errors.' This situation is illustrated in the default settings of the app. If the cost of a Type 1 error is 4 times as large as the cost of a Type 2 error, and we collect 64 participants in each condition of a two-sided t-test, that alpha is 0.05 and power is 0.80.", tags$br(), tags$br(),
                            "If we design 2000 studies like this, the number of Type 1 and Type 2 errors we make depend on how often the null hypothesis is true, and how often the alternative hypothesis is true. Let's assume both are equally likely for now. This means that in 1000 studies the null hypothesis is true, and we will make 50 Type 1 errors. In 1000 studies the alternative hypothesis is true, and we will make 100-80 = 20% Type 2 errors, so in 200 studies we will not observe a significant result even if there is a true effect. Combining Type 1 and Type 2 errors, in the long run, we should expect 250 of our 2000 studies to yield an error."
                            ),
                        box(title = "Power functions",
                            status = "info", 
                            solidHeader = TRUE, collapsible = TRUE, 
                            "The trickiest thing of using this Shiny app is entering the correct power function. You can provide an analytic power function, either programmed yourself, or from an existing package loading on the server. Then, make sure the alpha value is not set, but specified as x, and that the function itself returns a single value, the power of the test. Finally, if you use existing power functions the shiny app needs to know which package this function is from, and thus the call to the function needs to be precended by the package and '::', so 'pwr::' or 'TOSTER::' or 'ANOVApower::'. Some examples that work are provided below.", tags$br(), tags$br(),
                            "TOSTER::powerTOSTtwo(alpha=x, N=200, low_eqbound_d=-0.4, high_eqbound_d=0.4)", tags$br(), tags$br(),
                            "pwr::pwr.anova.test(n = 100, k = 2, f = 0.171875, sig.level = x)$power", tags$br(), tags$br(),
                            "For a more challenging power function, we can use the ANOVApower package by myself and Aaron Caldwell. The power function in the ANOVAexact function is based on a simulation, which takes a while to perform. The optimization function used in this Shiny app needs to perform the power calculation multiple times. Thus, the result takes a while to calculate. Furthermore, the output of the ANOVA_exact function is power as 80%, not 0.8, and thus we actually have to divide the power value by 100 for the Shiny app to return the correct results. Nevertheless, it works.", tags$br(), tags$br(),
                            "ANOVApower::ANOVA_exact(ANOVApower::ANOVA_design(design = '2b', n = 100, mu = c(24, 26.2), sd = 6.4))$main_results$power/100"
                            )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "alpha_sample_size",
                    fluidRow(
                        box(
                            title = "Input parameters",
                            numericInput("alpha", "Alpha level:", 0.05),
                            numericInput("N", "Sample Size:", 200),
                            numericInput("standardize_N", "Standardize N:", 100)
                        ),
                        infoBoxOutput("alpha2Box"),
                        box(title = "Explanation",
                            status = "warning", 
                            solidHeader = TRUE, collapsible = TRUE, 
                            "The idea behind this recommendation is discussed most extensively by Leamer, 1978. He writes 'The rule of thumb quite popular now, that is, setting the significance level arbitrarily to .05, is shown to be deficient in the sense that from every reasonable viewpoint the significance level should be a decreasing function of sample size.' This was already recognized by Jeffreys (1939), who discusses ways to set the alpha level in Neyman-Pearson statistics: 'We should therefore get the best result, with any distribution of alpha, by some form that makes the ratio of the critical value to the standard error increase with n. It appears then that whatever the distribution may be, the use of a fixed P limit cannot be the one that will make the smallest number of mistakes.'", tags$br(), tags$br(),
                            "The goal is to prevent Lindley's paradox (https://en.wikipedia.org/wiki/Lindley%27s_paradox). I explain this in more detail in week 1 in my MOOC (https://www.coursera.org/learn/statistical-inferences).", tags$br(), tags$br(),
                            "To prevent Lindley's paradox one would need to lower the alpha level as a function of the statistical power. Good (1992) notes: 'we have empirical evidence that sensible P values are related to weights of evidence and, therefore, that P values are not entirely without merit. The real objection to P values is not that they usually are utter nonsense, but rather that they can be highly misleading, especially if the value of N is not also taken into account and is large.' Based on the observation by Jeffrey’s (1939) that, under specific circumstances, the Bayes factor against the null hypothesis is approximately inversely proportional to the square root of N, Good (1982) suggests a standardized p-value to bring p-values in closer relationship with weights of evidence. This formula standardizes the p-value to the evidence against the null hypothesis that would be observed if the standardized p-value was the tail area probability observed in a sample of 100 participants.", tags$br(), tags$br(),
                            "This Shiny app uses the same formula, but calculates the standardized alpha. As the sample size increases beyond N = 100, the alpha level decreases, reflecting the idea that with greater sample sizes, one needs a more stringent alpha level (or a higher level of evidence, in the Bayesian sense)."
                        )
                    )
            ),
            
            # Third tab content
            tabItem(tabName = "about",
                    h2("Justify Your Alpha: A Practical Guide"),
                    h4("For an explanation why researchers should justify their alpha levels, see:"),
                    h4("Lakens, D., Adolfi, F. G., Albers, C. J., Anvari, F., Apps, M. A. J., Argamon, S. E., … Zwaan, R. A. (2018). Justify your alpha. Nature Human Behaviour, 2, 168–171. https://doi.org/10.1038/s41562-018-0311-x"),
                    h4("You can download the pre-print of this article at ", a("PsyArXiV", href="https://psyarxiv.com/9s3y6/")),
                    h4("For a short introduction in why to lower your alpha level as a function of the sample size, see my ", a("blog post", href="http://daniellakens.blogspot.com/2018/12/testing-whether-observed-data-should.html"), ". For a short introduction on why and how to balance or minimize error rates, see my ", a("other blog post", href="http://daniellakens.blogspot.com/2019/05/justifying-your-alpha-by-minimizing-or.html"),"."),
                    h4("Get the code at ", a("GitHub", href="https://github.com/Lakens/JustifieR")),
                    h4("The best way to cite this app and the explanations of how to justify alpha levels in practice is through the preprint:"),
                    h4("Lakens, D. (2019). Justify Your Alpha: A Practical Guide.")
            )
            
        )
    )
)

server <- function(input, output) {
    stats <- reactive({ 
        input$power_start
        error <- input$error
        power_function <- isolate(input$power_function)
        costT1T2 <- input$costT1T2
        priorH1H0 <- input$priorH1H0
        
        f = function(x, power_function, costT1T2 = 1, priorH1H0 = 1, error = "minimal") {
            tryCatch({
                y <- 1 - eval(parse(text=paste(power_function)))
            }, error = function(e) {
                stop("The function does not work")
            })
            if(error == "balance"){
                max((costT1T2*x - priorH1H0*y)/(priorH1H0+1), (priorH1H0*y - costT1T2*x)/(priorH1H0+1))
            } else if (error == "minimal"){
                (costT1T2*x + priorH1H0*y)/(priorH1H0+1)
            }
        }
        
        #Run optimize to find the minimum
        res <- stats::optimize(f,
                               c(0, 1),
                               tol = 0.00001,
                               power_function = power_function,
                               costT1T2 = costT1T2,
                               priorH1H0 = priorH1H0,
                               error = error)
        if(error == "balance"){
            beta <- res$minimum - res$objective
        } else if (error == "minimal"){
            beta <- res$objective - res$minimum
        }
        alpha1 = res$minimum
        x = res$minimum
        beta1 = 1 - eval(parse(text=paste(power_function)))
        objective1 = res$objective
        
        list(alpha1 = format(alpha1, digits = 10, nsmall = 5, scientific = FALSE),
             beta1 = format(beta1, digits = 10, nsmall = 5, scientific = FALSE)
        )
    })
    
    output$alpha1Box <- renderInfoBox({
        infoBox(
            "Alpha", stats()$alpha1, icon = icon("percent"),
            color = "purple"
        )
    })
    
    output$beta1Box <- renderInfoBox({
        infoBox(
            "Beta", stats()$beta1, icon = icon("percent"),
            color = "green"
        )
    })
    
    output$alpha2Box <- renderInfoBox({
        infoBox(
            "Alpha", paste0(round(input$alpha/sqrt(input$N/input$standardize_N), digits = 10)), icon = icon("percent"),
            color = "purple"
        )
    })
    observeEvent(input$power_start,  {
      
      output$plot1 <- renderPlot({
        power_function <- isolate(input$power_function)
        costT1T2 <- input$costT1T2
        priorH1H0 <- input$priorH1H0
  
        alpha_level <- 0
        alpha_list <- numeric(9999)
        beta_list <- numeric(9999)
        w_list <- numeric(9999)
        w_c_list <- numeric(9999)
        for(i in 1:9999) {
          alpha_level <- alpha_level + 0.0001
          alpha_list[i] <- alpha_level
          x <- alpha_level
          beta_list[i] <- 1 - eval(parse(text=paste(power_function)))
          w_list[i] <- (alpha_level + beta_list[i]) / 2
          w_c_list[i] <- (costT1T2 * alpha_level + priorH1H0 * beta_list[i]) / (costT1T2 + priorH1H0)
        }
        
        # Create dataframe for plotting
        plot_data <- data.frame(alpha_list, beta_list, w_list, w_c_list)
        
        w_c_alpha_plot <- ggplot(data=plot_data, aes(x=alpha_list, y=w_c_list)) +
          geom_line(size = 1.3) +
          #geom_point(aes(x = 0.05, y = (costT1T2 * 0.05 + priorH1H0 * (1 - eval(parse(text=paste(power_function))))) / (priorH1H0 + costT1T2)), color="red", size = 3) +
          theme_minimal(base_size = 18) +
          scale_x_continuous("alpha", seq(0,1,0.1)) +
          scale_y_continuous("weighted combined error rate", seq(0,1,0.1), limits = c(0,1))
        w_c_alpha_plot
      })
    })


}
# Run the application
shinyApp(ui, server)