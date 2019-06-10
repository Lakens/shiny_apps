library(shiny)
library(pwr)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "TEST Your Alpha"),
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
                            title = "Input parameters",
                            selectInput("error", "Minimize or Balance Error Rates?:",
                                        c("Balance" = "balance",
                                          "Minimize" = "minimal"
                                          )),
                            numericInput("costT1T2", "Relative cost Type 1 and Type 2 errors:", 4),
                            numericInput("priorH1H0", "Prior Probability of H1 compared to H0:", 1),
                            textAreaInput("power_function", "Power function:", "pwr::pwr.t.test(d = 0.5, n = 64, sig.level = x, type = 'two.sample', alternative = 'two.sided')$power", width = '400px', height = '200px')
                        ),
                        infoBoxOutput("alpha1Box"),
                        infoBoxOutput("beta1Box")
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
                        infoBoxOutput("alpha2Box")
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
    output$alpha1Box <- renderInfoBox({
        error <- input$error
        power_function <- input$power_function
        costT1T2 <- input$costT1T2
        priorH1H0 <- input$priorH1H0

        f = function(x, power_function, costT1T2 = 1, priorH1H0 = 1, error = "minimal") {
            y <- 1 - eval(parse(text=paste(power_function)))
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
        infoBox(
            "Alpha", paste0(round(alpha1, digits = 5)), icon = icon("percent"),
            color = "purple"
        )
    })

    output$beta1Box <- renderInfoBox({
        error <- input$error
        power_function <- input$power_function
        costT1T2 <- input$costT1T2
        priorH1H0 <- input$priorH1H0
        
        f = function(x, power_function, costT1T2 = 1, priorH1H0 = 1, error = "minimal") {
            y <- 1 - eval(parse(text=paste(power_function)))
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
        x = res$minimum
        beta1 = 1 - eval(parse(text=paste(power_function)))
        objective1 = res$objective
        
        infoBox(
            "Beta", paste0(round(beta1, digits = 5)), icon = icon("percent"),
            color = "green"
        )
    })
    
    output$alpha2Box <- renderInfoBox({
        infoBox(
            "Alpha", paste0(round(input$alpha/sqrt(input$N/input$standardize_N), digits = 5)), icon = icon("percent"),
            color = "purple"
        )
    })
    
}
# Run the application
shinyApp(ui, server)