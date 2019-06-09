library(shiny)
library(pwr)
library(shinythemes)

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