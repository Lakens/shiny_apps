server <- function(input, output) {

  #' Second Generation P-value
  p_delta <- function(lb, ub, delta_lb, delta_ub) {
    
    # special case: infinite CI and H0 bounds in the same direction
    if ((delta_lb == -Inf & lb == -Inf) | (delta_ub == Inf & ub == Inf)) {
      return(1)
    }
    
    # usual case: non-point CI & non-point Ho
    # pdelta = |CI intersect Ho| / min{ |CI|, 2|Ho| }
    if (delta_lb != delta_ub & lb != ub) {
      if (lb > delta_ub | ub < delta_lb) {
        return(0)
      } else if(lb > delta_lb & ub < delta_ub){
        return(1)
      } else {
        return(
          (min(ub, delta_ub) - max(lb, delta_lb)) /
            min(ub - lb, 2 * (delta_ub - delta_lb))
        )
      }
    }
    
    # special case 1: point CI, w/ or w/out a point H0
    # pdelta = 0 if CI is inside the Ho
    # pdelta = 1 if CI is inside the Ho
    if (lb == ub) {
      if (lb <= delta_ub & lb >= delta_lb){
        return(1)
      } else {
        return(0)
      }
    }
    
    # special case 2: point H0 & non-point CI
    # pdelta = 1/2 if H0 is inside the CI
    # pdelta = 0 if H0 is outside the CI
    if (delta_lb == delta_ub & lb != ub) {
      if (delta_lb <= ub & delta_lb >= lb) {
        return(1/2)
      } else {
        return(0)
      }
    }
  }
   output$plot1 <- renderPlot({
     step = 0.01
     
     p_tost_list <- numeric(length(seq(140, 150, step)))
     sgpv_list <- numeric(length(seq(140, 150, step)))
     p_list <- numeric(length(seq(140, 150, step)))
     t_list <- numeric(length(seq(140, 150, step)))
     
     count <- 0
     
     for(i in seq(140, 150, step)){
       count <- count + 1
       m <- i
       mu <- input$mu
       sd <- input$sd
       n <- input$N
       low_eqbound = input$range[1]
       high_eqbound = input$range[2]
       alpha = input$alpha
       
       res <- TOSTone.raw(m = m,
                          mu = mu,
                          sd = sd,
                          n = n,
                          low_eqbound = low_eqbound,
                          high_eqbound = high_eqbound,
                          alpha = alpha,
                          plot = FALSE,
                          verbose = FALSE
       )
       t <- (m - mu)/(sd/sqrt(n))
       t_list[count] <- t
       sgpv_list[count] <- p_delta(mu+res$LL_CI_TTEST, mu+res$UL_CI_TTEST, mu+low_eqbound, mu+high_eqbound)
       p_tost_list[count] <- max(res$TOST_p1, res$TOST_p2)
       p_list[count] <- 2 * pt(-abs(t), df = n-1)
     }

     plot(NA,
          ylim = c(0, 1),
          xlim = c(0, 1001),
          yaxt = "n",
          xaxt = "n",
          ylab = "P-value for TOST and 1-SGPV",
          xlab = "Observed Mean")
     axis(1, at = seq(0,1000,100), labels = seq(140,150,1), las = 1)
     axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1), las = 1)

     lines(sgpv_list, type="l", col = "darkgrey", lwd = 3, lty = 3)
     lines(p_tost_list, lwd = 3)
     abline(h = 0.5, 
            lty = 6,
            col = "lightgrey")
     
   })
   
   output$text1 <- renderText({
     paste("To examine the relation between the TOST p-value and the SGPV we can calculate both statistics across a range of observed effect sizes. In the figure p-values are plotted for the TOST procedure (black line) and the SGPV (grey dashed line). The statistics are calculated for hypothetical one-sample t-tests for all means that can be observed in studies ranging from 140 to 150 (on the x-axis). You can change the values to recreate other plots in the paper. For example, set the sample size to 10, the standard deviation to 2.5, and the equivalence range to -0.4 and 0.4 (while leaving the mean to test against on 145 and the alpha level to 0.05) to see the 'small sample correction' at work, where the SGPV is set to 0.5. Increase the equivalence range to -1 and 1 to see the SGPV level off slightly above the 0.5 value.")
   })

   output$plot2 <- renderPlot({
     step = 0.01
     
     p_tost_list <- numeric(length(seq(140, 150, step)))
     sgpv_list <- numeric(length(seq(140, 150, step)))
     p_list <- numeric(length(seq(140, 150, step)))
     t_list <- numeric(length(seq(140, 150, step)))
     
     count <- 0
     
     for(i in seq(140, 150, step)){
       count <- count + 1
       m <- i
       mu <- input$mu
       sd <- input$sd
       n <- input$N
       low_eqbound = input$range[1]
       high_eqbound = input$range[2]
       alpha = input$alpha
       
       res <- TOSTone.raw(m = m,
                          mu = mu,
                          sd = sd,
                          n = n,
                          low_eqbound = low_eqbound,
                          high_eqbound = high_eqbound,
                          alpha = alpha,
                          plot = FALSE,
                          verbose = FALSE
       )
       t <- (m - mu)/(sd/sqrt(n))
       t_list[count] <- t
       sgpv_list[count] <- p_delta(mu+res$LL_CI_TTEST, mu+res$UL_CI_TTEST, mu+low_eqbound, mu+high_eqbound)
       p_tost_list[count] <- max(res$TOST_p1, res$TOST_p2)
       p_list[count] <- 2 * pt(-abs(t), df = n-1)
     }

          plot(sgpv_list, 
          p_tost_list,
          type="l",
          lwd = 3, 
          ylim = c(0, 1), 
          xlim = c(0, 1),
          # yaxt = "n",
          # xaxt = "n",
          ylab = "TOST p-value",
          xlab = "SGPV")
   })
   
   output$text2 <- renderText({
     paste("The figure above plots the p-values from the TOST procedure (y-axis) direclty against the p-values from the SGPV.")
   })
   
}