# R code to plot graphs of the effect of correlated predictors on beta coefficients in regression.
# Written by Nick Brown (nicholasjlbrown@gmail.com), 2017-2019.

# This code was inspired by the figures in Friedman & Wall, 2005 (10.1198/000313005X41337).
# It plots the evolution of the beta coefficients and R-squared in the regression
#  lm(Y ~ X1 + X2)
# as the correlation between X1 and X2 varies from (large and negative) to (large and positive).

# Version history
# 2018-07-05 21:56Z 0.01
#   First numbered version released.
# 2018-07-06 14:08Z 0.02
#   Rearranged legend elements to avoid misalignment of partial and semipartial r1/r2.
#   Allowed plot to take up full width of window.
#   Fixed a bug with the rX1X2 limits slider when the correlation product is negative.
# 2019-01-11 13:03Z 0.03
#   Added the ability to hover within the plot to show the exact values at any point.
# 2019-10-06 00:56Z 0.04
#   Added variance inflation factor (VIF) calculation.
# 2019-10-06 01:44Z 0.05
#   Fixed VIF calculation (!).

# To-do:
#   Add help text.
#   Enforce minimum ranges for sliders.

useShiny <- TRUE
#useShiny <- FALSE
if (useShiny) {
  library(shiny)
}

library(ggplot2)

# Things you are likely to want to change.
rYX1 <- 0.53                  # first predictor-outcome correlation
rYX2 <- 0.23                  # second predictor-outcome correlation
# Note: if one of your correlations is actually negative, make it positive here and
#  read the value corresponding to the opposite-signed rX1X2 from the plot.

# Things you are less likely to want to change.
rmin <- -0.80                 # lowest negative predictor-predictor correlation to include; overridden in the Shiny version
rmax <- 0.90                  # highest positive predictor-predictor correlation to include; overridden in the Shiny version
# Adjust the above values by a small amount if the Y-axis gets out of hand.

# Things that change the look of the output.
detail <- 600                 # number of points per half X-axis; increase for slightly smoother curves
show.partial_r <- FALSE       # TRUE: include curves for partial correlations, by default
show.semipartial_r <- FALSE   # TRUE: include curves for semipartial correlations, by default
fw.labels <- FALSE            # TRUE: label regions like Friedman & Wall (10.1198/000313005X41337) did

col.beta1 <- "#0000FF"
col.beta2 <- "#00FFFF"
col.partial_r1 <- "#FF0000"
col.partial_r2 <- "#FF69B4"
col.semipartial_r1 <- "#FFA000"
col.semipartial_r2 <- "#FFFF00"
col.Rsq <- "#00AA00"

betasEtc <- function (rYX1, rYX2, rX1X2) {
  num1 <- (rYX1 - (rYX2 * rX1X2))
  num2 <- (rYX2 - (rYX1 * rX1X2))

  denom_beta <- 1 - (rX1X2 ^ 2)
  beta1 <- num1 / denom_beta
  beta2 <- num2 / denom_beta

  denom_semipartial_r1 <- sqrt(1 - (rYX2 ^ 2))
  denom_semipartial_r2 <- sqrt(1 - (rYX1 ^ 2))

  partial_r1 <- num1 / (denom_semipartial_r1 * sqrt(denom_beta))
  partial_r2 <- num2 / (denom_semipartial_r2 * sqrt(denom_beta))

  semipartial_r1 <- num1 / sqrt(denom_beta)
  semipartial_r2 <- num2 / sqrt(denom_beta)

  Rsq <- (rYX1 * beta1) + (rYX2 * beta2)
# We could also calculate RÂ² with the following formulae, which give the same result:
#Rsq <- (rYX1^2 + rYX2^2 - (2*rYX1*rYX2*rX1X2)) / denom_beta
#Rsq <- beta1^2 + beta2^2 + (2*beta1*beta2*rX1X2)

  result <- list(beta1=beta1, beta2=beta2, partial_r1=partial_r1, partial_r2=partial_r2, semipartial_r1=semipartial_r1, semipartial_r2=semipartial_r2, Rsq=Rsq)
  return(result)
}

fixedw <- function (x, dp=3, striplz=TRUE) {
  dpf <- paste("%.", dp, "f", sep="")
  result <- sprintf(dpf, x)

#  if (striplz) {        # remove leading zero (for numbers that cannot reach +/- 1.0)
#    result <- sub("0.", ".", result, fixed=TRUE)
#  }

  if (x >= 0) {         # add space to non-negative numbers to ensure fixed width for all values
    result <- paste(" ", result, sep="")
  }

  return(result)
}

text_values <- function (rYX1, rYX2, rX1X2, show.partial_r, show.semipartial_r, go) {
  if (go == 0) {                        # first time round, nothing to show
    result <- ""
  }
  else if (length(rX1X2) == 0) {        # cursor is not inside the plot
    result <- "Hover cursor within the plot to show values at any point along the X-axis"
  }
  else {
    result <- ""
    values <- betasEtc(rYX1, rYX2, rX1X2)

    result <- paste(result, "rYX1=", fixedw(rYX1), "   ", sep="")
    result <- paste(result, "rYX2=", fixedw(rYX2), "   ", sep="")
    result <- paste(result, "rX1X2=", fixedw(rX1X2), "   ", sep="")

    vif <- solve(matrix(c(1, rX1X2, rX1X2, 1), ncol=2))[1, 1]
    result <- paste(result, "VIF=", fixedw(vif), "   ", sep="")

    result <- paste(result, "β1=", fixedw(values[["beta1"]], striplz=FALSE), "   ", sep="")
    result <- paste(result, "β2=", fixedw(values[["beta2"]], striplz=FALSE), "   ", sep="")
    result <- paste(result, "R²=", fixedw(values[["Rsq"]]), "   ", sep="")

    if (show.partial_r) {
      result <- paste(result, "p.r1=", fixedw(values[["partial_r1"]]), "   ", sep="")
      result <- paste(result, "p.r2=", fixedw(values[["partial_r2"]]), "   ", sep="")
    }

    if (show.semipartial_r) {
      result <- paste(result, "s.r1=", fixedw(values[["semipartial_r1"]]), "   ", sep="")
      result <- paste(result, "s.r2=", fixedw(values[["semipartial_r2"]]), "   ", sep="")
    }
  }

  return (result)
}

# Calculate minimum/maximum rX1X2 (cf. Friedman & Wall, 2005, p. 134).
rX1X2_limits <- function (rYX1, rYX2, round.dp=-1) {
  rmc1 <- rYX1 * rYX2
  rmc2 <- sqrt((1 - rYX1 ^ 2) * (1 - rYX2 ^ 2))
  limits <- c(rmc1 - rmc2, rmc1 + rmc2)

  if (round.dp >= 0) {
    delta <- 5 * (10 ^ (-(round.dp + 1)))    # round to next smallest multiple of (10 ^ -round.dp)
    limits <- round(limits + c(delta, -delta), round.dp)
  }

  return(limits)
}

make_fw_plot <- function (rYX1, rYX2, rmin, rmax, show.partial_r, show.semipartial_r, fw.labels) {
  base_Rsq <- -99.0
  base_partial_r1 <- -99.0
  base_partial_r2 <- -99.0
  base_semipartial_r1 <- -99.0
  base_semipartial_r2 <- -99.0
  suppr.line <- -99.0
  prevRsq <- 0.0
  Rsq.line <- 0.0
  prevbeta2 <- 0.0

  reverse <- if ((rYX1 * rYX2) < 0) -1 else 1

# We perform all the calculations for all-positive correlations and correct when displaying
  rYX1 <- abs(rYX1)
  rYX2 <- abs(rYX2)

# We define X1 as the predictor with the largest correlation with the outcome.
  if (rYX1 < rYX2) {
    x <- rYX2
    rYX2 <- rYX1
    rYX1 <- x
  }

# Limit user-supplied minimum/maximum rX1X2 to valid values.
# This is redundant in the Shiny version as the input slider limits the values, but hey, "belt and braces".
  rlim <- rX1X2_limits(rYX1, rYX2)
  rmin <- max(rmin, rlim[1])
  rmax <- min(rmax, rlim[2])

# Classical suppression is typically defined as "rYX2 equals zero", but what exactly is "zero"?
  negtype <- if (abs(rYX2) <= 0.01) "Classical" else "Negative"

  rlabels <- c("Reciprocal\nsuppression", "Redundancy", paste(negtype, "\nsuppression", sep=""))
  if (fw.labels) {
    rlabels <- c("Region 1\nEnhancement", "Region 2\nRedundancy", "Region 3\nSuppression", "Region 4\nEnhancement")
  }
  rlabels <- paste(rlabels, "   ")                  # easiest way to ensure guide item separation
  region.labels <- factor(rlabels, levels=rlabels)  # factors in alphabetical order

  r12min <- round(rmin * detail)
  r12max <- round(rmax * detail)

  vsize <- r12max + 1 - r12min
  crX1X2 <- rep(NA, vsize)
  cbeta1 <- rep(NA, vsize)
  cbeta2 <- rep(NA, vsize)
  cRsq <- rep(NA, vsize)

  if (show.partial_r) {
    cpartial_r1 <- rep(NA, vsize)
    cpartial_r2 <- rep(NA, vsize)
  }

  if (show.semipartial_r) {
    csemipartial_r1 <- rep(NA, vsize)
    csemipartial_r2 <- rep(NA, vsize)
  }

  v <- 0
  for (r12 in r12min:r12max) {
    v <- v + 1
    rX1X2 <- r12 / detail
    values <- betasEtc(rYX1, rYX2, rX1X2)

    beta1 <- values[["beta1"]]
    beta2 <- values[["beta2"]]
    partial_r1 <- values[["partial_r1"]]
    partial_r2 <- values[["partial_r2"]]
    semipartial_r1 <- values[["semipartial_r1"]]
    semipartial_r2 <- values[["semipartial_r2"]]
    Rsq <- values[["Rsq"]]

    if (r12 == 0) {
      base_Rsq <- Rsq         # remember R² value when rX1X2 is zero, so we can detect it again later

      if (show.partial_r) {
        base_partial_r1 <- partial_r1
        base_partial_r2 <- partial_r2
      }

      if (show.semipartial_r) {
        base_semipartial_r1 <- semipartial_r1
        base_semipartial_r2 <- semipartial_r2
      }
    }
    else {
      if ((prevRsq <= base_Rsq) && (Rsq >= base_Rsq)) {
        Rsq.line <- rX1X2   # border between suppression and enhancement (cf. Friedman & Wall)
      }
    }

    if (    (suppr.line < 0.0)
         && (    ((prevbeta2 * beta2) < 0.0)
              || (beta2 == 0.0)
            )
       ) {
      suppr.line <- rX1X2   # border between redundancy and enhancement
    }

    crX1X2[v] <- rX1X2
    cbeta1[v] <- beta1
    cbeta2[v] <- beta2
    cRsq[v] <- Rsq

    if (show.partial_r) {
      cpartial_r1[v] <- partial_r1
      cpartial_r2[v] <- partial_r2
    }

    if (show.semipartial_r) {
      csemipartial_r1[v] <- semipartial_r1
      csemipartial_r2[v] <- semipartial_r2
    }

    prevRsq <- Rsq
    prevbeta2 <- beta2
  }

# Min/max limits for axes
  ymin <- min(round((min(cbeta2) - 0.1), 1), 0)
  ymax <- round(max(cbeta1), 1) + 0.1
  xmin <- -1.0
  xmin <- round((rmin - 0.1), 1)
  xmax <- 1.0
  xmax <- round(rmax, 1) + 0.1

#I# For an explanation of the #I# code, see https://stackoverflow.com/questions/50594057/how-can-i-determine-the-x-and-y-coordinates-of-the-corners-in-ggplot2 for
  ycmin <- ymin
  ycmax <- ymax
  xcmin <- xmin
  xcmax <- xmax
#I#  ycmin <- -Inf
#I#  ycmax <- Inf
#I#  xcmin <- -Inf
#I#  xcmax <- Inf

  xbreaks <- (seq(round(xmin * 10), round(xmax * 10), 1) / 10) * reverse
  ybreaks <- seq(round(ymin * 10), round(ymax * 10), 1) / 10

  df <- as.data.frame(cbind(crX1X2 * reverse, cbeta1, cbeta2, cRsq))
  names(df) <- c("rX1X2", "beta1", "beta2", "Rsq")

  if (show.partial_r) {
    df$partial_r1 <- cpartial_r1
    df$partial_r2 <- cpartial_r2
  }

  if (show.semipartial_r) {
    df$semipartial_r1 <- csemipartial_r1
    df$semipartial_r2 <- csemipartial_r2
  }

  linesize <- 0.9
  linesize.dotted <- 0.5

  suppr.xcmax <- if (fw.labels) Rsq.line else xcmax
  xp <- c(xcmin, 0, 0, xcmin,
          0, suppr.line, suppr.line, 0,
          suppr.line, suppr.xcmax, suppr.xcmax, suppr.line
  )
  poly.fill <- c("#C0C0FF", "#C0FFC0", "#FFA040")

  if (fw.labels) {
    xp <- c(xp, Rsq.line, xcmax, xcmax, Rsq.line)
    poly.fill <- c(poly.fill, "#FFA0A0")
  }

  n.poly <- length(poly.fill)
  ids <- factor(c(1:n.poly))
  yp <- rep(c(ycmin, ycmin, ycmax, ycmax), n.poly)
  poly.coords <- data.frame(id=rep(ids, each=4), x=(xp * reverse), y=yp)
  poly.labels <- data.frame(id=ids, label=region.labels)
  poly.data <- merge(poly.labels, poly.coords, by="id")

  xlim <- c(xmin, xmax)
  if (reverse < 0) {
    xlim <- -rev(xlim)
  }
  ylim <- c(ymin, ymax)

  gg <- ggplot(df, aes(x=rX1X2)) +
    coord_cartesian(xlim=xlim, ylim=ylim) +
    scale_x_continuous(breaks=xbreaks
                      , expand=c(0, 0)          #I# comment this out
    ) +
    scale_y_continuous(breaks=ybreaks, name="βs and R²"
                      , expand=c(0, 0)          #I# comment this out
                      ) +
    geom_line(aes(y=beta1, colour="beta1"), size=linesize) +
    geom_line(aes(y=beta2, colour="beta2"), size=linesize) +
    geom_line(aes(y=Rsq, colour="Rsq"), size=linesize) +
    geom_polygon(data=poly.data, aes(x=x, y=y, fill=label, group=id), alpha=0.15) +
    scale_fill_manual(values=poly.fill, name="", breaks=region.labels) +
    geom_hline(aes(yintercept=rYX1), colour=col.beta1, linetype="dotted", size=linesize.dotted) +
    geom_hline(aes(yintercept=rYX2), colour=col.beta2, linetype="dotted", size=linesize.dotted) +
    geom_hline(aes(yintercept=base_Rsq), colour=col.Rsq, linetype="dotted", size=linesize.dotted) +
    geom_hline(aes(yintercept=0.0)) +
    geom_vline(aes(xintercept=0.0)) +
    theme(legend.position="top"
        , legend.direction="horizontal"
        , legend.title=element_text(size=14)
        , legend.text=element_text(size=14)
        , axis.title=element_text(size=14)
    ) +
    NULL

  if (suppr.line >= 0.0) {
    gg <- gg + geom_vline(aes(xintercept=(suppr.line * reverse)))
  }

  if (fw.labels) {
    gg <- gg + geom_vline(aes(xintercept=(Rsq.line * reverse)))
  }

  scm.limits <- c("beta1", "beta2")
  scm.labels <- c("β1", "β2")
  scm.values <- c(col.beta1, col.beta2)

  if (show.partial_r) {
    scm.limits <- c(scm.limits, "p.r1", "p.r2")
    scm.labels <- c(scm.labels, "p.r1", "p.r2")
    scm.values <- c(scm.values, col.partial_r1, col.partial_r2)

    gg <- gg +
      geom_line(aes(y=partial_r1, colour="p.r1"), size=linesize) +
      geom_hline(aes(yintercept=base_partial_r1), colour=col.partial_r1, linetype="dotted", size=linesize.dotted) +
      geom_line(aes(y=partial_r2, colour="p.r2"), size=linesize) +
      geom_hline(aes(yintercept=base_partial_r2), colour=col.partial_r2, linetype="dotted", size=linesize.dotted)
  }

  if (show.semipartial_r) {
    scm.limits <- c(scm.limits, "s.r1", "s.r2")
    scm.labels <- c(scm.labels, "s.r1", "s.r2")
    scm.values <- c(scm.values, col.semipartial_r1, col.semipartial_r2)

    gg <- gg +
      geom_line(aes(y=semipartial_r1, colour="s.r1"), size=linesize) +
      geom_hline(aes(yintercept=base_semipartial_r1), colour=col.semipartial_r1, linetype="dotted", size=linesize.dotted) +
      geom_line(aes(y=semipartial_r2, colour="s.r2"), size=linesize) +
      geom_hline(aes(yintercept=base_semipartial_r2), colour=col.semipartial_r2, linetype="dotted", size=linesize.dotted)
  }

  scm.limits <- c(scm.limits, "Rsq")
  scm.labels <- c(scm.labels, "R²")
  scm.values <- c(scm.values, col.Rsq)

  gg <- gg + scale_colour_manual(
    paste("rYX1=", sprintf("%.2f", rYX1), "  \n", "rYX2=", sprintf("%.2f", rYX2), sep=""),
    limits=scm.limits,
    labels=scm.labels,
    values=scm.values
  )

  return(gg)
}

server <- function (input, output, session) {
  output$plot <- renderPlot({
    if (input$go > prevGo) {    # user clicked the Go button
      prevGo <<- input$go
    }
    else {
      return()           # this clears the plot area
    }

    isolate({
      reverse <- if ((input$rYX1 * input$rYX2) < 0) -1 else 1
      rlim <- rX1X2_limits(input$rYX1, input$rYX2, 2)
      updateSliderInput(session, inputId="rrange", min=rlim[1], max=rlim[2])
      rminmax <- input$rrange

      if (reverse != prevReverse) {
        prevReverse <<- reverse
        updateSliderInput(session, inputId="rrange", value=rlim)
        rminmax <- rlim
      }

      if (reverse < 0) {
        rminmax <- -rev(rminmax)
      }

      make_fw_plot(input$rYX1, input$rYX2, rminmax[1], rminmax[2], input$show.partial_r, input$show.semipartial_r, input$fw.labels)
    })
  }, height=function () {
    min(session$clientData$output_plot_width, 780)
  })

  output$text_values <- renderText({
    text_values(input$rYX1, input$rYX2, input$plot_hover$x, input$show.partial_r, input$show.semipartial_r, input$go)
  })
}

if (useShiny) {
  prevGo <<- 0
  prevReverse <<- 0
  rlim <- rX1X2_limits(rYX1, rYX2, 2)

  ui <- fluidPage(
    titlePanel("Graphical illustration of two-predictor suppression effects, v0.05")
  , fluidRow(
      column(width=3
      , sliderInput(inputId="rYX1", label="Correlation between Y and X1", value=rYX1, min=-0.99, max=0.99, step=0.01)
      )
    , column(width=3
      , sliderInput(inputId="rYX2", label="Correlation between Y and X2", value=rYX2, min=-0.99, max=0.99, step=0.01)
      )
    , column(width=1
      , br(), br()
      , actionButton(inputId="go", label="Go!")
    )
    , column(width=2
      , checkboxInput(inputId="show.partial_r", label="Partial correlations (p.r1, p.r2)", value=show.partial_r)
      , checkboxInput(inputId="show.semipartial_r", label="Semipartial correlations (s.r1, s.r2)", value=show.semipartial_r)
      , checkboxInput(inputId="fw.labels", label="Friedman & Wall's region labels", value=fw.labels)
    )
    , column(width=3
      , sliderInput(inputId="rrange", label="Range of correlations between X1 and X2", value=rlim, min=rlim[1], max=rlim[2], step=0.01)
    )
    )
  , fluidRow(
      mainPanel(width="100%"
      , tags$style(type="text/css", "#text_values {background-color: rgba(255, 255, 0, 0.20);}")
      , verbatimTextOutput("text_values")
      )
    )
  , fluidRow(
      mainPanel(width="100%"
      , plotOutput(outputId="plot", width="100%", hover="plot_hover")
      )
    )
  )

  shinyApp(ui=ui, server=server)
} else {
  gg <- make_fw_plot(rYX1, rYX2, rmin, rmax, show.partial_r, show.semipartial_r, fw.labels)
  gg
}

#rsconnect::deployApp("~/Academic/Methods etc./Suppression/ShinyApp/SuppressionGraphics")
