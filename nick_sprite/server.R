# Written by Nick Brown (nicholasjlbrown@gmail.com), 2018-2019.
# This work is licensed under a Creative Commons Attribution 4.0 International License (CC-BY).
#  See http://creativecommons.org/licenses/by/4.0/
# Thanks to Cédric Batailler for help with the X-axis.

# Version history
# 2018-02-19 16:08Z 0.01
#   First Shiny version released.
# 2018-02-19 17:23Z 0.02
#   Improved the look of the X-axis (numbers can go sideways if they start to bunch up).
#   Added an upper limit for the X-axis for when no items get close to the scale maximum.
#   Added code to use human-friendly intervals on the Y-axis.
# 2018-02-19 18:31Z 0.03
#   Fixed a bug that caused a previous error/warning message to hang around on the next run.
#   Added version number display
# 2018-02-19 21:43Z 0.04
#   Added input elements to allow a specific response value to appear a fixed number of times.
# 2018-02-20 17:21Z 0.05
#   Fixed a bug that caused a crash 50% of the time when scaleMin and scaleMax were both negative.
#   Added dynamic updates of upper/lower bounds of input controls, depending on the values of others.
# 2018-02-21 15:07Z 0.06
#   Fixed a bug that caused spurious error messages with certain fixed-value configurations.
#   Plots now appear sorted from smallest to largest skewness.
#   Added a rudimentary help feature.
#   Fixed a bug that meant that the user couldn't easily type in a SD close to the permitted minimum.
#   Fixed a bug that could cause errors in extreme cases with delta=2 in rSprite.delta.
#   Improved performance by taking a pragmatic decision about when to stop looking for duplicates.
# 2018-02-21 23:22Z 0.07
#   Added link to enable user to download the data that went into the plots.
#   Fixed a bug that was preventing solutions from being found with very large means and very small SDs.
# 2018-03-03 23:46Z 0.08
#   Increased the size of the plot area.
#   Increased maximum grid size to 10 x 10.
#   Changed plot bar colour for better visibility if black text encroaches on bars.
#   Reduced the chances of missing a valid solution when only a few (requested number > n > 1) exist.
#   Changed displayed name to rSprite.
# 2018-03-24 15:00Z 0.09
#   Display solutions on the smallest grid that they will fit onto.
#   User now chooses the number of results they want, not the grid size.
#   Moved the decimal places input field to just below the mean and SD.
#   Fixed a bug that could cause spurious solutions to be returned if none were possible.
# 2018-03-27 20:23Z 0.10
#   Fixed a bug that could cause the "No solutions found" message to be split into two.
#   Fixed a bug that prevented entering 0 or a negative number as the fixed value.
#   Fixed a bug that prevented a solution from being found in some extreme circumstances.
#   Fixed a bug that produced variable bar widths with large X-axis ranges.
# 2018-04-18 13:50Z 0.11
#   Fixed a bug that prevented the SD granularity from being changed.
#   Tightened the restrictions on the maximum SD that can be entered.
#   Moved the scale limit fields to the top of the list.
#   Fixed a small bug that sometimes showed more ticks than necessary on the X-axis.
#   Allow fixed values to be outside the scale range.
# 2018-05-22 13:32Z 0.12
#   Fixed a bug that caused a failure to calculate the possible SD range in some extreme samples.
# 2018-05-26 19:27Z 0.13
#   Added note about privatcy to the help text.
#   Added blank line before download link.
#   Added "loading" spinner image.
# 2018-11-08 23:40Z 0.14
#   Increased the size of the plot area.
#   Changed help text to point to preprint article instead of James's blog post.
#   Added CC-BY license.
#   Fixed a small bug that caused slightly different X-axis widths depending on the data.
# 2019-06-02 20:56Z 0.15
#   Fixed a bug that could cause valid SDs to be rejected as too small with means near the scale limits.
#
# To do:
# Check when to turn X-axis numbers sideways, eg 13-77 N=345 M=26 SD=12, one pane.

# To think about (could be hard):
# Allow zero as a number of a fixed value (i.e., that value does not appear).
# From Jordan: If no solution is found, print SD of nearest solution.

library(ggplot2)
library(gridExtra)
library(moments)
library(shiny)

rSprite.huge <- 1e15
rSprite.dust <- 1e-12

# Store a message for future display (unless otherwisde specified).
# If we have unloaded Shiny for debugging, show the message immediately.
rSprite.message <- function (s, shinyType="default", shinyNow=FALSE) {
  if (!exists("shinyUI")) {
    cat("rSPRITE message: |", s, "| (shinyType=", shinyType, ")", "\n", sep="")
    return()
  }

  message <- paste(shinyType, s, sep="%%")
  if (shinyNow) {
    rSprite.shinyMessages(list(message))
  }
  else {
    rSprite.messageList <<- append(rSprite.messageList, message)
  }
}

# See if a mean is GRIM-consistent. If not, return the nearest mean that is.
rSprite.checkGrim <- function (N, tMean, dp) {
  gMean <- tMean
  int <- round(tMean * N)           # nearest integer; doesn't matter if this rounds up or down
  frac <- int / N
  dif <- abs(tMean - frac)
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  if (dif > gran) {
    gMean <- round(int / N, dp)
    dpformat <- paste("%.", dp, "f", sep="")
    s <- paste("Mean ", sprintf(dpformat, tMean), " fails GRIM test - using ", sprintf(dpformat, gMean), sep="")
    rSprite.message(s, shinyType="warning")
  }

  return(gMean)
}

# Determine minimum and maximum SDs for given scale ranges, N, and mean.
rSprite.sdLimits <- function (N, tMean, scaleMin, scaleMax, dp) {
  result <- c(rSprite.huge, -rSprite.huge)      # impossible values

  aMax <- scaleMin                              # "aMax" means "value of a to produce the max SD"
  aMin <- floor(tMean)
  bMax <- max(scaleMax, scaleMin + 1, aMin + 1) # sanity check (just scaleMax would normally be ok)
  bMin <- aMin + 1
  total <- round(tMean * N)
  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]

    k <- round((total - (N * b)) / (a - b))
    k <- min(max(k, 1), N - 1)   # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, N - k))
    diff <- sum(vec) - total
    if ((diff < 0) && (k > 1)) {
      vec <- c(rep(a, k - 1), abs(diff), rep(b, N - k))
    }
    else if ((diff > 0) && ((N - k) > 1)) {
      vec <- c(rep(a, k), diff, rep(b, N - k - 1))
    }
    result[m] <- round(sd(vec), dp)
  }

  return(result)
}

# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
rSprite.delta <- function (vec, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c()) {
# Most of the time we change a pair of numbers by +/- 1, but 2 allows us to jump over fixed numbers.
  delta <- 1
  if (    (runif(1) < 0.2)
       && ((length(vec[vec > (scaleMin + 1)])) > 0)    # Check there is a number we can decrement by 2!
     ) {
    delta <- 2
  }

# Select an element to decrement. This should be greater than the minimum, and not
#  1 greater than a fixed value (because decrementing it would give us the fixed value).
# For better performance, at the risk of modest bias, we select from unique elements only.
  uniqueCanDec <- !duplicated(vec)
  notFixedDec <- if (length(fixed) > 0) !(vec %in% (fixed + delta)) else TRUE
  indexCanDec <- uniqueCanDec & (vec > (scaleMin + delta - 1)) & notFixedDec
  if (length(indexCanDec) == 0) {
    return(vec)           # Go back and try again.
  }

# Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < tSD)

# If we want to make the SD larger, there is no point in decrementing the largest item,
#  unless we have no choice.
  if (increaseSD) {
    indexCanDecTry1 <- indexCanDec & (vec < max(vec))
    if (sum(indexCanDecTry1) > 0) {
      indexCanDec <- indexCanDecTry1
    }
  }

  whichCanDec <- which(indexCanDec)
  whichWillDec <- whichCanDec[as.integer(runif(1) * length(whichCanDec)) + 1];
  willDec <- vec[whichWillDec]

# Select an element to increment. This should be smaller than the maximum,
#  and not 1 less than a fixed value.
  vecInc <- vec
  vecInc[whichWillDec] <- rSprite.huge        # mark the element that we decremented, so we can exclude it
  uniqueCanInc <- !duplicated(vecInc)
  notFixedInc <- if (length(fixed) > 0) !(vecInc %in% (fixed - delta)) else TRUE
  indexCanInc <- uniqueCanInc & (vecInc < (scaleMax - delta + 1)) & (vecInc != rSprite.huge) & notFixedInc

# If we want to make the SD smaller, there is no point in incrementing an item larger than
#   the one that we are going to decrement, unless we have no other choice.
  if (!increaseSD) {
    indexCanIncTry1 <- indexCanInc & (vec < willDec)
    if (sum(indexCanIncTry1) > 0) {
      indexCanInc <- indexCanIncTry1
    }
  }

# There is no point in incrementing an element that is <delta> smaller than the one
#  that we are going to decrement, unless we have no other choice.
  dontInc <- willDec - delta
  indexCanIncTry2 <- indexCanInc & (vecInc != dontInc)
  if (sum(indexCanIncTry2) > 0) {
    indexCanInc <- indexCanIncTry2
  }

# If we can't find an element to increment, just return the current vector unchanged and let our caller sort it out.
  if (sum(indexCanInc) < 1) {
    return(vec)           # Go back and try again.
  }

  whichCanInc <- which(indexCanInc)
  whichWillInc <- whichCanInc[as.integer(runif(1) * length(whichCanInc)) + 1];

# Another option is to only change one of the cells (decrement one without incrementing another,
#  or vice versa).
# This enables us to explore different means that still round to the same target value.
# I could have probably written this more elegantly if I'd thought of this issue before I wrote
#  the code above to select candidates to be both incremented and decremented, but there we are.
# So what we do here is to perform either the decrement or the increment first, and then see if
#  the mean is still GRIM-consistent with the target mean. If it is, in a proportion of cases,
#  we don't adjust the other cell.
# This can probably be optimised further, by considering whether we want to increase or decrease the SD.
# For some reason that I don't understand, the most "even" exploration of the means seems to occur
#  if the proportion of cases where we change the mean is not too large.

  decFirst <- (runif(1) < 0.5)
  if (decFirst) {
    vec[whichWillDec] <- vec[whichWillDec] - delta
  }
  else {
    vec[whichWillInc] <- vec[whichWillInc] + delta
  }

  doInc <- TRUE
  if (runif(1) < 0.1) {
    newFullVec <- c(vec, fixed)
    newMean <- mean(newFullVec)
    if (round(newMean, dp) == tMean) {         # New mean is GRIM-consistent, so we will keep it.
      doInc <- FALSE
    }
  }

  if (doInc) {
    if (decFirst) {
      vec[whichWillInc] <- vec[whichWillInc] + delta
    }
    else {
      vec[whichWillDec] <- vec[whichWillDec] - delta
    }
  }

  return(vec)
}

# Build the label to go across the top of each results chart.
rSprite.chartLabel <- function (N, tMean, tSD, scaleMin, scaleMax, dp, splitLine) {
  dpformat <- paste("%.", dp, "f", sep="")
  label <- paste("N=", N
                 , " (", scaleMin, "-", scaleMax, ")%%"
                 , "M=", sprintf(dpformat, tMean)
                 , " SD=", sprintf(dpformat, tSD)
                 , sep=""
  )

  if (splitLine) {
    label <- unlist(strsplit(label, "%%"))
  }
  else {
    label <- gsub("%%", " ", label)
  }

  return (label)
}

# Find a single vector of responses that matches the target mean and SD.
# Assumes that the mean has been checked for GRIM consistency (see rSprite.getSample).
rSprite.seekVector <- function (N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), label) {
# Generate some random starting data.
  rN <- N - length(fixed)
  vec <- pmax(pmin(as.integer(runif(rN) * 2 * tMean), scaleMax), scaleMin)
  result <- c()

  if (length(fixed) > 0) {         # replace any of the fixed numbers with a random non-fixed number
    whichFixed <- which(vec %in% fixed)
    notFixed <- sample(setdiff(scaleMin:scaleMax, fixed), length(whichFixed), replace=TRUE)
    vec[whichFixed] <- notFixed
  }

# Adjust mean of starting data.
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  meanOK <- FALSE
  maxStartLoops <- N * (scaleMax - scaleMin)

  for (i in 1:maxStartLoops) {
    fullVec <- c(vec, fixed)
    cMean <- mean(fullVec)
    dif <- abs(cMean - tMean)
    if (dif < gran) {
      meanOK <- TRUE
      break;
    }

# Identify numbers that we can increment or decrement.
# This should exclude numbers that would become one of the fixed values.
    deltaMean <- 1
    if (runif(1) < 0.2) {
      deltaMean <- 2       # This allows us to "jump over" the fixed values, if they are not at the extremities.
    }

    increaseMean <- (cMean < tMean)
    if (increaseMean) {
      filter <- (vec < (scaleMax - deltaMean + 1)) & (!(vec %in% (fixed - deltaMean)))
    }
    else {
      filter <- (vec > (scaleMin + deltaMean - 1)) & (!(vec %in% (fixed + deltaMean)))
    }

    canBumpMean <- which(filter)
    bumpMean <- canBumpMean[as.integer(runif(1) * length(canBumpMean)) + 1]   # select a changeable number
    vec[bumpMean] <- vec[bumpMean] + (if (increaseMean) deltaMean else -deltaMean)
  }

  if (!meanOK) {
    s <- "Couldn't initialize data with correct mean"  # This is actually a coding error if mean is in range
    rSprite.message(s, shinyType="error")
    return(result)
  }

  maxLoops <- max(round(N * ((scaleMax - scaleMin) ^ 2)), 1000)  # this maybe needs some more testing for pathological conditions
  found <- FALSE
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors

  for (i in 1:maxLoops) {
    cSD <- sd(c(vec, fixed))
    if (abs(cSD - tSD) <= gran) {
      result <- vec
      break
    }

    vec <- rSprite.delta(vec, tMean, tSD, scaleMin, scaleMax, dp, fixed)
    if (length(vec) == 0) {    # rSprite.delta() failed (but may have generated its own message(s)).
      break
    }
  }

  return(result)
}

# Generate a sample of one or more unique SPRITE solutions.
rSprite.getSample <- function (maxCases, N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c()) {
  result <- list(rows=c(), label="")

# Check mean is possible with GRIM; if not, identify the nearest valid mean.
  tMean <- rSprite.checkGrim(N, tMean, dp)

# Determine minimum and maximum SDs.
  sdLimits <- rSprite.sdLimits(N, tMean, scaleMin, scaleMax, dp)

  for (m in 1:2) {
    mSD <- sdLimits[m]
    s <- ""
    if ((m == 1) && (mSD > tSD)) {
      s <- "small; minimum="
    }
    else if ((m == 2) && (mSD < tSD)) {
      s <- "large; maximum="
    }

    if (s != "") {
      dpformat <- paste("%.", dp, "f", sep="")
      s <- paste("Target SD ", sprintf(dpformat, tSD), " is too ", s, sprintf(dpformat, mSD), sep="")
      rSprite.message(s, shinyType="warning")
      return(result)
    }
  }

  if (scaleMin >= scaleMax) {
    s <- paste("Scale minimum should be smaller than maximum")
    rSprite.message(s, shinyType="warning")
    return(result)
  }

  result$rows <- c()
  result$label <- rSprite.chartLabel(N, tMean, tSD, scaleMin, scaleMax, dp, (maxCases > 9))
  for (i in 1:(maxCases * 8)) {   # 8 is arbitrary; break early if we find enough unique cases.
    vec <- rSprite.seekVector(N, tMean, tSD, scaleMin, scaleMax, dp, fixed, result$label)
    if (length(vec) == 0) {       # If no solution was found on this run, return any we found up to now.
      if (length(result$rows) == 0) {
        s <- paste("No solution found for ", paste(result$label, collapse=" "), sep="")
        rSprite.message(s, shinyType="warning")
      }

      return(result)              # This may be slightly unsatisfactory if solutions are just very hard to come by.
    }

    fullVec <- sort(c(vec, fixed))         # Sorting lets us find duplicates more easily.
    if (length(result$rows) == 0) {
      result$rows <- matrix(fullVec, nrow=1)
    }
    else {
      newRows <- rbind(result$rows, fullVec)
      if (tail(duplicated(newRows), 1)) {  # The solution we just found is a duplicate.
        dups <- dups + 1
        if (dups > maxDups) {
          break
        }
        else {
          next
        }
      }

      result$rows <- newRows
    }

    nCases <- nrow(result$rows)
    if (nCases == maxCases) {
      incomplete <- FALSE
      break
    }

# Calculate the maximum number of consecutive duplicates we will accept before deciding to give up.
# The value of 0.00001 below is our nominal acceptable chance of missing a valid solution;
#  however, it's extremely likely that all possible solutions are not all equally likely to be found.
# So we also set a floor of 100 attempts.
    maxDups <- max(round(log(0.00001) / log(nCases / (nCases + 1))), 100)
    dups <- 0
  }

  if (nCases < maxCases) {
    was <- if (nCases == 1) "was" else "were"
    s <- paste(maxCases, " unique examples were requested, but only ", nrow(result$rows), " ", was, " found", sep="")
    rSprite.message(s, shinyType="warning")
  }

  return(result)
}

# Build a single results chart (grob).
rSprite.buildOneChart <- function (vec, scaleMin, scaleMax, gridSize, xMax, yMax, label) {
  df <- data.frame(vec)

# Avoid showing a large number of empty elements on the right of the X-axis if our upper bound is very large.
  xLimit <- if (((scaleMax - scaleMin) <= 11) || (xMax > scaleMax))
              max(scaleMax, xMax)
            else
              min(scaleMax, (xMax + 2))
  xBreaks <- scaleMin:xLimit

# Allow for room above the highest bar to display the label.
  yLimit <- yMax
  llen <- length(label)
  if (llen > 0) {
    yBump <- round(llen * max(2, yMax * 0.1) * (((gridSize >= 4) + 2) / 2))
    yLimit <- yMax + yBump
  }

  yTicks <- c(10, 8, 6, 5, rep(4, 6))[gridSize]
  yTickSize <- round((yMax / (yTicks - 1)) + 1)
  yLabelGaps <- c(1, 2, 3, 4, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000)
  yGap <- yLabelGaps[yLabelGaps >= yTickSize][1]
  yBreaks <- (0:(yTicks - 1)) * yGap

  axisTitleSize <- c(20, 14, 12, 11, 10, rep(8, 5))[gridSize]
  axisTextSize <- c(16, 12, 10, 9, 8, rep(7, 5))[gridSize]

  grob <- ggplot(df, aes(x=factor(vec, levels=xBreaks))) +
          geom_bar(fill="#0099ff", width=0.9) +
          scale_x_discrete(drop=FALSE) +
          scale_y_continuous(limits=c(0, yLimit), breaks=yBreaks) +
          theme(axis.title=element_text(size=axisTitleSize)) +
          theme(axis.text=element_text(size=axisTextSize)) +
          labs(x="response", y="count")

  if (llen > 0) {
    if (gridSize <= 10) {
      labelTextSize <- axisTitleSize * 0.352778 * (1 - (0.1 * (gridSize >= 8)))     # see StackOverflow 36547590
      labelText <- paste(label, collapse="\n")
      labelY <- (yLimit + 1 - llen) - (gridSize >= 5) - (gridSize >= 7)
      grob <- grob + annotate("text", x=round((xLimit + scaleMin) / 2), y=labelY, label=labelText, size=labelTextSize)
    }
  }

  flipXThreshold <- c(50, 30, 10, 15, 10, rep(3, 5))[gridSize]
  if (length(xBreaks) > flipXThreshold) {
    grob <- grob + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  }

  return(grob)
}

# Build a grid containing all the results charts.
rSprite.buildCharts <- function (sample, scaleMin, scaleMax, gridSize) {
  rows <- sample$rows
  if (nrow(rows) > 1) {
    rows <- rows[order(apply(rows, 1, skewness)),]
  }

  xMax <- max(rows)
  yMax <- max(unlist(apply(rows, 1, table)))
  grobs <- apply(rows, 1, function (x) {
    rSprite.buildOneChart(x, scaleMin, scaleMax, gridSize, xMax, yMax, sample$label)
  })
  layoutMatrix <- matrix(1:(gridSize ^ 2), nrow=gridSize, ncol=gridSize, byrow=TRUE)
  grid.arrange(grobs=grobs, layout_matrix=layoutMatrix)
}

# Function to display one or more notification messages.
rSprite.shinyMessages <- function (messageList) {
  lapply(rSprite.notifIdList, function (x) {
    removeNotification(x)
  })
  rSprite.notifIdList <<- list()

  uniqueMessages <- unique(unlist(messageList))
  sapply(uniqueMessages, function (x) {
    split <- unlist(strsplit(x, "%%"))
    messageType <- split[1]
    messageText <- split[2]
    id <- showNotification(messageText, type=messageType, duration=NULL, closeButton=FALSE)
    rSprite.notifIdList <<- append(rSprite.notifIdList, id)
  })
}

rSprite.helpText <- c(
  "rSPRITE is an implementation by Nick Brown of SPRITE, an idea by James Heathers."
  , "<br/><br/>"
  , "rSPRITE simulates data from an integer (e.g., Likert-type) scale in the form of bar charts."
  , "<br/><br/>"
  , "You can request up to 100 samples to be presented on a square grid."
  , " You need to specify the minimum and maximum item values of the scale,"
  , " and the mean, standard deviation, and size of the sample."
  , " The charts are presented in increasing order of skewness, from top left to bottom right."
  , "<br/><br/>"
  , "Optionally, you can provide a fixed value and a count;"
  , " this forces every sample to contain exactly that many occurrences of that value,"
  , " which may be outside the scale range."
  , "<br/><br/>"
  , "You can also download the individual values that make up the bar charts to a CSV file."
  , "<br/><br/>"
  , "If you check the box labeled 'Use fixed seed', you will get the same results on every run;"
  , " this can be useful when reporting problems, but otherwise, leave this box unchecked."
  , "<br/><br/>"
  , "rSPRITE may not always find every solution when there are only a few to be found."
  , " If you get a message saying that fewer results were found than you hoped for,"
  , " please try a couple more times to see if one or two more solutions show up."
  , "<br/><br/>"
  , "A general observation: rSPRITE is a tool and not a complete system."
  , " Like any tool, it has the potential to be used incorrectly."
  , " If you ask it do something silly, it will do it, very probably without warning you."
  , "<br/><br/>"
  , "For more information on SPRITE in general, see <a href=https://peerj.com/preprints/26968v1/>here</a>."
  , "<br/><br/>"
  , "Please report bugs to nicholasjlbrown@gmail.com"
  , "<br/><br/>"
  , "Privacy policy: rSPRITE does not collect any information about you whatsoever."
  , " If you are using this code in a web browser at shinyapps.io, you can find the RStudio"
  , " terms of use <a href='https://www.rstudio.com/about/rstudio-service-terms-of-use/'>here</a>."
)

rSprite.notifIdList <<- list()
rSprite.messageList <<- list()
rSprite.prevGo <<- 0
rSprite.prevHelp <<- 0
rSprite.plotData <<- c()

server <- function (input, output, session) {
  fixedCount <- reactive({
    result <- 0
    sn <- gsub(" ", "", input$fixedCount)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^[0-9]+$", sn)) {
        f <- as.numeric(sn)
        if ((f > 0) && (f < input$N)) {
          result <- f
        }
      }
    }

    if (result == rSprite.huge) {
      s <- paste("Fixed count must be an integer from 1 to ", (input$N - 1)
               , "; input |", input$fixedCount
               , "| ignored"
               , sep=""
      )
      rSprite.message(s, shinyType="warning")
      result <- 0
    }

    result
  })

  fixedResponse <- reactive({
    result <- 0
    sn <- gsub(" ", "", input$fixedResponse)
    if (sn != "") {
      result <- rSprite.huge
      if (grepl("^-?[0-9]+$", sn)) {
        result <- as.numeric(sn)
      }
    }

    if (result == rSprite.huge) {
      s <- paste("Fixed value must be an integer from ", input$scaleMin
               , " to ", input$scaleMax
               , "; input |", input$fixedResponse
               , "| ignored"
               , sep=""
      )
      rSprite.message(s, shinyType="warning")
      result <- 0
    }

    result
  })

  reactiveSample <- eventReactive(input$go, {
    rSprite.message("Calculating...", shinyNow=TRUE)
    set.seed(if (input$fixedSeed) 1 else as.numeric(Sys.time()))
    fixed <- rep(fixedResponse(), fixedCount())
    gridSize <- sqrt(as.numeric(input$gridSize))

    rSprite.getSample(
      gridSize ^ 2
    , input$N
    , input$tMean
    , input$tSD
    , input$scaleMin
    , input$scaleMax
    , input$dp
    , fixed
    )
  })

# This element is just a place to catch and handle changes in the input controls and their relations to each other.
# We never actually output anything to a text box.
  output$dummy <- renderText({
    N <- input$N
    tMean <- input$tMean
    tSD <- input$tSD
    scaleMin <- input$scaleMin
    scaleMax <- input$scaleMax
    dp <- input$dp
    dstep <- c(0.1, 0.01, 0.001)[dp]

    updateNumericInput(session, inputId="scaleMin", max=(scaleMax - 1))
    updateNumericInput(session, inputId="scaleMax", min=(scaleMin + 1))
    updateNumericInput(session, inputId="tMean", min=scaleMin, max=scaleMax, step=dstep)
    updateNumericInput(session, inputId="tMean", min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)

# It is tempting to force the mean value to a GRIM-consistent one here
#  (cf. what we do for the SD below), but this would be an error,
#  as we would be unable to "scroll" from one valid mean to another using the
#  input spinners if there were any invalid intermediate values
#  (we would constantly be forced back).
# However, we do force the mean to be between scaleMin and scaleMax.
    if (!is.na(tMean)) {
      newMean <- max(min(round(tMean, dp), scaleMax), scaleMin)
      if (newMean != tMean) {
        updateNumericInput(session, inputId="tMean", value=newMean)
      }
    }

# Similarly, it would be nice to have the range for the SD limited by the current mean,
#  but this leads to all sorts of complications. So we allow the user to enter an SD
#  that is too small or large, and tell them later.
    if (!is.na(tSD)) {
      newSD <- max(min(round(tSD, dp), scaleMax), 0)
      if (newSD != tSD) {
        updateNumericInput(session, inputId="tSD", value=newSD)
      }
    }

    return()      # just falling out at the end gives an Shiny error message the first time we come here
  })

  output$plotDisplayed <- reactive({
    input$go
    input$help

    (length(rSprite.plotData) > 0)
  })
  outputOptions(output, "plotDisplayed", suspendWhenHidden=FALSE, priority=-1)

  output$downloadData <- downloadHandler(
    filename=function () {
      "spritedata.csv"
    }
  , content=function (file) {
      write.table(rSprite.plotData, file, row.names=FALSE, col.names=FALSE, sep=",")
    }
  )

  output$help <- renderUI({
    input$go                  # creates a dependency on the Go button

    helpText <- ""            # Unless the user clicked Help, we will clear any existing help text.
    if (input$help > rSprite.prevHelp) {    # user clicked the Help button
      rSprite.prevHelp <<- input$help
      helpText <- HTML(paste(rSprite.helpText, collapse=""))
    }

    isolate({
      helpText
    })
  })

  output$plot <- renderPlot({
    input$help                          # creates a dependency on the Help button
    rSprite.plotData <<- c()

    if (input$go > rSprite.prevGo) {    # user clicked the Go button
      rSprite.prevGo <<- input$go
    }
    else {
      return()           # this clears the plot area (which conveniently allows the help text to show)
    }

    isolate({
      N <- input$N
      tMean <- input$tMean
      tSD <- input$tSD
      scaleMin <- input$scaleMin
      scaleMax <- input$scaleMax
      dp <- input$dp
      gridSize <- sqrt(as.numeric(input$gridSize))

      sample <- reactiveSample()
      if (length(sample$rows) > 0) {
        if (    (gridSize == 10)
             && (session$clientData$url_hostname == "127.0.0.1")        # On developer's local screen...
           ) {                                                          # ... don't show 10x10 grid...
          rSprite.message("Skipping rendering", shinyNow=TRUE)          # ... to speed up generation of test data.
        }
        else {
          gridSize <- floor(sqrt(nrow(sample$rows)) + 0.999)
          rSprite.buildCharts(sample, scaleMin, scaleMax, gridSize)
          rSprite.message("Rendering...", shinyNow=TRUE)
        }

        rSprite.plotData <<- sample$rows
      }

      rSprite.shinyMessages(rSprite.messageList)
      rSprite.messageList <<- list()
    })
  }, height=function () {
    min(session$clientData$output_plot_width, 780)
  }, width=1200)
}
