library(shinycssloaders)

N <- 45
tMean <- 3.532
tSD <- 1.561
dp <- 2
scaleMin <- 1
scaleMax <- 7
fixedValue <- ""
fixedCount <- ""
fixedSeed <- 0

#tMean <- 19.4
#tSD <- 19.9
#dp <- 1
#scaleMin <- 0
#scaleMax <- 41
#fixedValue <- 0
#fixedCount <- 21
#fixedSeed <- 1

dstep <- c(0.1, 0.01, 0.001)[dp]

ui <- fluidPage(
  titlePanel("rSPRITE beta 0.13")
, sidebarLayout(
    position="left"
  , sidebarPanel(
      width=2
    , numericInput(inputId="scaleMin", label="Minimum scale value", value=scaleMin, min=-20, max=1, step=1)
    , numericInput(inputId="scaleMax", label="Maximum scale value", value=scaleMax, min=2, max=50, step=1)
    , numericInput(inputId="N", label="Sample size", value=N, min=2, max=1000, step=1)
    , numericInput(inputId="tMean", label="Target mean", value=round(tMean, dp), min=scaleMin, max=scaleMax, step=dstep)
    , numericInput(inputId="tSD", label="Target SD", value=round(tSD, dp), min=0, max=(((scaleMax - scaleMin) / 2) + 1), step=dstep)
    , numericInput(inputId="dp", label="Decimal places", value=dp, min=1, max=3, step=1)
    , selectInput(inputId="gridSize", label="Number of results", choices=(c(1:10) ^ 2), selected=9)
    , fluidRow(
        column(
          6
        , textInput(inputId="fixedResponse", label="Fixed value", value=fixedValue)
      )
      , column(
          6
        , textInput(inputId="fixedCount", label="Fixed count", value=fixedCount)
      )
    )
    , checkboxInput(inputId="fixedSeed", label="Use fixed seed", value=fixedSeed)
    , fluidRow(
        column(
          6
        , actionButton(inputId="go", label="Go!")
        )
      , column(
          6
        , actionButton(inputId="help", label="Help")
        )
      )
    , conditionalPanel(
        condition="output.plotDisplayed"
      , br()
      , downloadLink(outputId="downloadData", label="Download data")
    )
  )
  , mainPanel(
      withSpinner(
        plotOutput(outputId="plot", width="100%"), type=5
      )
    , absolutePanel(        # allows help panel to overlay plot panel
        top="50"
      , textOutput(outputId="dummy")
      , htmlOutput(outputId="help")
      )
    )
  )
, tags$head(
    tags$style(
      HTML(".shiny-notification { position:relative; bottom:2px; left:-200px; width:125% }")
    )
  )
)
