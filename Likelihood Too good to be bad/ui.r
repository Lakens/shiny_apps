ui <- fluidPage(titlePanel("Likelihood Ratio for Mixed Results"),
  sidebarLayout(
    sidebarPanel(numericInput("n", "Number of studies :", 3, min = 1, max = 100),
                 numericInput("x", "Number of successes:", 2, min = 0, max = 100),
                 sliderInput("H0", "Type 1 error rate:", min = 0, max = 1, value = 0.05, step= 0.01),
                 sliderInput("H1", "Assumed Power:", min = 0, max = 1, value = 0.8, step= 0.01)
                 ),
    mainPanel(plotOutput("coolplot"),br(),
              h4("Shiny app accompanying: Lakens, D., & Etz, A. J. (2017). Too true to be bad: When sets of studies with significant and non-significant findings are probably true. Social Psychological and Personality Science. https://osf.io/preprints/psyarxiv/nnkg9"),br(),
              h4(textOutput("text1")), br(),
              h4(textOutput("text2")),br(),
              h4("When the observed results are equally likely under H0 and H1, the likelihood ratio is 1. Benchmarks to interpret Likelihood Ratios suggest that when 1<LR<8 there is weak evidence, when 8<LR<32 there is moderate evidence, and when LR>32, there is strong evidence."),br(),
              h4(textOutput("text3")), br(),
              dataTableOutput("table")
    )
  )
)