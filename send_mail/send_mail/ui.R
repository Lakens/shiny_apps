library(shiny)
library(mailR)

ui =fluidPage(
  fluidRow(
    div(id = "login",
        wellPanel(title = "Mail your report", 
                  textInput("to", label = "To:", placeholder = "To:"),
                  textInput("sub","Subject:"),
                  textInput("msg","Message:"),
                  actionButton("mailButton",label = "Send mail") 
        )
    ))
)