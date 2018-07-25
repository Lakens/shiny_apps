server = function(input, output, session) {
  observeEvent(input$mailButton,{
    isolate({
      send.mail(from = "D.Lakens@tue.nl",
                to = input$to,
                subject = input$sub,
                body = input$msg,
                smtp = list(host.name = "smtp.tue.nl", port = 2525, user.name = "NAME", passwd = "PASSWORD", tls = TRUE),
                authenticate = TRUE,
                html = TRUE,
                send = TRUE)
    })
  })
  
}
