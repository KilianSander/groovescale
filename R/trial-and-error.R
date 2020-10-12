make_radiobuttons <- function(itemtexts,
                              choices,
                              labels = NULL) {
  radiobuttons <- c()
  for (i in lengths(itemtexts)) {
    radiobuttons <- append(radiobuttons, shiny::tags$div(shiny::tags$b(itemtexts[[i]]),
                                                         shiny::radioButtons(inputId = paste0("item", i),
                                                                             label = "",
                                                                             choiceNames = labels,
                                                                             choiceValues = choices,
                                                                             inline = TRUE,
                                                                             selected = 0)
                                                         )
    )
  }
  itemtexts <- paste(unlist(radiobuttons),
  return(itemtexts)
}

Liste <- make_radiobuttons(itemtexts = c("I1","i2","I3"), choices = 1:3, labels = c("gut","mittel","schlecht"))
