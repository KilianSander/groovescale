# radiobuttons
psychTestR::page(ui = "Test",
                 label = "GRV",
                 final = FALSE)

ui <- shiny::div(
  shiny::p("Hier kommen Radiobuttons:"),
  shiny::radioButtons("Test",
                      "",
                      choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                      choiceValues = 1:7,
                      selected = 0,
                      inline = TRUE
                      )
)

#table tests
ui_table <-
htmltools::withTags(
  table(
    tr(
      th(),
      th()
    ),
    tr(
      th("Diese Musik weckt in mir den Wunsch, mich zu bewegen."),
      th(shiny::radioButtons("item1",
                             "",
                             choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                             choiceValues = 1:7,
                             selected = 0,
                             inline = TRUE))
    ),
    tr(
      th("Diese Musik ist gut zum Tanzen"),
      th(shiny::radioButtons("item2",
                             "",
                             choiceNames = c("Stimme ganz und gar nicht zu", "Stimme nicht zu", "Stimme eher nicht zu", "Weder noch", "Stimme eher zu", "Stimme zu", "Stimme voll und ganz zu"),
                             choiceValues = 1:7,
                             selected = 0,
                             inline = TRUE))
    )
  )
)

# the final function should look like this...
multiple_radio_button_NAFC_page <- function(label,
                                            prompts,
                                            choices,
                                            subprompt = "",
                                            labels = NULL,
                                            trigger_button_text = "Continue",
                                            failed_validation_message = "Answer missing!",
                                            save_answer = TRUE,
                                            hide_ui_responses = FALSE,
                                            response_ui_id = "response_ui",
                                            on_complete = NULL,
                                            admin_ui = NULL) {
  stopifnot(
    is.scalar.character(label),
    is.scalar.character(trigger_button_text),
    is.scalar.character(failed_validation_message),
    is.character(choices),
    length(choices) > 0L
  )
  ui <- (
    make_ui_multiple_radiobutton_NAFC(
      label,
      choices,
      subprompt = subprompt,
      labels = labels,
      trigger_button_text = trigger_button_text,
      hide = hide_response_ui,
      id = response_ui_id
    )
  )
  get_answer <- function(input, ...)
    input[[label]]
  validate <- function(answer, ...)
    if (!is.null(answer)) {
      TRUE
    } else {
      failed_validation_message
    }
  psychTestR::page(ui = ui,
                   label = label,
                   get_answer = get_answer,
                   save_answer = save_answer,
                   validate = validate,
                   on_complete = on_complete,
                   final = FALSE,
                   admin_ui = admin_ui
                   )
}
# and like this
make_ui_multiple_radiobutton_NAFC <- function(label,
                                              choices,
                                              subprompt = "",
                                              labels = NULL,
                                              trigger_button_text = "Continue",
                                              hide = FALSE,
                                              id = "response_ui") {
  stopifnot(
    is.character(choices) && length(choices) > 0L,
    is.scalar.logical(hide),
    is.null(labels) ||
      ((is.character(labels) || is.list(labels)) &&
         length(labels) == length(choices)
      )
  )
  if (is.null(labels)) {
    labels <- if (is.null(names(choices)))
      choices
    else
      names(choices)
  }
  #747 in https://github.com/pmcharrison/psychTestR/blob/master/R/test-elements.R
}
