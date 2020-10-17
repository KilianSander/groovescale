# the final function should look like this...
multi_radiobutton_NAFC_page <- function(label,
                                        prompts,
                                        choices,
                                        instruction = "",
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
    is.character.vector(prompts),
    is.scalar.character(trigger_button_text),
    is.scalar.character(failed_validation_message),
    is.character.or.numeric(choices),
    length(choices) > 0L
  )
  ui <- (
    make_ui_multi_radiobutton_NAFC(
      label,
      prompts,
      choices,
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
#' Make multiple NAFC radiobuttons with the same choices
#'
#' Creates the HTML code for multiple n-alternative forced choice response
#' radiobutton options
#'
#' @param label (Character scalar) Label for the current page.
#'
#' @param prompts (Character vector) Prompts to be displayed over the response
#' choices
#'
#' @param choices (Character vector) Choices for the participant.
#' If unnamed, then these values will be used both for radiobutton IDs and for
#' button labels.
#' If named, then values will be used for button IDs and names
#' will be used for button labels.
#'
#' @param labels Optional vector of labels for the NAFC radiobutton choices.
#' If not \code{NULL}, will overwrite the names of \code{choices}.
#' This vector of labels can either be a character vector
#' or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.
#'
#' @param trigger_button_text (Character scalar) Text for the trigger button.
#'
#' @param hide (Boolean scalar) Whether the radiobuttons should be hidden (possibly to be shown later).
#'
#' @param id (Character scalar) HTML ID for the div containing the radiobuttons.
#'
make_ui_multi_radiobutton_NAFC <- function(label,
                                           prompts,
                                           choices,
                                           labels = NULL,
                                           trigger_button_text = "Continue",
                                           hide = FALSE,
                                           id = "response_ui") {
  stopifnot(
    is.character.or.numeric(choices) && length(choices) > 0L,
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
  labels <-
    purrr::map(labels, function(label)
      shiny::tags$span(style = "font-size: 15px; line-height: 15px;", label))

  multi_radiobuttons <-
    do.call(shiny::tagList,
            lapply(1:length(prompts), function(i) {
              shiny::tags$div(style = "text-align: left;",
                              shiny::tags$b(prompts[i]),
                              shiny::radioButtons(paste0("item",i),
                                                  "",
                                                  choiceNames = labels,
                                                  choiceValues = choices,
                                                  inline = TRUE,
                                                  selected = 0))}
            )
    )
  multi_radiobuttons <- htmltools::renderTags(multi_radiobuttons)
  multi_radiobuttons_div <- shiny::HTML(multi_radiobuttons$html)
  shiny::tags$div(id = id,
                  style = "inline-block;",
                  multi_radiobuttons_div,
                  psychTestR::trigger_button("next", trigger_button_text)
                  )
}
