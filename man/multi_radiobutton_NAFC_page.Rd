% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_elements.R
\name{multi_radiobutton_NAFC_page}
\alias{multi_radiobutton_NAFC_page}
\title{New multiple radiobutton NAFC page}
\usage{
multi_radiobutton_NAFC_page(
  label,
  prompts,
  choices,
  instruction = "",
  labels = NULL,
  trigger_button_text = "Continue",
  failed_validation_message = "Answer missing!",
  save_answer = TRUE,
  hide_response_ui = FALSE,
  random_order = FALSE,
  response_ui_id = "response_ui",
  on_complete = NULL,
  admin_ui = NULL
)
}
\arguments{
\item{label}{(Character scalar) Label for the current page.}

\item{prompts}{(Character vector) Prompts to be displayed over the response
choices}

\item{choices}{(Character vector) Choices for the participant.
If unnamed, then these values will be used both for radiobutton IDs and for
button labels.
If named, then values will be used for button IDs and names
will be used for button labels.}

\item{labels}{Optional vector of labels for the NAFC radiobutton choices.
If not \code{NULL}, will overwrite the names of \code{choices}.
This vector of labels can either be a character vector
or a list of Shiny tag objects, e.g. as created by \code{shiny::HTML()}.}

\item{trigger_button_text}{(Character scalar) Text for the trigger button.}

\item{failed_validation_message}{(Character scalar) Text to be displayed
when validation fails.}

\item{save_answer}{(Boolean scalar) Whether or not to save the answer.}

\item{hide_response_ui}{(Boolean scalar) Whether to begin with the response
interface hidden (it can be subsequently made visible through Javascript,
using the element ID as set in \code{response_ui_id}.
See \link[psychTestR]{audio_NAFC_page} for an example.).}

\item{random_order}{(Boolean scalar) Whether the order of the items should
be randomized.}

\item{response_ui_id}{(Character scalar) HTML ID for the response user interface.}

\item{on_complete}{Optional function to execute on leaving the page
(after successful validation).
The argument list should include \code{...},
and any of:
\code{state}, the participant's state object;
\code{answer}, the participant's most recent answer;
\code{input}, the current page's Shiny input object;
\code{session}, the current Shiny session object;
\code{opt}, the test's option list as created by \code{test_options()}.}

\item{admin_ui}{Optional UI component for the admin panel.}
}
\description{
Creates a multiple radiobutton n-alternative forced choice page.
}
