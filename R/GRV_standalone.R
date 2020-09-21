#' Standalone Experience of Groove Questionnaire
#'
#' This function launches a standalone testing session for the Experience of
#' Groove Questionnaire.
#'
#' @param url URL to the audio. Can be an absolute URL (e.g.
#' "http://mysite.com/audio.mp3") or a URL relative to the /www directory (e.g.
#' "audio.mp3").
#' @param label Label to identify all answers corresponding to the audio given
#' under \code{url}
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#' @param save_answer Whether or not to save the answer.
#' @param arrange_choices_vertically Whether to arrange the response buttons
#' vertically (the default) as opposed to horizontally.
#' @param wait Whether to wait for the audio to finish before displaying the
#' response buttons.
#' @param loop Whether the audio should loop.
#' @param admin_ui Optional UI component for the admin panel.
#' @param show_controls Whether or not to show audio controls to the
#' participant, so that they can control audio playback.
#' @param allow_download Whether the participant is given a button to download
#' the audio file; only relevant if \code{show_controls} is \code{TRUE}.
#' @param info_page If set \code{TRUE} displays a page before the actual
#' questionnaire informing the participant that the following pages refer
#' to the same audio example
#' @param admin_password Password to access the admin panel.
#' @param admin_password Password to access the admin panel.
#' @param researcher_email Researcher's email; used in participant help message.
#' @param validate_id Optional validation function. See \code{validate} in
#' \link[psychTestR]{page} for further information.
#' @param languages Character vector of languages that may be selected via the
#' URL parameter 'language'. If no language is provided by the URL parameter,
#' defaults to the first language in this vector. Languages should be encoded
#' according to ISO 639-2 conventions. Possible languages are shown with
#' \link[groovescale]{languages}.
#' @param ... Further arguments to be passed to \code{GRV_standalone()}
#'
#' @export
GRV_standalone <- function(url = "https://raw.githubusercontent.com/KilianSander/groovescale/master/inst/www/audio/jingle.mp3",
                           label = "GRV",
                           type = tools::file_ext(url),
                           save_answer = TRUE,
                           on_complete = NULL,
                           arrange_choices_vertically = TRUE,
                           wait = FALSE,
                           loop = FALSE,
                           admin_ui = NULL,
                           show_controls = TRUE,
                           allow_download = FALSE,
                           information = TRUE,
                           admin_password = "groove",
                           researcher_email = NULL,
                           validate_id = "auto",
                           languages = groovescale::languages(),
                           ...) {
  elts <- c(
    psychTestR::new_timeline(
      psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                           placeholder = paste(psychTestR::i18n("EG"), "123456", sep = " "),
                           button_text = psychTestR::i18n("CONTINUE"),
                           validate = validate_id),
    dict = groovescale_dict
    ),
    GRV(url,
        label = label,
        type = type,
        save_answer = save_answer,
        on_complete = on_complete,
        arrange_choices_vertically = arrange_choices_vertically,
        wait = wait,
        loop = loop,
        admin_ui = admin_ui,
        show_controls = show_controls,
        allow_download = allow_download,
        information = information,
        welcome = TRUE,
        ...),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(psychTestR::i18n("RESULTS_SAVED"),
        psychTestR::i18n("CLOSE_BROWSER"))),
      dict = groovescale_dict
    )
    )
  title <- unlist(setNames(
    purrr::map(groovescale::languages(), function(x)
      groovescale_dict$translate("TGRV_TITLE", x)),
    groovescale::languages()
  ))
  psychTestR::make_test(elts,
                        opt = psychTestR::test_options(title = title,
                                                       admin_password = admin_password,
                                                       researcher_email = researcher_email,
                                                       languages = languages))
}

