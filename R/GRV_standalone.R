#' Standalone Experience of Groove Questionnaire
#'
#' This function launches a standalone testing session for the Experience of Groove Questionnaire.
#'
#' @param url URL to the audio. Can be an absolute URL (e.g. "http://mysite.com/audio.mp3") or a URL relative to the /www directory (e.g. "audio.mp3").
#' @param label Label to identify all answers corresponding to the audio given under \code{url}
#' @param type Audio type (e.g. 'mp3'). Defaults to the provided file extension.
#' @param save_answer Whether or not to save the answer.
#' @param arrange_choices_vertically Whether to arrange the response buttons vertically (the default) as opposed to horizontally.
#' @param wait Whether to wait for the audio to finish before displaying the response buttons.
#' @param loop Whether the audio should loop.
#' @param admin_ui Optional UI component for the admin panel.
#' @param show_controls Whether or not to show audio controls to the participant, so that they can control audio playback.
#' @param allow_download Whether the participant is given a button to download the audio file; only relevant if \code{show_controls} is \code{TRUE}.
#' @param admin_password Password to access the admin panel.
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
                           admin_password = "groove") {
  psychTestR::make_test(psychTestR::join(psychTestR::get_p_id(prompt = "ENTER_ID",
                                                              placeholder = "EG 123456",
                                                              button_text = "NEXT"),
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
                                             allow_download = allow_download),
                                         psychTestR::final_page("CLOSE_BROWSER")),
                        opt = test_options(title = "Experience of Groove Questionnaire",
                                           admin_password = admin_password))
}

