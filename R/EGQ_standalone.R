#' Standalone Experience of Groove Questionnaire
#'
#' This function launches a standalone testing session for the Experience of
#' Groove Questionnaire \insertCite{EGQPaper}{groovescale}.
#'
#' @inheritParams EGQ
#'
#' @references \insertAllCited{}
#'
#' @export
EGQ_standalone <- function(label = "jingle",
                           url = "https://raw.githubusercontent.com/KilianSander/groovescale/master/inst/www/audio/jingle.mp3",
                           type = tools::file_ext(url),
                           dict = groovescale::groovescale_dict,
                           random_order = FALSE,
                           show_controls = TRUE,
                           allow_download = FALSE,
                           admin_password = "groove",
                           researcher_email = NULL,
                           languages = groovescale::languages(),
                           validate_id = "auto",
                           ...) {
  elts <- c(
    psychTestR::new_timeline(
      psychTestR::get_p_id(prompt = psychTestR::i18n("ENTER_ID"),
                           placeholder = paste(psychTestR::i18n("EG"), "123456", sep = " "),
                           button_text = psychTestR::i18n("CONTINUE"),
                           validate = validate_id),
      dict = dict
    ),
    EGQ(url = url,
        label = label,
        type = type,
        dict = dict,
        random_order = random_order,
        show_controls = show_controls,
        allow_download = allow_download,
        ...),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::p(psychTestR::i18n("RESULTS_SAVED"),
                                      psychTestR::i18n("CLOSE_BROWSER"))),
      dict = dict)
    )
  title <- unlist(setNames(
    purrr::map(groovescale::languages(), function(x)
      groovescale::groovescale_dict$translate("TGRV_TITLE", x)),
    groovescale::languages()
  ))
  psychTestR::make_test(elts,
                        opt = psychTestR::test_options(title = title,
                                                       admin_password = admin_password,
                                                       researcher_email = researcher_email,
                                                       languages = languages))
}
