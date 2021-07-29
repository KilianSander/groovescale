#' Experience of Groove Questionnaire
#'
#' This function defines an Experience of Groove Questionnaire (EGQ;
#' \insertCite{EGQPaper;textual}{groovescale}) module for incorporation into a
#' psychTestR timeline. Use this function if you want to include the EGQ in a
#' battery of other tests, or if you want to add custom psychTestR pages to
#' your test timeline. For a standalone implementation of the EGQ, consider
#' using \code{EGQ_standalone()}.
#'
#' @inheritParams audio_multi_radiobutton_NAFC_page
#'
#' @references
#'  \insertAllCited{}
#'
#' @export
EGQ <- function(label = "jingle",
                url = "https://raw.githubusercontent.com/KilianSander/groovescale/master/inst/www/audio/jingle.mp3",
                type = tools::file_ext(url),
                dict = groovescale::groovescale_dict,
                random_order = FALSE,
                show_controls = TRUE,
                allow_download = FALSE,
                ...) {
  egq <- psychTestR::new_timeline(audio_multi_radiobutton_NAFC_page(label = label,
                                                                    url = url,
                                                                    instruction = psychTestR::i18n("TGRV_ITEM_PREAMBLE"),
                                                                    prompts = sapply(1:6, function(x) psychTestR::i18n(sprintf("TGRV_000%o_PROMPT", x)), simplify = T, USE.NAMES = T),
                                                                    choices = 0:6,
                                                                    labels = sapply(1:7, function(x) psychTestR::i18n(sprintf("TGRV_CHOICE%o", x)), simplify = T, USE.NAMES = T),
                                                                    random_order = random_order,
                                                                    show_controls = show_controls,
                                                                    allow_download = allow_download,
                                                                    ...),
                                  dict = dict)

  psychTestR::join(psychTestR::begin_module(label = paste("EGQ", label, sep = "_")),
                   egq,
                   # scoring
                   psychTestR::code_block(function(state, ...){
                     results <- psychTestR::get_results(state = state, complete = FALSE)
                     results <- as.integer(results[[paste("EGQ", label, sep = "_")]][[label]])
                     urge_to_move <- mean(sapply(1:3, function(i) results[i]))
                     pleasure <- mean(sapply(4:6, function(i) results[i]))
                     #psychTestR::results(state)$results[[paste0("EGQ_", label)]][[label]] <- NULL
                     psychTestR::save_result(place = state,
                                             label = "urge_to_move",
                                             value = urge_to_move)
                     psychTestR::save_result(place = state,
                                             label = "pleasure",
                                             value = pleasure)
                     psychTestR::save_result(place = state,
                                             label = "raw_answer",
                                             value = results)}),
                   psychTestR::elt_save_results_to_disk(complete = TRUE),
                   psychTestR::end_module()
                   )
}
