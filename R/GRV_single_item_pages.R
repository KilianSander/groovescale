#' Experience of Groove Questionnaire
#'
#' This functions defines an Experience of Groove Questionnaire module with one
#' page per item for incorporation in a psychTestR timeline.
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
#' @param dict (i18n_dict) The dictionary used for internationalisation.
#' @param information If \code{TRUE} displays a page before the actual
#' questionnaire informing the participant that the following pages refer
#' to the same audio example
#' @param ... Further arguments to be passed to \code{GRV()}
#'
#' @export
GRV <- function(url = "https://raw.githubusercontent.com/KilianSander/groovescale/master/inst/www/audio/jingle.mp3",
                label = "GRV",
                type = tools::file_ext(url),
                save_answer = TRUE,
                on_complete = NULL,
                arrange_choices_vertically = length(choices) > 2L,
                wait = FALSE,
                loop = FALSE,
                admin_ui = NULL,
                show_controls = TRUE,
                allow_download = FALSE,
                dict = groovescale_dict,
                information = TRUE,
                ...) {
  # randomize items
  order <- sample(1:6,6)
  # build page per item
  if(information==TRUE) {
    elts <- info_page(dict = dict)
  } else {elts <- c()}
  choices <- as.character(1:7)
  choice_ids <- sprintf("TGRV_CHOICE%o", 1:7)
  for (item in order) {
    itempage <- psychTestR::new_timeline(
      psychTestR::audio_NAFC_page(
        label = paste0("item",item),
        prompt = psychTestR::i18n(paste0("TGRV_000",item,"_PROMPT")),
        choices = choices,
        labels = purrr::map(choice_ids, psychTestR::i18n),
        url = url,
        type = type,
        save_answer = save_answer,
        on_complete = on_complete,
        arrange_choices_vertically = arrange_choices_vertically,
        wait = wait,
        loop = loop,
        admin_ui = admin_ui,
        show_controls = show_controls,
        allow_download = allow_download),
        dict = dict
      )
    elts <- psychTestR::join(elts, itempage)
  }
  psychTestR::join(psychTestR::begin_module(label = paste("GRV", label, sep = "_")),
       elts,
       #scoring
       psychTestR::code_block(function(state, ...){
         results <- psychTestR::get_results(state = state, complete = FALSE)
         grv <- results[[paste("GRV", label, sep = "_")]]
         urge_to_move <- ((as.numeric(grv[["item1"]]) + as.numeric(grv[["item2"]]) + as.numeric(grv[["item3"]]))/3)
         pleasure <- ((as.numeric(grv[["item4"]]) + as.numeric(grv[["item5"]]) + as.numeric(grv[["item6"]]))/3)
         general_score <- ((urge_to_move + pleasure)/2)
         res <- c("urge_to_move", "pleasure", "general_score")
         for (i in res) {
           psychTestR::save_result(place = state,
                                   label = i,
                                   value = get(i))}
       }),
       psychTestR::elt_save_results_to_disk(complete = TRUE),
       psychTestR::end_module())
}

#' Info page
#'
#' This function creates a page to inform a participant that the six items of
#' the Experience of Groove Questionnaire refer to the same audio example.
#'
#' @param dict (i18n_dict) The dictionary used for internationalisation.
#' @param ... Further arguments to be passed to \code{info_page}.
#'
#' @export
info_page <- function(dict = groovescale::groovescale_dict,
                      ...){
  psychTestR::new_timeline(
    psychTestR::one_button_page(body = psychTestR::i18n("TGRV_INFO"),
                                button_text = psychTestR::i18n("CONTINUE")
    ),
    dict = dict
  )
}
