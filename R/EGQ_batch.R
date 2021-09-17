#' EGQ Multiple Stimuli
#'
#' This function produces the Experience of Groove Questionnaire
#' \insertCite{@EGQ; @EGQPaper}{groovescale} for multiple stimuli.
#'
#' @param labels vector of the labels of the stimuli pages. Duplicates are not
#' allowed.
#'
#' @param urls vector of the URLs of the stimuli. May contain duplicates for
#' test-retest purposes.
#'
#' @inheritParams EGQ
#'
#' @references
#'  \insertAllCited{}
#'
#' @export
EGQ_batch <- function(labels,
                      urls,
                      type = tools::file_ext(url),
                      dict = groovescale::groovescale_dict,
                      random_order = FALSE,
                      show_controls = TRUE,
                      allow_download = FALSE,
                      ...) {
  stopifnot(length(labels)==length(urls),
            is.vector(labels),
            is.character(labels),
            is.vector(urls),
            anyDuplicated(labels)==0)
  #batch
  mapply(function(label, url) {EGQ(label = label,
                                   url = url,
                                   dict = dict,
                                   random_order = random_order,
                                   show_controls = show_controls,
                                   allow_download = allow_download,
                                   ...)
                                   },
         labels,
         urls)
}
