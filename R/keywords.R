#' Tags text with likely keywords
#'
#' Given input text, returns a series of keywords and associated scores
#' @inheritParams political
#' @return List with keyword score pairs
#' @keywords indico.io machine learning API classification tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}, \code{\link{text_tags}}
#' @export
#' @import httr rjson stringr
#' @examples
#' keywords <- keywords("Monday: Delightful with mostly sunny skies.
#'                    Highs in the low 70s.")
#' keywords
#' most.possible <- sort(unlist(keywords), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected keyword %s with a score of %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is %s with a score %0.4f.",
#'             names(most.possible)[2], most.possible[2]))
#'
keywords <- function(text, api_key = FALSE, cloud = FALSE, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'keywords', api_key, cloud, ...)
}

#' Tags text with likely categories
#'
#' Given a list of input documents, returns a list of probability distributions
#' over 111 possible topics
#' @inheritParams political
#' @return List of lists with text tag probability pairs
#' @keywords indico.io machine learning API classification tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}
#' @export
#' @import httr rjson stringr
#' @examples
#' keywords <- batch_keywords(c("Monday: Delightful with mostly sunny skies",
#'                                  "Highs in the low 70s."))
#' keywords
#' most.possible <- sort(unlist(keywords[[1]]), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected keyword %s with a score of %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is %s with a score of %0.4f.",
#'             names(most.possible)[2], most.possible[2]))
#'
batch_keywords <- function(text, api_key = FALSE, cloud = FALSE, ...) {
  make_request(text, 'keywords', api_key, cloud, batch = TRUE, ...)
}
