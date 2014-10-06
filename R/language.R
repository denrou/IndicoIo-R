#' Detects language of the text
#'
#' Given input text, returns a probability distribution over 33 possible
#' languages of what language the text was written in.
#' @inheritParams political
#' @return List with language probability pairs
#' @keywords indico.io machine learning API language detection
#' @seealso \code{\link{political}}, \code{\link{sentiment}}
#' @export
#' @examples
#' languages <- language("Monday: Delightful with mostly sunny skies.
#'                       Highs in the low 70s.")
#' languages
#' most.possible <- sort(unlist(languages), decreasing = T)[1:2]
#' cat(sprintf("Detected %s language with prbability %0.4f.\nNext possible is %s with probability %0.4f.", 
#'             names(most.possible)[1], most.possible[1],
#'             names(most.possible)[2], most.possible[2]))
#' 
language <- function(text, local.api = FALSE) {
  
  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }
  
  api <- ifelse(local.api, .indicoio$local_api, .indicoio$remote_api)
  api <- str_c(api, "language")
  
  # Makes request
  response <- POST(api, 
              accept_json(),
              add_headers(.indicoio$header),
              body = toJSON(list(text = text))
  )
  stop_for_status(response)
  
  # Returns results
  answer <- content(response, as = "parsed", type = "application/json")
  if (length(answer) < 2) {
    stop("Invalid result from API!")
  }
  answer
}
