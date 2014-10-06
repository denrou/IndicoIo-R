#' Returns a scalar estimate of the sentiment of the text
#'
#' Given input text, returns a scalar estimate of the sentiment of that text.
#' @param text text for analysis
#' @param local.api logical, whether use local or remote API
#' @return Numerical value roughly in the range from 0 to 1 with 0.5 indicating neutral sentiment.
#' For reference, 0 suggests very negative sentiment and 1 suggests very positive sentiment.
#' @keywords indico.io machine learning API sentiment analysis
#' @seealso \code{\link{political}} 
#' @export
#' @examples
#' emotion <- sentiment("Thanks everyone for the birthday wishes!!
#'                       It was a crazy few days ><")
#' emotion
#' cat(sprintf("This text has %s tonality", 
#'              ifelse(emotion > 0.5, "positive", "negative")))
#' 
sentiment <- function(text, local.api = FALSE) {
  
  # Checks parameters
  if (missing(text)) {
    stop("No text for analysis provided!")
  }
  
  api <- ifelse(local.api, .indicoio$local_api, .indicoio$remote_api)
  api <- str_c(api, "sentiment")
  
  # Makes request
  response <- POST(api, 
                   accept_json(),
                   add_headers(.indicoio$header),
                   body = toJSON(list(text = text))
  )
  stop_for_status(response)
  
  # Returns results
  answer <- content(response, as = "parsed", type = "application/json")
  if (!"Sentiment" %in% names(answer)) {
    stop("Invalid result from API!")
  }
  answer[["Sentiment"]]
}
