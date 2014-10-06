#' Returns a probability distribution over the political alignment of the speaker
#'
#' Given input text, returns a probability distribution over the political 
#' alignment of the speaker.
#' @param text text for analysis
#' @param local.api logical, whether use local or remote API
#' @return List with party probability pairs
#' @keywords indico.io machine learning API political sentiment analysis
#' @seealso \code{\link{sentiment}} 
#' @export
#' @examples
#' affilation <- political("I am so proud to stand here today 
#'                          as Prime Minister of four nations
#'                          in one United Kingdom.")
#' affilation
#' most.like <- names(affilation[which.max(unlist(affilation))])
#' least.like <- names(affilation[which.min(unlist(affilation))])
#' cat(sprintf("This text is most like %s and least like %s", 
#'             most.like, least.like))
#' 
political <- function(text, local.api = FALSE) {
  
  # Checks parameters
  if (missing(text)) {
    stop("No text for analysis provided!")
  }
  
  api <- ifelse(local.api, .indicoio$local_api, .indicoio$remote_api)
  api <- str_c(api, "political")
  
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
