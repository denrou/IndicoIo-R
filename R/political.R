#' Returns a probability distribution over the political alignment of the speaker
#'
#' Given input text, returns a probability distribution over the political 
#' alignment of the speaker.
#' @param text text for analysis
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @return List with party probability pairs
#' @keywords indico.io machine learning API political sentiment analysis
#' @seealso \code{\link{sentiment}}, \code{\link{language}}
#' @export
#' @import httr rjson stringr
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
political <- function(text, api_key = FALSE, cloud = FALSE, ...) {
  
  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }
  
  make_request(text, 'political', api_key, cloud, ...)
}

#' Returns a list of probability distributions over the political alignments of the speakers
#'
#' Given input text, returns a probability distribution over the political 
#' alignment of the speaker.
#' @param text array of documents for analysis
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @return List of lists with party probability pairs
#' @keywords indico.io machine learning API political sentiment analysis
#' @seealso \code{\link{sentiment}}, \code{\link{language}}
#' @export
#' @import httr rjson stringr
#' @examples
#' text <- c("I am so proud to stand here today 
#'            as Prime Minister of four nations
#'            in one United Kingdom.")
#' affilation <- batch_political()
#' affilation
#' most.like <- names(affilation[which.max(unlist(affilation[[1]]))])
#' least.like <- names(affilation[which.min(unlist(affilation[[1]]))])
#' cat(sprintf("This text is most like %s and least like %s", 
#'             most.like, least.like))
#' 
batch_political <- function(text, api_key = FALSE, cloud = FALSE, ...) {
  make_request(text, 'political', api_key, cloud, batch = TRUE, ...)
}
