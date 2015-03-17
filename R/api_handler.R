#' Returns a response from the indico API endpoint
#'
#' Given input data, return the response from the indico API endpoint specified
#' @param data data for analysis
#' @param api api to call
#' @param auth username and password for HTTP Basic Auth
#' @param cloud subdomain for indico private cloud
#' @param batch send call to batch endpoint
#' @return error or response extracted from the indico API response
#' @keywords indico.io machine learning API
#' @export
#' @import httr rjson stringr
make_request <- function(data, api, auth = FALSE, cloud = FALSE, batch = FALSE) {
  
  if (cloud) {
    private_cloud <- sprintf(.indicoio$private_cloud, cloud)
  }

  base_url <- ifelse(cloud, private_cloud, .indicoio$remote_api)
  url <- str_c(base_url, api)
  if (batch) {
    url <- str_c(url, '/batch')
  }

  # Makes request
  headers <- add_headers(.indicoio$header)
  body <- toJSON(list(data = data))

  if (auth == FALSE) {
    auth <- .indicoio$auth
  }

  if (is.vector(auth) && (length(auth) == 2)) {
    auth <- authenticate(auth[1], auth[2], type="basic")
    response <- POST(url, accept_json(), headers, auth, body = body)
  } else {
    response <- POST(url, accept_json(), headers, body = body)
  }
  
  stop_for_status(response)
  
  # Returns results
  answer <- content(response, as = "parsed", type = "application/json")
  if ("error" %in% names(answer)) {
    stop(answer[["error"]])
  }

  answer[["results"]]
}
