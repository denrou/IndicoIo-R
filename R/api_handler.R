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

#' Returns a response from the indico API endpoint
#'
#' Given an input image, returns a data.frame obj
#' @param img Image to convert to a data.frame
#' @return data.frame constructed from image
#' @import httr rjson stringr
format_image <- function(img) {
  # Converts to anonymous data.frame
  df <- data.frame(img)
  colnames(df) <- NULL
  df
}

#' Returns a list of `data.frame`s given a list of input images
#'
#' Given a list of input images, returns a list of `data.frame`s
#' @param imgs List of images to convert to a `data.frame`s
#' @return `data.frame`s constructed from list of images
#' @import httr rjson stringr
format_images <- function(imgs) {
  img_list = list()
  for (i in 1:length(imgs)) {
    img <- data.frame(imgs[[i]])
    colnames(img) <- NULL
    img_list[[i]] = img
  }
  img_list
}