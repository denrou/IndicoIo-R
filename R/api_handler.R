#' Returns a response from the indico API endpoint
#'
#' Given input data, return the response from the indico API endpoint specified
#' @param data data for analysis
#' @param api api to call
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param batch send call to batch endpoint
#' @return error or response extracted from the indico API response
#' @keywords indico.io machine learning API
#' @import httr rjson stringr
make_request <- function(data, api, api_key = FALSE, cloud = FALSE, batch = FALSE) {

  # default to env variables and config file settings
  if (!is.character(cloud) && (cloud == FALSE)) {
    cloud <- .indicoio$cloud
  }
  if (!is.character(api_key) && (api_key == FALSE)) {
    api_key <- .indicoio$api_key
  }

  # compose the proper request url
  url <- request_url(cloud, api, batch, api_key)

  # configure request headers + body
  headers <- add_headers(.indicoio$header)
  body <- toJSON(list(data = data))

  response <- POST(url, accept_json(), headers, body = body)

  stop_for_status(response)

  # Returns results
  answer <- content(response, as = "parsed", type = "application/json")
  if ("error" %in% names(answer)) {
    stop(answer[["error"]])
  }

  answer[["results"]]
}

#' Returns the url for the proper indico API
#'
#' Produces the proper url to query with a given request
#' @param cloud (optional) indico subdomain for private cloud
#' @param api name of API
#' @param batch (logical) does the request contain more than one example?
#' @param api_key your personal indico API key
#' @return url for API request
#' @import httr rjson stringr
request_url <- function(cloud, api, batch, api_key) {
  # compose the proper request url
  if (cloud != FALSE && cloud != "") {
    private_cloud <- sprintf("https://%s.indico.domains/", cloud)
    base_url <- private_cloud
  } else {
    base_url <- .indicoio$remote_api
  }

  if (!is.character(api_key) && (api_key == FALSE)) {
    stop("Please provide an api key.")
  }
  url <- str_c(base_url, api)
  url <- ifelse(batch, str_c(url, '/batch'), url)
  url <- str_c(url, '?key=', api_key)
}

#' Returns a response from the indico API endpoint
#'
#' Given an input image, returns a data.frame obj
#' @param img Image to convert to a data.frame
#' @param size integer pixel size to resize images down to
#' @return data.frame constructed from image
#' @import httr rjson stringr
format_image <- function(img, size) {
  # Converts to anonymous data.frame
  if (is.character(img)) {
    if (file.exists(img)) {
      img <- readPNG(img)
    } else { # is already base64
      img <- base64decode(img)
      img <- readPNG(img)
    }
  }

  if (is.matrix(img) || is.data.frame(img)) {
    warning("Image input as matrices and dataframes will be deprecated in the next major release");
  }

  if (!is.character(img) && max(img) <= 1.0) {
    img <- img * 255
  }

  if (nrow(img) > size && ncol(img) > size) {
    img <- resizePixels(img, size, size)
  }

  if (any(is.na(img))) {
      stop("Invalid input!")
  }
  vector <- writePNG(img)
  b64 <- base64encode(vector)
  b64
}

#' Returns a list of `data.frame`s given a list of input images
#'
#' Given a list of input images, returns a list of `data.frame`s
#' @param imgs List of images to convert to a `data.frame`s
#' @param size integer pixel size to resize images down to
#' @return `data.frame`s constructed from list of images
#' @import httr rjson stringr
format_images <- function(imgs, size) {
  img_list = list()
  for (i in 1:length(imgs)) {
    img <- format_image(imgs[[i]], size)
    img_list[[i]] = img
  }
  img_list
}

resizePixels <- function(im, w, h) {
  pixels = as.vector(im)
  # initial width/height
  w1 = nrow(im)
  h1 = ncol(im)

  # target width/height
  w2 = w
  h2 = h
  # Create empty vector
  temp = vector('numeric', w2*h2)

  # Compute ratios
  x_ratio = w1/w2
  y_ratio = h1/h2

  # Do resizing
  for (i in 0:(h2-1)) {
    for (j in 0:(w2-1)) {
      px = floor(j*x_ratio)
      py = floor(i*y_ratio)
      temp[(i*w2)+j] = pixels[(py*w1)+px]
    }
  }

  m = matrix(temp, h2, w2)
  return(m)
}
