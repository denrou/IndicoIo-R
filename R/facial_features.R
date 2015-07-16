#' Detects facial features
#'
#' Given an grayscale input image of a face, returns a 48 dimensional feature vector explaining that face.
#' Useful as a form of feature engineering for face oriented tasks.
#' Input should be in a list of list format, resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @inheritParams face_emotion
#' @return List with face features
#' @keywords indico.io machine learning API face features recognition
#' @seealso \code{\link{face_emotion}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img <- matrix(runif(48*48, 0, 1), nrow = 48)
#' features <- facial_features(img)
#' length(features)
#'
facial_features <- function(img, api_key = FALSE, cloud = FALSE, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  if (!is.character(img) && length(dim(img)) != 2) {
    stop("Image should be represented by two-dimensional structure!")
  }

  img <- format_image(img, 48)
  make_request(img, 'facialfeatures', api_key, cloud, ...)
}

#' Detects facial features
#'
#' Given a list of grayscale input images of a face, returns a 48 dimensional feature vector explaining that face.
#' Useful as a form of feature engineering for face oriented tasks.
#' Input should be in a list of list of lists format, resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @inheritParams batch_face_emotion
#' @return List of lists with facial features
#' @keywords indico.io machine learning API face features recognition
#' @seealso \code{\link{face_emotion}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img_list = list()
#' img_list[[1]] = matrix(runif(48*48, 0, 1), nrow = 48)
#' features <- batch_facial_features(img_list)
#' length(features)
#' length(features[[1]])
#'
batch_facial_features <- function(imgs, api_key = FALSE, cloud = FALSE, ...) {
  img_list <- format_images(imgs, 48)
  make_request(img_list, 'facialfeatures', api_key, cloud, batch = TRUE, ...)
}

face_features <- facial_features
batch_face_features <- batch_facial_features
