#' Detects face emotion
#'
#' Given a grayscale input image of a face, returns a probability distribution over emotional state.
#' Input should be a two-dimensional structure (like data.frame or matrix), resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @param img image data
#' @param cloud subdomain for indico private cloud
#' @return List with face emotions probability pairs
#' @keywords indico.io machine learning API face emotions recognition
#' @seealso \code{\link{face_features}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr png
#' @examples
#' ## Example 1
#' img <- matrix(runif(48*48, 0, 1), nrow = 48)
#' emotion <- face_emotion(img)
#' 
#' most.possible <- sort(unlist(emotion), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected '%s' emotion with probability %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is '%s' emotion with probability %0.4f.", 
#'             names(most.possible)[2], most.possible[2]))
#' 
#' ## Example 2
#' # Reads PNG file
#' file.face <- system.file("extdata", "face1.png", package = "indicoio")
#' img <- readPNG(file.face)
#' # Converts to grayscale
#' img <- 0.2126 * img[, , 1] + 0.7152 * img[, , 2] + 0.0722 * img[, , 3]
#' # Plots image
#' plot(0:1, 0:1, xlab = "", ylab = "", axes = FALSE, col = "white")
#' rasterImage(img, xleft = 0, ybottom = 0, xright = 1, ytop = 1)
#' # Detects emotion
#' face_emotion(img)
#' 
face_emotion <- function(img, cloud = FALSE) {
  
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }
  
  if (length(dim(img)) != 2) {
    stop("Image should be represented by two-dimensional structure!")
  }
  
  # Converts to anonymous data.frame
  img <- data.frame(img)
  colnames(img) <- NULL
  
  make_request(img, 'fer', cloud)
}

#' Detects face emotion
#'
#' Given a list of grayscale input images of faces, returns a list of probability distributions over emotional state.
#' Input should be a list of two-dimensional structures (like data.frame or matrix), resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @param imgs image data
#' @param auth Username and password for HTTP Basic Auth
#' @param cloud subdomain for indico private cloud
#' @return List of lists with face emotions probability pairs
#' @keywords indico.io machine learning API face emotions recognition
#' @seealso \code{\link{face_features}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr png
#' @examples
#' ## Example 1
#' img_list = list()
#' img_list[[1]] <- matrix(runif(48*48, 0, 1), nrow = 48)
#' emotion <- face_emotion(img_list)
#' 
#' most.possible <- sort(unlist(emotion[[1]]), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected '%s' emotion with probability %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is '%s' emotion with probability %0.4f.", 
#'             names(most.possible)[2], most.possible[2]))
#' 
batch_face_emotion <- function(imgs, auth = FALSE, cloud = FALSE) {

  # convert to list of dataframes
  img_list = list()
  for (i in 1:length(imgs)) {
    img <- data.frame(imgs[[i]])
    colnames(img) <- NULL
    img_list[[i]] = img
  }

  make_request(img_list, 'fer', auth, cloud, batch = TRUE)
}

fer <- face_emotion
batch_fer <- batch_face_emotion
