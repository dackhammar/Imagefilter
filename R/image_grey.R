#' @title image_grey
#'
#' @description
#' Takes a rgb image and converts it to grayscale.
#'
#' @param x image you have
#' @param y name of the output figure
#'
#' @return grey figures
#'
#' @importFrom magick image_read
#' @importFrom EBImage readImage
#' @importFrom EBImage writeImage
#' @importFrom EBImage channel
#'
#' @examples
#' image_grey("https://jeroen.github.io/images/frink.png","test.jpg")
#'
#' @export
image_grey <- function (x,y) {
  image_data <- readImage(x)
  new_image <- channel(image_data, "green")*0.587 + channel(image_data, "blue")*0.114 + channel(image_data, "red")*0.299

  writeImage(new_image, y)
  return(image_read(new_image))
}

