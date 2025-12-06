#' @title image_cutoff
#'
#' @description
#' Takes a rgb image and applies a cut-off filter where pixels with #808080 gets
#' colored pink #FF00FF and other pixels #000000.
#'
#' @param x image you have
#' @param y name of the output figure
#'
#' @return cutoff figures
#'
#' @importFrom magick image_read
#' @importFrom magick image_write
#' 
#' @examples
#' \donttest{
#' tmp <- tempfile(fileext = ".jpg")
#' image_cutoff(
#'   "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRKwyTL9drJIaBbGx_-p8ogSLh_9DcPVDM8mQ&s",
#'   tmp
#' )
#' }
#' 
#' @export
image_cutoff <- function (x,y) {
  pic<-image_read(x) # Read image
  bitmap<-pic[[1]] # Extract bitmap
  # Review notes: Removed the bitmap_cutoff, we can apply changes directly to bitmap.
  # Added the below so we can handle images with rgba (derived from edit in image_grey)
  dims<-dim(bitmap)
  if (dims[1] == 4) {
    a<-bitmap[4,,,drop = F] # Extract alpha channel
    bitmap<-bitmap[1:3,,] # Extract
  }
  # Iterate over bitmap
  # Review notes: Changed this to seq by 3 to remove if else.
  for (i in seq(1, length(bitmap), by = 3)) {
      greyscale<-strtoi(bitmap[i],16)*0.299+strtoi(bitmap[i+1],16)*0.587+strtoi(bitmap[i+2],16)*0.114
      # Apply cut-off
      # Review notes: One might consider adding an argument to define the cutoff)
      if (greyscale < 128) {
        bitmap[i] <- as.raw(0)
        bitmap[i+1] <- as.raw(0)
        bitmap[i+2] <- as.raw(0)
      }
      else {
        bitmap[i] <- as.raw(255)
        bitmap[i+1] <- as.raw(0)
        bitmap[i+2] <- as.raw(255)
      }
  }
  # Apply alpha channel back if 4 channel input
  if (dims[1] == 4) {
    out <- array(as.raw(0), dim = dims)
    out[1:3,,] <- bitmap
    out[4,,] <- a
    bitmap <- out
  }
  # Read image from bitmap
  output <- image_read(bitmap)
  # Save image
  image_write(output, y)
  # Return image
  return(output)
}

