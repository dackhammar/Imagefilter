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
#'
#' @examples
#' image_grey("https://jeroen.github.io/images/frink.png","test.jpg")
#'
#' @export
image_grey <- function (x,y) {
  # Review notes:
  # I choose to use your logic from the cutoff function instead of
  # relying on the functions from EBImage since you already had created
  # a function for generating grayscale values and modifying the bitmap
  # image.
  pic<-magick::image_read(x) # Read image
  bitmap<-pic[[1]] # Extract bitmap
  # Review notes:
  # This was tricky, test image was transparent png with alpha channel.
  # So we need to remove the alpha channel to use the same approach.
  # Option would be to vectorize over bitmap[1,,], bitmap[2,,] and, bitmap[3,,]
  # Check number of channels
  dims<-dim(bitmap)
  if (dims[1] == 4) {
    a<-bitmap[4,,,drop = F] # Extract alpha channel
    bitmap<-bitmap[1:3,,] # Extract
  }
  # Review notes: Removed the bitmap_cutoff, we can apply changes directly to bitmap.
  # Iterate over bitmap
  # Review notes: Changed this to seq by 3 to remove if else.
  for (i in seq(1, length(bitmap), by = 3)) {
      # Calculate grayscale value
      greyscale <- strtoi(bitmap[i],16)*0.299+strtoi(bitmap[i+1],16)*0.587+strtoi(bitmap[i+2],16)*0.114
      # Apply to all channels
      bitmap[i] <- as.raw(greyscale)
      bitmap[i+1] <- as.raw(greyscale)
      bitmap[i+2] <- as.raw(greyscale)
  }
  # Apply alpha channel back if 4 channel input
  if (dims[1] == 4) {
    out <- array(as.raw(0), dim = dims)
    out[1:3,,] <- bitmap
    out[4,,] <- a
    bitmap <- out
  }

  # Read image from bitmap
  output <- magick::image_read(bitmap)
  # Save file
  magick::image_write(output, y)
  # Return image
  return(output)
}
