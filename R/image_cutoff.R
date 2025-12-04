#' @title image_cutoff
#'
#' @param x image you have
#' @param y name of the output figure
#'
#' @return cutoff figures
#'
#' @examples
#' image_cutoff("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRKwyTL9drJIaBbGx_-p8ogSLh_9DcPVDM8mQ&s","test.jpg")
#'
image_cutoff <- function (x,y) {{{
  pic<-image_read(x)
  bitmap<-pic[[1]]
  bitmap_cutoff<-bitmap
  for (i in 1:length(bitmap)) {

    if (i %% 3 == 1){
      greyscale<-strtoi(bitmap[i],16)*0.299+strtoi(bitmap[i+1],16)*0.587+strtoi(bitmap[i+2],16)*0.114
      if (greyscale < 128) {
        bitmap_cutoff[i] <- as.raw("00")
        bitmap_cutoff[i+1] <- as.raw("00")
        bitmap_cutoff[i+2] <- as.raw("00")
      }
      else {
        bitmap_cutoff[i] <- as.raw("255")
        bitmap_cutoff[i+1] <- as.raw("00")
        bitmap_cutoff[i+2] <- as.raw("255")
      }

    }

    else {
      i = i+1
    }
  }
}

  image_write(image_read(bitmap_cutoff), y)}
  image_read(bitmap_cutoff)}

