save_image_ok <- function (x, width = NULL, height = NULL, file = NULL, ...) 
{
  if (is.null(width) + is.null(height) == 0) {
    message("Both width and height were defined. Use width only by default. ")
    height <- NULL
  }
  if (is.null(file)) {
    temp_png <- tempfile(fileext = ".png")
  }
  else {
    temp_png <- file
  }
  temp_img <- save_kable(x = x, file = temp_png, ...)
  img_dpi <- 600
  if (is.null(width) + is.null(height) <= 1 & is.null(attr(temp_img, 
                                                           "info"))) {
    warning("You need to install magick in order to use width/height in ", 
            "as_image. ")
  }
  else {
    if (!is.null(width)) {
      img_dpi <- attr(temp_img, "info")$width/width
    }
    if (!is.null(height)) {
      img_dpi <- attr(temp_img, "info")$height/height
    }
  }
  include_graphics(temp_png, dpi = img_dpi)
}