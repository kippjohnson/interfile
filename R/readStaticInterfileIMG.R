#' Read interfile binary file format images
#'
#' This function imports static (2D) binary interfile files.
#'
#' @param fname Filename of the interfile
#' @param endian Endian-ness of the file you are attempting to read. Permitted values are "big" or "little"
#' @param file.row Rows of pixels in the image
#' @param file.col Columns of pixels in the image
#' @param what Type of binary data stored: permitted values are "numeric", "double", "integer", "int", "logical", "complex", "character", "raw".
#' @param bytes.per.pixel Number of bytes per pixel. This information shold be stored in the header file (.hdr) of the interfile.
#'
#' @return A matrix of pixel intensities
#' @export readStaticInterfileIMG
#'
#' @examples
#'
#' # Load an example interfile (Rabbit aorta, Tc99m scan)
#' interfile.location <- system.file("extdata", "Tc99m_20percent_energy.img", package = "interfile")
#'
#' # Read the binary interfile
#' my_scan <- readStaticInterfileIMG(interfile.location)
#'
#' # Check out the results (returned as a matrix)
#' dim(my_scan)
#' head(my_scan)

readStaticInterfileIMG <- function(fname,
                                   endian="little",
                                   file.row=80,
                                   file.col=80,
                                   what="double",
                                   bytes.per.pixel=4){

  if(! grepl("\\.img$", fname)){  # if filename pasted in does not
    img.file <- paste0(fname, ".img") # end in .img, append .img to it
  }else{
    img.file <- fname
  }

  to.read <- file(img.file, "rb") # open file in binary mode

  n.pixel <- file.row * file.col
  dat <- matrix(NA, nrow=n.pixel, ncol=1) # matrix to store results

  i <- 1

  while(i <= n.pixel){
    res <- readBin(con=to.read,
                   what=what,
                   n=1,
                   size=bytes.per.pixel,
                   endian=endian)

    dat[i, 1] <- res

    i <- i + 1

  } # end while loop

  close(to.read) # close binary connection

  # Reshape interfile data into matrix
  dat.mat <- matrix(dat, ncol=file.col, nrow=file.row)
  return(dat.mat)
}
