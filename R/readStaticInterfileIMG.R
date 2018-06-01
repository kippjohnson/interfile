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
#' # Check out the results (returned as a matrix)
#' dim(my_scan)
#' head(my_scan)
readHeaderFileHDR <- function(fname) {
header <- fname
  if (!grepl("\\.hdr$", header)) {
    # if filename pasted in does not
    hdr.file <-
      paste0(header, ".hdr") # end in .hdr, append .hdr to it
  } else{
    hdr.file <- header
  }
  to.read <- file(hdr.file, "rt") #open for reading in text mode

  all_data <- readLines(to.read) #extract all lines
    for (i in 1:length(all_data)) { #iterate through all lines
      line <- all_data[i]

      #Determines the byte order
      if (grepl("byte order", line, ignore.case = TRUE)) {
        if (grepl("little", line, ignore.case = TRUE)) {
          endian <- "little"
        }
        if (grepl("big", line, ignore.case = TRUE)) {
          endian <- "big"
        }
      }
      #determines the row size
      if (grepl("matrix size \\[1\\]", line, ignore.case = TRUE)) {
        row <- substr(c, nchar(c) - 5 + 1, nchar(c))
        row <- as.numeric(gsub("\\D", "", row))
        file.row <- row
      }
      #determines the column size
      if (grepl("matrix size \\[2\\]", line, ignore.case = TRUE)) {
        column <- substr(line, nchar(line) - 5 + 1, nchar(line))
        column <- row <- as.numeric(gsub("\\D", "", column))
        file.column <- column
      }
      #determines the pixel size
      if (grepl("per pixel", line, ignore.case = TRUE)) {
        bytes.per.pixel <- as.numeric(gsub("\\D", "", line))
       # bytes.per.pixel <- substr(line, nchar(line) - 3 + 1, nchar(line))
      #  bytes.per.pixel <- gregexpr("[0-9]+", bytes.per.pixel)
      }
      #determines the data format
      if (grepl("number format", line, ignore.case = TRUE)) {
        if (grepl("float", line, ignore.case = TRUE)) {
          what <- "double"
        }
        if (grepl("float", line, ignore.case = TRUE)) {
          what <- "double"
        }
        if (grepl("numeric", line, ignore.case = TRUE)) {
          what <- "numeric"
        }
        if (grepl("integer", line, ignore.case = TRUE)) {
          what <- "integer"
        }
        if (grepl("int", line, ignore.case = TRUE)) {
          what <- "int"
        }
        if (grepl("logical", line, ignore.case = TRUE)) {
          what <- "logical"
        }
        if (grepl("complex", line, ignore.case = TRUE)) {
          what <- "complex"
        }
        if (grepl("character", line, ignore.case = TRUE)) {
          what <- "character"
        }
        if (grepl("raw", line, ignore.case = TRUE)) {
          what <- "raw"
        }
      }
      i <- i + 1
    }
  close(to.read) #close connection
  fname <- sub(".hdr$", ".img", hdr.file) #change file to .img
  number <- regexpr("\\/[^\\/]*$", fname) + 1
  fname <- substring(fname, number) #obtain fname of .img
  folder <- substring(hdr.file, 0, number - 2)
  number1 <- regexpr("\\/[^\\/]*$", folder) + 1
  folder <- substring(hdr.file, number1, number - 2) #obtain folder name containg file
  fname <-
    system.file(folder, fname, package = "interfile") #store .img file in fname
  readStaticInterfileIMG(fname, endian, file.row, file.column, what, bytes.per.pixel) #call binary file reader and return
}

readStaticInterfileIMG <- function(fname,
                                   endian,
                                   file.row,
                                   file.col,
                                   what,
                                   bytes.per.pixel) {
  if (!grepl("\\.img$", fname)) {
    # if filename pasted in does not
    img.file <-
      paste0(fname, ".img") # end in .img, append .img to it
  } else{
    img.file <- fname
  }

  to.read <- file(img.file, "rb") # open file in binary mode

  n.pixel <- file.row * file.col
  dat <-
    matrix(NA, nrow = n.pixel, ncol = 1) # matrix to store results

  i <- 1

  while (i <= n.pixel) {
    res <- readBin(
      con = to.read,
      what = what,
      n = 1,
      size = bytes.per.pixel,
      endian = endian
    )

    dat[i, 1] <- res

    i <- i + 1

  } # end while loop

  close(to.read) # close binary connection

  # Reshape interfile data into matrix
  dat.mat <- matrix(dat, ncol = file.col, nrow = file.row)
  return(dat.mat)
}
