#----------------------------
#--- Functions to  Load Data
#============================

#--- Load a Single File ----
# This takes a file name and reads the file.
# It determines the start and the end point of the mesh and then
# reshapes the long format into a table of:
#   x | y | U | V | P | vort_xy_plane
LoadFile <- function(filename) {
  # Determine the starting and ending lines
  rstart = grep("DT=", readLines(filename))[1]
  rend = grep(" 1 ", readLines(filename))[1]
  # Read the data from the filename given
  filedata <- read.table(filename, sep = " ",
                         skip = rstart,
                         nrow = rend - rstart - 1)
  # Re-shape the data into correct columns
  filedata <- as.data.frame(matrix(c(t(filedata[2:6])), ncol=6))
  # Give the columns of the data appropriate names
  colnames(filedata) <- c("x", "y", "U", "V", "P", "vort_xy_plane")
  # Return the data from the file
  return(filedata)
}

LoadFolder <- function(folderpath = "Input_Data") {
  # Get a list of the files
  filelist <- list.files(path = folderpath, pattern = "*.dat")

}