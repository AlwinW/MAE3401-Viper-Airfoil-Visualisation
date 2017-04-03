#----------------------------
#--- Functions to  Load Data
#============================

#--- Load a Single File ----
# This takes a file name and reads the file.
# It determines the start and the end point of the mesh and then
# reshapes the long format into a table of:
#   x | y | U | V | P | vort_xy_plane
LoadFile <- function(filename, foldername = NULL) {
  # Determine the metadata
  Re = as.numeric(
    unlist(strsplit(unlist(strsplit(filename, "Re"))[2], "AoA"))[1])
  AoA = as.numeric(
    unlist(strsplit(unlist(strsplit(filename, "AoA"))[2], ".dat"))[1])
  # Concatenate to find the correct filename
  if (!is.null(foldername)) {}
    filepath = paste(foldername, filename, sep = "/")
  # Determine the starting and ending lines
  rstart = grep("DT=", readLines(filepath))[1]
  rend = grep(" 1 ", readLines(filepath))[1]
  # Read the data from the filepath given
  filedata <- read.table(filepath, sep = " ",
                         skip = rstart,
                         nrow = rend - rstart - 1)
  # Re-shape the data into correct columns
  filedata <- as.data.frame(matrix(c(t(filedata[2:6])), ncol=6))
  # Give the columns of the data appropriate names
  colnames(filedata) <- c("x", "y", "U", "V", "P", "vort_xy_plane")
  # Combind the filedata and metadata into one list
  filedata <- list(
    Re = Re,
    AoA = AoA,
    filepath = filepath,
    filedata = filedata
  )
  # Return the data from the file
  return(filedata)
}

#--- Load a Folder ----
# This takes a folder name and reads the list of files.
# Each of the files is then read by calling the "LoadFile" function
LoadFolder <- function(foldername = "Input_Data") {
  # Get a list of the files
  filelist <- list.files(path = foldername, pattern = "*.dat")
  # Load the data for each file using parallel cores
  parallelCluster <- parallel::makeCluster(parallel::detectCores())
  folderdata <- parallel::parLapply(
    parallelCluster, filelist, LoadFile, foldername = foldername)
  stopCluster(parallelCluster)
  # Recombine Re and AoA since the order may have been shifted around
  #   during the parallel work
  listnames <- unlist(lapply(folderdata, function(x) 
    paste("Re", sprintf("%04d", x$Re), "AoA", sprintf("%03d", x$AoA), sep = "")
    ))
  # Set the names of folderdata and then reorder them
  names(folderdata) <- listnames
  folderdata <- folderdata[order(listnames)]
  # Return the list
  return(folderdata)
}



