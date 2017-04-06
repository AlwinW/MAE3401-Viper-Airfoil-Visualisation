#---------------------------->
#--- Functions to  Load Data
#--- Alwin Wang MAE3401
#============================>

#--- Load a Single File ----
# This takes a file name and reads the file.
# It determines the start and the end point of the mesh and then
# reshapes the long format into a table of:
#   x | y | U | V | P | vort_xy_plane
LoadFile <- function(filename, foldername = NULL) {
  # Note: Future dev: time
  # Determine the metadata
  Re = as.numeric(
    unlist(strsplit(unlist(strsplit(filename, "Re"))[2], "AoA"))[1])
  AoA = as.numeric(
    unlist(strsplit(unlist(strsplit(filename, "AoA"))[2], ".dat"))[1])
  # Concatenate to find the correct filename
  if (!is.null(foldername)) {}
    filepath = paste(foldername, filename, sep = "/")
  # print(filepath)
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
    ID = paste0("Re", sprintf("%04d", Re), 
                "AoA", sprintf("%03d", AoA)),    
    Re = Re,
    AoA = AoA,
    filepath = filepath,
    omesh = filedata
  )
  # Return the data from the file
  return(filedata)
}


#--- Summarise Airfoil Data ----
# Given a NACA, it puts the important information into the global environment
LoadAirfoil <- function(NACA, a, c, env = FALSE) {
  # Max camber; Location of max; Thickness
  m = (NACA %/% 1000) / 100
  p = (NACA %/% 100 %% 10) / 10
  t = (NACA %% 100) / 100
  # Chord; x-shift
  c = 1
  a = - 1/2
  # Cylinder approximation of radius r and centre at (xc, yc) on the camber line
  r = 1.1019*t^2*c
  rootfind <- uniroot(
    function(x) (m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2))^2 + ((x-a)/c)^2 - r^2,
    lower = a, upper = a + p*c,
    tol = 1e-9)
  xc = rootfind$root
  yc = m/p^2 * (2*p*((xc-a)/c) - ((xc-a)/c)^2)
  # Sampling with cylinder approximiation
  thetac = atan(yc/(xc-a))      # Angle between the horizontal and raduis from (0,a) to (xc, yc)
  xsamp = xc - r*cos(3*thetac)  # No. cyl points = No. points between a and xsamp 
  # Output
  airfoildata = list(
    m = m, p = p, t = t, c = c, a = a,
    r = r,xc = xc, yc = yc, thetac = thetac, xsamp = xsamp)
  if (env == TRUE)
    list2env(airfoildata, envir = .GlobalEnv)
  return(airfoildata)
}

