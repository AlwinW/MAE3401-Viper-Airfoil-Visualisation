#----------------------------
#--- Functions to  Load Data
#============================

#--- Load data from a .dat file ----
LoadData <- function(filename) {
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

#--- Summarise Critical Airfoil Data ----
AirfoilData <- function(NACA, a, c) {
  # Max camber; Location of m; Thickness
  m = (NACA %/% 1000) / 100
  p = (NACA %/% 100 %% 10) / 10
  t = (NACA %% 100) / 100
  # Chord; x-shift
  c = 1
  a = - 1/2
  # Cylinder approx
  r = 1.1019*t^2*c
  rootfind <- uniroot(function(x) (m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2))^2 + ((x-a)/c)^2 - r^2,
                      lower = a, upper = a + p*c,
                      tol = 1e-9)
  xc = rootfind$root
  yc = m/p^2 * (2*p*((xc-a)/c) - ((xc-a)/c)^2)
  thetac = atan(yc/(xc-a)) # Angle between the horizontal and raduis from (0,a) to (xc, yc)
  xsamp = xc + r*cos(3*thetac)
  # Output
  airfoildata = list(m = m, p = p, t = t, c = c, a = a,
                     r = r,xc = xc, yc = yc, thetac = thetac, xsamp = xsamp)
  list2env(airfoildata, envir = .GlobalEnv)
  return(airfoildata)
}

# Additional information required:
# - Angle of Attack
# - NACA series