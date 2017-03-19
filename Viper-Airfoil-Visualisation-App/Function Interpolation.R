#----------------------------
#--- Functions for Interpolations
#============================

#--- Interpolate the original mesh into a rectangular grid ----

#--- Interpolate the original mesh along a line ----
InterpLine <- function(omesh, 
                       xO = NA, AoA = 0, surf = "upper", eq = "norm", 
                       focusdist = 0.5, totaldist = 20, len = 51,
                       lvec = NA, 
                       varnames = c("U", "V", "P", "vort_xy_plane"),
                       linear = TRUE) {
  # Determine if lvec needs to be calcuated
  if(is.na(lvec))
    lvec = AirfoilLineGen(xO, AoA, surf, eq, focusdist, totaldist, len)
  # Loop through each variable to interpolate
  lmesh <- lvec
  for (var in varnames) {
    lmeshv <- suppressWarnings(
      as.data.frame(interpp(x = omesh$x, y = omesh$y, z = omesh[[var]],
                            xo = lvec$x, yo = lvec$y,
                            linear = linear)))
    lmesh <- cbind(lmesh, lmeshv[3])
  }
  # Give the columns meaningful names
  colnames(lmesh) <- c(colnames(lvec), varnames)
  return(lmesh)
}
