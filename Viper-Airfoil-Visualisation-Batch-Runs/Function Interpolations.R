#---------------------------->
#--- Functions for Interpolations
#--- Alwin Wang MAE3401
#============================>

#--- Interpolate at a given point ----
# This uses the interpolation function from akima: interpp
# ASSUMES that x and y points are the first two columns
InterpPoint <- function(omesh, lvec, varnames = c("U", "V", "P", "vort_xy_plane"),
                        linear = TRUE, extrap = FALSE) {
  # Loop through each variable to interpolate
  lmesh <- lvec
  for (var in varnames) {
    lmeshv <- suppressWarnings(
      as.data.frame(interpp(x = omesh$x, y = omesh$y, z = omesh[[var]],
                            xo = lvec[[1]], yo = lvec[[2]],
                            linear = linear,
                            extrap = extrap)))
    lmesh <- cbind(lmesh, lmeshv[3])
  }
  # Give the columns meaningful names
  colnames(lmesh) <- c(colnames(lvec), varnames)
  return(lmesh)
}


#--- Vector Proj of Interpolation ----
InterpProj <- function(omesh, lvec, varnames = c("U", "V", "P", "vort_xy_plane"),
                       linear = TRUE, extrap = FALSE) {
  # Interpolate  to find the varibles
  lmesh <- InterpPoint(omesh, lvec, varnames, linear, extrap)
  # Use vector projection parallel to the normal
  lmesh <- lmesh %>%
    mutate(Udash = sqrt((U - (delx*U + dely*V)/dist^2 * delx)^2 + (V - (delx*U + dely*V)/dist^2 * dely)^2),
           Vdash = (delx*U + dely*V)/dist) %>%
    mutate(Udash = ifelse(dist != 0, Udash, 0),
           Vdash = ifelse(dist != 0, Vdash, 0))
}


