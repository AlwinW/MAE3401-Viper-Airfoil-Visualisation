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
  suppressWarnings(
  lmesh <- lapply(
    varnames,
    function(var) {
      interpvar <- as.data.frame(interpp(
                  x = omesh$x, y = omesh$y, z = omesh[[var]],
                  xo = lvec[[1]], yo = lvec[[2]],
                  linear = linear,
                  extrap = extrap))
      interpvar <- interpvar[3]
      colnames(interpvar) <- var
      return(interpvar)
    })
  )
  # Append these interpolations to the original data
  lmesh <- bind_cols(lvec, lmesh)
  # Return output
  return(lmesh)
}


#--- Vector Proj of Interpolation ----
InterpProj <- function(omesh, lvec, varnames = c("U", "V", "P", "vort_xy_plane"),
                       linear = TRUE, extrap = FALSE, plotsurf = FALSE) {
  # Interpolate  to find the varibles
  lmesh <- InterpPoint(omesh, lvec, varnames, linear, extrap)
  # Use vector projection parallel to the normal
  lmesh <- lmesh %>%
    mutate(surf = ifelse(plotsurf == FALSE & surf == "upper" & dely > 0, "upper", "lower")) %>%
    mutate(surf = ifelse(plotsurf == FALSE & surf == "lower" & dely < 0, "lower", "upper")) %>%
    # Udash and Vdash found by using vector projections
    mutate(Udash = sqrt((U - (delx*U + dely*V)/dist^2 * delx)^2 + (V - (delx*U + dely*V)/dist^2 * dely)^2),
           Vdash = (delx*U + dely*V)/dist) %>%
    mutate(Udash = ifelse(dist != 0, Udash, 0),
           Vdash = ifelse(dist != 0, Vdash, 0)) %>%
    # sign of Udash found by cross product, upper vs lower
    mutate(Udash = sign(dely*U - delx*V) * ifelse(surf == "upper", 1, -1) * Udash)
  
  # print(paste(lmesh$dist, lmesh$Udash))
  
  return(lmesh)
}


