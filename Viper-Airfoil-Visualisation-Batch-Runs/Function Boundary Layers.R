#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>

#--- Find the BL Thickness ----
# This method can be used on a single xO or a vector xvec
BLThickness <- function(omesh, lvec, varnames = c("U", "V")) {
  # Find the interpolation along the points of lvec
  interpval <- InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE) %>%
    select(xO, dist, surf, Udash, Vdash, UUmdash)
  #--- 100% Boundary Layer Thickness ----
  # Manipulate the data to find the boundary layer thickness
  interpval <- interpval %>%
    # Turning Point
    mutate(tp.lag1 = lead(Udash, 1) - Udash) %>%
    mutate(tp.sign1 = sign(tp.lag1 * lag(tp.lag1)),
           tp.lag2 = tp.lag1 - lag(tp.lag1, 1)) %>%
    # UUm
    mutate(uum.diff = 1 - UUmdash) %>%
    mutate(uum.sign = sign(lead(uum.diff,1) * uum.diff)) %>%
    # Magintude of vel
    mutate(mag.vel = 1 - sqrt(Udash^2 + Vdash^2)) %>%
    mutate(mag.sign = sign(lead(mag.vel,1) * mag.vel))
  
  # Using the max thickness
  blthickmax <- interpval %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.max(Udash)) %>%
    mutate(method = "max")
  # Using the turning point
  blthicktp <- interpval %>%
    filter(tp.sign1 == -1, tp.lag2 < 0, abs(Udash) > 0.1) %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.min(dist))%>%
    mutate(method = "tp")
  # Using Uum equal to 1
  blthickuum <- interpval %>%
    filter(uum.sign == -1, abs(Udash) > 0.1) %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.min(dist))%>%
    mutate(method = "UUm")
  # Using the magnitude of the velocity
  blthickmag <- interpval %>%
    filter(mag.sign == -1, abs(Udash) > 0.1) %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.min(dist))%>%
    mutate(method = "mag")
  # Remove the object to conserve RAM
  rm(interpval)
  # Output of thicknesses (100%)
  blthickness = rbind(blthickmax, blthicktp, blthickuum, blthickmag) %>%
    group_by(method, add = TRUE)
  
  return(blthickness)
}


#--- Find the BL Values ----
# The various values of thicknesses
BLValues <- function(omesh, lvec, blthickness, varnames = c("U", "V")) {
  # Find the interpolation along the points of lvec
  interpval <- InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE) %>%
    select(xO, dist, surf, Udash, Vdash, UUmdash)
  #--- Determine the distances ----
  blvalues = list()
  for (i in 1:nrow(blthickness)) {
    soln <- interpval %>%
      ungroup() %>%
      filter(xO == blthickness$xO[i],
             surf == blthickness$surf[i],
             dist <= blthickness$dist[i],
             Udash < blthickness$Udash[i] * 0.99) %>%
      slice(which.max(dist))
    
    integrand <- interpval %>%
      ungroup() %>%
      filter(xO == blthickness$xO[i],
             surf == blthickness$surf[i],
             dist <= soln$dist) %>%
      mutate(Ur = Udash / soln$Udash,
             dispthick = 1 - Ur,
             momethick = Ur * (1 - Ur),
             kinethick = Ur * (1 - (Ur)^2)) %>%
      select(dispthick, momethick, kinethick)
    
    distances <- (
      apply(integrand, 2, sum) -
        1/2 * integrand[1,] -
        1/2 * integrand[nrow(integrand),]) *
      soln$dist/(nrow(integrand)/2)
    
    blvalues[[i]] <- cbind(
      soln, thickness = soln$dist, distances, method = blthickness$method[i])
  }
  blvalues = bind_rows(blvalues)
  return(blvalues)
}


# #--- Combine to give BL Calcs ----
# BLCalcs <- function(omesh, xO, surf, AoA, Re) {
#   # Find the various gradients for the point of interest
#   gradint <- AirfoilGrads(xO, surf = surf)
#   # Determine the boundary value thickness
#   blthickness <- BLThickOptim(omesh, xO, surf = surf, AoA, gradint = gradint)
#   thickness = blthickness$thickness
#   blU = blthickness$blU
#   # Determine the theoretical flow
#   theory = 5 * (xO - a) / sqrt(Re * (xO - a))
#   # Determine the integral values
#   blintegrals <- BLIntegrals(omesh, xO, thickness, blU, surf = surf, AoA, gradint = gradint)
#   # Combine all the data
#   blvals <- data.frame(
#     bldist = c(thickness, theory, with(blintegrals, c(dispthick, momethick, kinethick))),
#     bldistname = c("Thickness", "Theory", "Dislacement Thickness", "Momentum Thickness", "Kinetic Energy Thickness")
#   )
#   # Add additional information for plotting
#   blvals <- cbind(blvals, NormalPoint(xO, blvals$bldist, AoA, surf, gradint = gradint))
#   # Return as a dataframe
#   return(blvals)
# }
