#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>

#--- Optim Domain ----
OptimDom <- function(omesh, xO, surf, AoA, gradint = gradint, upper = 18, length.out = 2e5, target = 0) {
  # Determine Udash (single function call, quicker than Optim)
  dist = NormalSamp(seq(0, upper, length.out = length.out))
  lvec = NormalPoint(xO, dist, AoA, surf, gradint = gradint)
  interp = InterpProj(omesh, lvec, varnames = c("U", "V"))
  # Determine the location of the max
  if (target == 0)
    loc = which.max(interp$Udash)
  else
    loc = which.min(abs(interp$Udash - target))
  return(dist[c(loc-1, loc+1)])
}

#--- BL Thickness (optim) ----
# Uses the optim command to find the maximum and then 99% of that
BLThickOptim <- function(omesh, xO, surf, AoA, gradint = gradint, upper = 18, optimdom = TRUE) {

  # Find the maximum value for U'
  if (optimdom)
    interval = OptimDom(omesh, xO, surf, AoA, gradint = gradint, upper = upper, target = 0)
  else
    interval = c(0, upper)
  # Find the maxmum U' in this interval
  blresult <- optimise(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V"))
print(interp$Udash)
      return(interp$Udash)
    },
    interval = interval,
    maximum = TRUE)
  # Determine the 99% Um value
  blU = blresult$objective - abs(blresult$objective)*0.01
  
  # Find the distance for 99%
  if (optimdom)
    interval = OptimDom(omesh, xO, surf, AoA, gradint = gradint, upper = blresult$maximum, target = blU)
  else
    interval = c(0, blresult$maximum)
  # Find the value of dist
  result <- optimise(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V")) 
print(paste(dist, interp$Udash))
      return(abs(interp$Udash - blU))
    },
    interval = interval,
    maximum = FALSE)
  # Summarise results
  thickness = result$minimum
  return(data.frame(thickness, blU))
}


#--- BL Integrals ----
# Find the displacement, momentum and kinetic thicknesses
BLIntegrals <- function(omesh, xO, thickness, blU, surf, AoA, gradint = gradint) {
  # Displacment Thickness
  dispthickint = integrate(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V")) 
      integrand = 1 - interp$Udash / blU
      return(integrand)
    },
    lower = 0,
    upper = thickness
  )
  dispthick = dispthickint$value
  # Momentum Thickness
  momethickint = integrate(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V")) 
      integrand = interp$Udash/blU * (1 - interp$Udash / blU)
      return(integrand)
    },
    lower = 0,
    upper = thickness
  )
  momethick = momethickint$value
  # Kinetic Thickness
  kinethickint = integrate(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V")) 
      integrand = interp$Udash/blU * (1 - (interp$Udash / blU)^2 )
      return(integrand)
    },
    lower = 0,
    upper = thickness
  )
  kinethick = kinethickint$value
  return(data.frame(
    dispthick = dispthick,
    momethick = momethick,
    kinethick = kinethick
  ))
}

#--- Combine to give BL Calcs ----
BLCalcs <- function(omesh, xO, surf, AoA) {
  # Find the various gradients for the point of interest
  gradint <- AirfoilGrads(xO, surf = surf)
  # Determine the boundary value thickness
  system.time(blthickness <- BLThickOptim(omesh, xO, surf = surf, AoA, gradint = gradint, optimdom = TRUE))
  thickness = blthickness$thickness
  blU = blthickness$blU
  # Determine the integral values
  system.time(blintegrals <- BLIntegrals(omesh, xO, thickness, blU, surf = surf, AoA, gradint = gradint))
  # Combine all the data
  blvals <- data.frame(
    bldist = c(thickness, with(blintegrals, c(dispthick, momethick, kinethick))),
    bldistname = c("Thickness", "Dislacement Thickness", "Momentum Thickness", "Kinetic Energy Thickness")
  )
  # Add additional information for plotting
  blvals <- cbind(blvals, NormalPoint(xO, blvals$bldist, AoA, surf, gradint = gradint))
  # Return as a dataframe
  return(blvals)
}
