#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>

#--- BL Thickness (optim) ----
# Uses the optim command to find the maximum and then 99% of that
BLThickOptim <- function(omesh, xO, surf, AoA) {
  # Find the maximum value for U'
  blresult <- optimise(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V"))
      # print(interp$Udash)
      return(interp$Udash)
    },
    interval = c(0,18),
    maximum = TRUE)
  # Determine the 99% Um value
  blU = blresult$objective - abs(blresult$objective)*0.01
  # Find the distance for 99%
  result <- optimise(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V")) 
      # print(paste(dist, interp$Udash))
      return(abs(interp$Udash - blU))
    },
    interval = c(0, blresult$maximum),
    maximum = FALSE)
  # Summarise results
  thickness = result$minimum
  return(data.frame(thickness, blU))
}



BLCalcs <- function(omesh, xO, surf, AoA) {
  # Find the various gradients for the point of interest
  gradint <- AirfoilGrads(xO, surf = surf)
  # Determine the boundary value thickness
  blthickness <- BLThickOptim(omesh, xO, surf, AoA)
  thickness = blthickness$thickness
  blU = blthickness$blU
  
  xvp <- NormalSamp(seq(0, blthickness$thickness * 1.5, by = 0.01))
  lvecvp <- NormalPoint(xO, xvp, AoA, surf, gradint = gradint)
  interpvp <- InterpProj(omesh, lvecvp, varnames = c("U", "V")) 
  ggplot() + 
    geom_path(data = interpvp, aes(x = Udash, y = dist))
  
  ggplot() + 
    geom_path(data = interpvp, aes(x = 1 - Udash/blU, y = dist))
  
}

#--- BL Integrals ----
BLIntegrals <- function(omesh, xO, thickness, blU, surf, AoA) {
  # Get the important data
  dispthick = integrate(
    function(dist) {
      lvec <- NormalPoint(xO, dist, AoA, surf, gradint = gradint)
      interp <- InterpProj(omesh, lvec, varnames = c("U", "V")) 
      integrand = 1 - interp$Udash / blU
      return(integrand)
    },
    lower = 0,
    upper = thickness
  )
}


