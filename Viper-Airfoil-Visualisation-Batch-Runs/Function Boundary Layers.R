#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>

#--- Optim Domain ----
OptimDom <- function(omesh, xO, surf, AoA, gradint = gradint, upper = 15, length.out = 2e5, target = 0) {
  # Determine Udash (single function call, quicker than Optim)
  dist = NormalSamp(seq(0, upper, length.out = length.out))
  lvec = NormalPoint(xO, dist, AoA, surf, gradint = gradint)
  interp = InterpProj(omesh, lvec, varnames = c("U", "V"))
  # Determine the location of the max
  if (target == 0)
    loc = which.max(interp$Udash)
  else
    loc = which.min(abs(interp$Udash - target))
  # Optim
  optim = select(interp[loc,], dist, Udash)
  return(optim)
}

#--- BL Thickness (optim) ----
# Uses the optim command to find the maximum and then 99% of that
BLThickOptim <- function(omesh, xO, surf, AoA, gradint = gradint, upper = 18) {
  # Find the maximum value for U'
  blresult = OptimDom(omesh, xO, surf, AoA, gradint = gradint, upper = upper, target = 0)
  blU = blresult$Udash - abs(blresult$Udash)*0.01
  
  # Find the distance for 99%
  result = OptimDom(omesh, xO, surf, AoA, gradint = gradint, upper = blresult$dist, target = blU)
  thickness = result$dist
  return(data.frame(thickness, blU))
}


#--- BL Integrals ----
# Find the displacement, momentum and kinetic thicknesses
BLIntegrals <- function(omesh, xO, thickness, blU, surf, AoA, gradint = gradint, length.out = 2e5) {
  # Use Simpson's 3/8 Rule
  length.out = length.out + (4 - length.out%% 3)
  # Interpolate
  dist = seq(0, thickness, length.out = length.out)
  lvec = NormalPoint(xO, dist, AoA, surf, gradint = gradint)
  interp = InterpProj(omesh, lvec, varnames = c("U", "V"))
  # Find the integrands
  integrand <- interp %>%
    mutate(dispthick = 1 - Udash/blU,
           momethick = Udash/blU * (1 - Udash / blU),
           kinethick = Udash/blU * (1 - (Udash / blU)^2)) %>%
    select(dispthick, momethick, kinethick)
  
  # # Using 3/8 rule
  # distances <- 3/8* (
  #   3 * apply(integrand, 2, sum) -
  #   apply(integrand[rep(c(TRUE, FALSE, FALSE), length.out = length.out),], 2, sum) -
  #   1 * integrand[1,] -
  #   1 * integrand[length.out,]) *
  #   thickness/length.out
  
  # Using trapezium rule
  distances <- (
    apply(integrand, 2, sum) -
    1/2 * integrand[1,] -
    1/2 * integrand[length.out,]) *
    thickness/length.out
  
  return(distances)
}

#--- Combine to give BL Calcs ----
BLCalcs <- function(omesh, xO, surf, AoA, Re) {
  # Find the various gradients for the point of interest
  gradint <- AirfoilGrads(xO, surf = surf)
  # Determine the boundary value thickness
  blthickness <- BLThickOptim(omesh, xO, surf = surf, AoA, gradint = gradint)
  thickness = blthickness$thickness
  blU = blthickness$blU
  # Determine the theoretical flow
  theory = 5 * (xO - a) / sqrt(Re * (xO - a))
  # Determine the integral values
  blintegrals <- BLIntegrals(omesh, xO, thickness, blU, surf = surf, AoA, gradint = gradint)
  # Combine all the data
  blvals <- data.frame(
    bldist = c(thickness, theory, with(blintegrals, c(dispthick, momethick, kinethick))),
    bldistname = c("Thickness", "Theory", "Dislacement Thickness", "Momentum Thickness", "Kinetic Energy Thickness")
  )
  # Add additional information for plotting
  blvals <- cbind(blvals, NormalPoint(xO, blvals$bldist, AoA, surf, gradint = gradint))
  # Return as a dataframe
  return(blvals)
}
