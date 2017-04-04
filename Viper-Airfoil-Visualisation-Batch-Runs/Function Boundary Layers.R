#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>


#--- BL Thickness (optim) ----
# 
BLThickOptim <- function(omesh, xO, surf, AoA) {
  # Determine the gradient at xO
  gradint <- AirfoilGrads(xO, surf = surf)
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
      print(paste(dist, interp$Udash))
      return(abs(interp$Udash - blU))
    },
    interval = c(0, blresult$maximum),
    maximum = FALSE)
  # Summarise results
  thickness = result$objective
  blU = result$minimum
  
  
}



