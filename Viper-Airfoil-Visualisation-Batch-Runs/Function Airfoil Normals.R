#---------------------------->
#--- Functions for Airfoil Normals
#--- Alwin Wang MAE3401
#============================>

#--- Creating xvec for norms ----
# Allows for better, more evened sampling of normals
# A cyclinder approximation is used for the leading edge
AirfoilSamp <- function(xvec, del = c*8e-6, cylinder = FALSE) {
  # Sample according to a cubic function
  xvec = -2*a/c^3 * (xvec - a)^3 + a
  # Add extra x values for interpolation if cylinder used
  if (cylinder != FALSE & xvec[1] == a) {
    # Determine the number of points from -theta_c to theta_c
    xadd = seq(-0.0001, -thetac,  
               length.out = ceiling(length(xvec[xvec < xsamp])/2 + 1))
    xadd = xadd[xadd != -thetac]
    # 'encode it' and combine
    xadd = a - abs(a) + xadd
    # Return the result depending on what's required
    if (cylinder == TRUE)
      xvec = c(xadd, xvec)
    if (cylinder == "only")
      xvec = xadd
  }
  # Remove any unecessary LE
  xvec = xvec[xvec != a]
  # Adjust the TE value
  if (xvec[length(xvec)] == a + c)
    xvec[length(xvec)] = a + c - sign(a + c)*abs(a + c)*del
  
  return(xvec)
}


#--- Grad: NACA ----
# Numerically finds the gradients etc using the equations for
# the airfoil
AirfoilGradNACA <- function(xO, surf, del) {
  # Determine the value of x for xO on the airfoil and neighbours
  x = Airfoilx(xO, surf = surf)
  x = c(x-del, x, x + del)
  # Determine the values
  surfval = AirfoilCurve(x, out = surf)
  # Estimate the gradients
  surfval <- mutate(surfval,
                    dydx = (y - lag(y, 1)) / (x - lag(x, 1)),
                    dydxave = (dydx + lag(dydx, 1)) / 2)
  dydx = surfval$dydxave[3]
  # Determine normal and tagential equations for output
  out <- list(out = data.frame(
    surf = surf,
    eq = c("tan", "norm"),
    x = surfval$x[2],
    y = surfval$y[2],
    m = c(dydx, -1/dydx)) %>%
      mutate(c = -m*x + y)
  )
  return(out)
}


#--- Grad: Cylinder ----
# Algebraically finds the gradients etc using the geometry of
# a cyclinder
AirfoilGradCyl <- function(xO, surf, del) {
  thetaO = xO - a + abs(a)
  thetaO = ifelse(surf == "upper", 1, -1) * thetaO
  mN = tan(thetaO)
  # Generate the output
  out <- list(out = data.frame(
    surf = surf,
    eq = c("tan", "norm"),
    x = xc -r*cos(thetaO),
    y = yc -r*sin(thetaO),
    m = c(-1/mN, mN)) %>%
      mutate(c = -m*x + y)
  )
  return(out)
}

#--- Grad: Given some x ----
# Determines the gradients etc at a point x on the airfoil
AirfoilGrads <- function(xO, surf = "upper", del = c*1e-8, out = "all") {
  # Determine which geometry to use: airfoil or cylinder
  out <- ifelse(xO < a,
                AirfoilGradCyl(xO, surf, del),
                AirfoilGradNACA(xO, surf, del))
  out <- out[[1]]
  return(out)
}


