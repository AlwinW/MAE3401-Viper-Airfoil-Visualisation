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
    # x = surfval$x[2],
    x = xO,
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
  return(out)
}


#--- Creating dist vector for interp ----
NormalSamp <- function(dist, polyn = 3) {
  distmax = max(dist)^(polyn-1)
  dist = dist^polyn /(distmax)
  return(dist)
}


#--- (x,y) distance dist from xO ----
# Given: xO and surf
# Finds: (x, y) at a normal distance away from the surface
NormalPoint <- function(xO, dist, AoA = 0, surf = "upper", eq = "norm", gradint = NA) {
  # Find the gradient at the xO point
  gradint <- ifelse(is.na(gradint), AirfoilGrads(xO, surf = surf), gradint) 
  gradint <- gradint[[1]]
    # Note in AirfoilGrads it rootfinds for x already
  # Determine the location of (xp, yp) for a given distance
  gradint <- gradint %>%
    filter(surf == get("surf") & eq == get("eq")) %>%
    cbind(xO, ., dist) %>%
    mutate(xdist = sign(m) * dist/sqrt(1+m^2) * ifelse(surf=="upper",1,-1)) %>%
    mutate(xp = x + xdist,
           yp = y + xdist * m)
  # Transform the (xp, yp) and (x, y) coordinates and find the vector normal to the surface
  lvec <- cbind(
    AoATransform(distinct(gradint[c("xp", "yp")]), AoA),
    AoATransform(distinct(gradint[c("x", "y")]), AoA),
    gradint[c("xO", "dist", "surf", "eq")]
  ) %>%
    mutate(delx = xp - x, dely = yp - y)
  return(lvec)
}


#--- Combine x and surf into one lvec ----
NormalLvec <- function(xvec, dist, AoA = 0, 
                       surf = factor(c("upper", "lower"), levels = c("lower", "upper"))) {
  # Combine various x and surfvals efficiently using lapply and single gradint call
  suppressWarnings(
  lvec <- bind_rows(pblapply(
    surf,
    function(surfval) {
      bind_rows(lapply(xvec, NormalPoint, 
                       dist = dist, AoA = AoA, surf = surfval))
      }
    ))
  )
  return(lvec)
}
