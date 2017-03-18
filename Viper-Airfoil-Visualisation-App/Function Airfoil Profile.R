#----------------------------
#--- Functions for Airfoil Calculations
#============================

#--- Transform a set of (x,y) based on an AoA (in deg) ----
AoATransform <- function(data, AoA) {
  # Assumes X is column 1 and Y is column 2
  # Store the input data
  odata <- data
  ocolnames <- colnames(data)
  colnames(data[c(1,2)]) <- c("x", "y")
  # Apply the transformation
  data <- select(data, x, y) %>%
    mutate(
      r = sqrt(x^2 + y^2),
      theta = atan(y/x),
      theta = ifelse(is.na(theta),0,theta),
      theta = ifelse(x < 0, theta + pi, theta),
      theta = theta - AoA*pi/180,
      x = r*cos(theta),
      y = r*sin(theta)
    ) %>%
    select(x, y)
  # Re-combine the new data with the old data and restore colnames
  odata[c(1,2)] = data
  colnames(odata) <- ocolnames
  return(odata)
}

#--- Surface Coordinates for a NACA 4 digit airfoil ----
AirfoilCurve <- function(x = 0, out = "all") {
  # Test if x is within range
  on = ifelse(x >= a & x <= a + c, TRUE, stop("x not on airfoil"))
  # Determine the camber line yc
  yc = ifelse(x < p + a, 
    m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2),
    m/(1-p)^2  * (1 - 2*p + 2*p*((x-a)/c) - ((x-a)/c)^2)
  )
  # Determine the gradient of the camber line dycdx
  dycdx = ifelse(x < p + a,
    2*m/p^2 * (p - (x-a)/c),
    2*m/(1-p)^2 * (p - (x-a)/c)
  )
  # Determine the magnitude and direction of the thickness
  theta = atan(dycdx)
  yt = 5*t*(0.2969*sqrt((x-a)/c) - 0.1260*((x-a)/c) - 0.3516*((x-a)/c)^2 +
              0.2843*((x-a)/c)^3 - 0.1036*((x-a)/c)^4)
  # Add the thickness to the camber line
  xU = x - yt*sin(theta)
  yU = yc + yt*cos(theta)
  xL = x + yt*sin(theta)
  yL = yc - yt*cos(theta)
  # Output depending on the Out parameter
  if(out == "all")
    return(data.frame(x, yc, dycdx, theta, yt,  xU, yU,  xL, yL))
  else if(out == "coord")
    return(data.frame(x, xU, yU, xL, yL))
}

#--- Reshape Airfoil Points into (x,y) columns and AoA transform for plotting ----
AirfoilCoord <- function(xmin = a, xmax = c + a, AoA = 0, res = 100) {
  # Cluster points around LE and TE
  xvec = abs(a) * sin(seq(xmin, xmax, length.out = res)*pi/c)
  # Generate coordinates in a tidy format
  coord = AirfoilCurve(xvec, out = "coord") %>%
    rename(xo = x) %>%
    gather(key, value, -xo) %>%
    mutate(coord = substr(key,1,1), surf = substr(key, 2,2)) %>%
    select(-key) %>%
    spread(coord, value) %>%
    mutate(surf = factor(surf, levels = c("U", "L"))) %>%
    arrange(surf, xo*ifelse(surf=="U", 1, -1)) %>%
    select(x, y, surf)
  coord = AoATransform(coord, AoA = AoA)
  return(coord)
}

#--- Determine the gradient of the airfoil at x ----
AirfoilGrads <- function(x, del = c/100000, out = "all") {
  # Determine the points right next to x
  surf = AirfoilCurve(c(x - del, x, x + del), out = "coord") 
  # Estimate the gradients
  dyUdx = with(surf, (yU[3] - yU[1])/(xU[3] - xU[1]))
  dyLdx = with(surf, (yL[3] - yL[1])/(xL[3] - xL[1]))
  # Determine normal and tagential equations for output
  out <- data.frame(
    surf = c("upper", "upper", "lower", "lower"),
    eq = c("tan", "norm", "tan", "norm"),
    xo = x,
    x = with(surf, c(xU[2], xU[2], xL[2], xL[2])),
    y = with(surf, c(yU[2], yU[2], yL[2], yL[2])),
    m = c(dyUdx, -1/dyUdx, dyLdx, -1/dyLdx)) %>%
    mutate(c = -m*x + y)
  return(out)
}

#--- Generate (x,y) lines from the surface ----
AirfoilLineGen <- function(
    x, gradint = AirfoilGrads(x), AoA = 0, surf = "upper", eq = "norm", 
    focusdist = 0.5, len = 51, totaldist = 20) {
  # NOTE: tangent is broken, won't work!!
  # Using the graident-intercepts, determine the range of points
  #   (corrected for gradient direction & upper vs lower surface)
  gradint <- gradint %>%
    filter(surf == get("surf") & eq == get("eq")) %>%
    mutate(focusdist = focusdist, len = len) %>%
    mutate(xfocusdist = sign(m) * focusdist/sqrt(1+m^2) * ifelse(surf=="upper",1,-1)) %>%
    mutate(xfocus = x + xfocusdist, yfocus = y + xfocusdist*m) %>%
    mutate(xmax = x + totaldist, ymax = y + totaldist*m)
  # Generate the points (x,y)
  x = with(gradint, c(seq(x, xfocus, length.out = len), seq(xfocus, xmax, length.out = len)))
  y = with(gradint, c(seq(y, yfocus, length.out = len), seq(yfocus, ymax, length.out = len)))
  lvec = data.frame(x, y)
  # Transform the points based on the AoA supplied
  lvec = AoATransform(distinct(lvec), AoA)
  return(lvec)
}
