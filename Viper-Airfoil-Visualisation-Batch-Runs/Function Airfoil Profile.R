#---------------------------->
#--- Functions for Airfoil Calculations
#--- Alwin Wang MAE3401
#============================>

#--- Transform (x,y) based on AoA ----
# Takes the original data and assumes col 1 is x and col 2 is y
# x and y are transformed then joined back to the other original columns
AoATransform <- function(odata, AoA) {
  # Store the input data and column names
  ocolnames <- colnames(odata)
  data <- odata[c(1,2)]
  colnames(data) <- c("x", "y")
  # Apply the transformation
  data <- data %>%
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


#--- Camber and Thickness Curves ----
# Takes a given x and maps it to (x, y) on the airfoil.
# Note that these x values may be different!!
AirfoilCurve <- function(x = 0, out = "all") {
  # Test if x is within range
  on = ifelse(x >= a & x <= (a + c), 1, 0) # allows for root-finding
  # Determine the camber line yc
  yc = ifelse(x < p * c + a, 
              m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2),
              m/(1-p)^2  * (1 - 2*p + 2*p*((x-a)/c) - ((x-a)/c)^2)
  )
  # Determine the gradient of the camber line dycdx
  dycdx = ifelse(x < p * c + a,
                 2*m/p^2 * (p - (x-a)/c),
                 2*m/(1-p)^2 * (p - (x-a)/c)
  )
  # Determine the magnitude and direction of the thickness
  theta = atan(dycdx)
  yt = 5*t*(0.2969*sqrt(abs((x-a)/c)) - 0.1260*((x-a)/c) - 0.3516*((x-a)/c)^2 +
              0.2843*((x-a)/c)^3 - 0.1036*((x-a)/c)^4)
  # Add the thickness to the camber line
  xU = x - yt*sin(theta)
  yU = yc + yt*cos(theta)
  xL = x + yt*sin(theta)
  yL = yc - yt*cos(theta)
  # Output depending on the out parameter
  if(out == "all")
    summary = data.frame(x, yc, dycdx, theta, yt,  xU, yU,  xL, yL)
  else if(out == "coord")
    summary = data.frame(x, xU, yU, xL, yL)
  else if (out == "upper")
    summary = data.frame(x = xU, y = yU)
  else if (out == "lower")
    summary = data.frame(x = xL, y = yL)
  # Return the output
  return(summary * on)
}


#--- (x, y) Airfoil Coord Given xvec ----
# Determines the airfoil coordinates on the upper and lower surfaces
# for the domain xmin to xmax
# Direction: Lower TE > Lower LE > Upper LE > Upper TE
AirfoilCoord <- function(xmin = a, xmax = c + a, AoA = 0, res = 100) {
  # Cluster points around LE and TE
  xvec = abs(a) * sin(seq(xmin, xmax, length.out = res)*pi/c)
  # Generate coordinates in a tidy format
  coord <-  AirfoilCurve(xvec, out = "coord") %>%
    rename(xO = x) %>%
    gather(key, value, -xO) %>%
    mutate(coord = substr(key,1,1), surf = substr(key, 2,2)) %>%
    select(-key) %>%
    spread(coord, value) %>%
    mutate(surf = factor(surf, levels = c("L", "U"))) %>%
    arrange(surf, xO * ifelse(surf == "U", 1, -1)) %>%
    mutate(surf = as.character(surf)) %>%
    select(x, y, surf)
  coord = AoATransform(coord, AoA = AoA)
  return(coord)
}


#--- Find xO for req xL or xU ----
# Using root finding techniques, the value of x for xO = xL, xU is found
Airfoilx <- function(xO,  surf = "upper", tol = 1e-9, out = "x") {
  # Use the rooting finding in {stats} to find the root
  rootfind <- uniroot(function(x) AirfoilCurve(x, out = surf)$x - xO,
                      lower = a, upper = a + c,
                      tol = tol)
  # Ouput depending on out parameter
  if(out == "x")
    return(rootfind$root)
  else if(out ==  "all")
    return(rootfind)
  else if(out == "str")
    return(str(rootfind))
}