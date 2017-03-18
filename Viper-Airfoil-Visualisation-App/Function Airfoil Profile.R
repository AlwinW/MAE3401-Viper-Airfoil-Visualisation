#----------------------------
#--- Functions for Airfoil Calculations
#============================

## Surface Coordinates for a NACA 4 digit airfoil
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

## Reshape Airfoil Points into (x,y) columns for plotting
AirfoilCoord <- function(xmin = a, xmax = c + a, res = 100) {
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
  return(coord)
}

