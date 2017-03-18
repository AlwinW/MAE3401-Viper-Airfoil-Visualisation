#----------------------------
#--- Functions for Airfoil Calculations
#============================

## Surface Coordinates for a NACA 4 digit airfoil
AirfoilCurve <- function(x = 0, out = "all") {
  # NACA Camber Curve N.B. rubbish just holds a TRUE/FALSE value
  rubbish = 
    # LE to Max Camber
    ifelse(x >= a & x < p + a, {
      yc = m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2)
      dycdx = 2*m/p^2 * (p - (x-a)/c)
    },
    # Max Camber to TE
    ifelse(x > p + a & x <= c + a, {
      yc = m/(1-p)^2  * (1 - 2*p + 2*p*((x-a)/c) - ((x-a)/c)^2)
      dycdx = 2*m/(1-p)^2 * (p - (x-a)/c)
    },
    # Undefined x
    stop("x not on airfoil")
  ))
  # Add on the thichness to camber
  theta = atan(dycdx)
  yt = 5*t*(0.2969*sqrt((x-a)/c) - 0.1260*((x-a)/c) - 0.3516*((x-a)/c)^2 +
              0.2843*((x-a)/c)^3 - 0.1036*((x-a)/c)^4)
  xU = x - yt*sin(theta)
  yU = yc + yt*cos(theta)
  xL = x + yt*sin(theta)
  yL = yc - yt*cos(theta)
  if(out == "all")
    return(data.frame(x, yc, dycdx, theta, yt,  xU, yU,  xL, yL))
}

