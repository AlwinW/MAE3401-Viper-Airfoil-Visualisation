NACA = 4412
# Max camber; Location of m; Thickness
m = (NACA %/% 1000) / 100
p = (NACA %/% 100 %% 10) / 10
t = (NACA %% 100) / 100
# Chord; x-shift
c = 1
a = - 1/2


AirfoilCurve <- function(x, out = "all") {
  if(x >= a & x < p + a) {
    yc = m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2)
    dycdx = 2*m/p^2 * (p - (x-a)/c)
  }
  else if(x > p + a & x <= c + a) {
    yc = m/(1-p)^2  * (1 - 2*p + 2*p*((x-a)/c) - ((x-a)/c)^2)
    dycdx = 2*m/(1-p)^2 * (p - (x-a)/c)
  }
  else
    return(NA)
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

AirfoilGrads <- function(x, step = c/100000, out = "all") {
  xsurf <- suppressWarnings(data.frame(x = rbind(x - step, x, x + step)))
  surf <- xsurf %>%
    rowwise() %>%
    do(data.frame(., AirfoilCurve(.$x))) %>%
    data.frame(.) %>%
    select(x, xU, yU, xL, yL)
  dyUdx = with(surf, (yU[3] - yU[1])/(xU[3] - xU[1]))
  dyLdx = with(surf, (yL[3] - yL[1])/(xL[3] - xL[1]))
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


AirfoilLineGen <- function(x, gradint = AirfoilGrads(x), surf = "upper", eq = "norm", focusdist = 1, len = focusdist*50+1) {
  # NOTE: Tan is broken, won't work!!
  gradint <- gradint %>%
    filter(surf == get("surf") & eq == get("eq")) %>%
    mutate(focusdist = focusdist, len = len) %>%
    mutate(xfocusdist = sign(m)*focusdist/sqrt(1+m^2)*ifelse(surf=="upper",1,-1)) %>%
    mutate(xfocus = x + xfocusdist, yfocus = y + xfocusdist*m) %>%
    mutate(xmax = x + 4*xfocusdist, ymax = y + 4*xfocusdist*m)
  print(gradint)
  xvec = with(gradint, c(seq(x, xfocus, length.out = len), seq(x, xmax, length.out = len)))
  yvec = with(gradint, c(seq(y, yfocus, length.out = len), seq(y, ymax, length.out = len)))
  return(data.frame(xvec, yvec))
}

# ggplot(AirfoilLineGen(-0.4, surf = "upper")) + 
  # geom_point(aes(x = xvec, y = yvec))

