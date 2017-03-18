#----------------------------
#--- Rough functions for Tangents and Normals
#============================

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

AirfoilCoord <- function(xmin = a, xmax = c + a, res = 100) {
  coord <- data.frame(x = seq(xmin, xmax, length.out = res)) %>%
    rowwise() %>%
    do(data.frame(., AirfoilCurve(.$x))) %>%
    ungroup() %>%
    data.frame(.) %>%
    select(x,xU, yU, xL, yL) %>%
    gather(key, value, -x) %>%
    mutate(coord = paste(substr(key,1,1),"p",sep=""), surf = substr(key, 2,2)) %>%
    select(-key) %>%
    spread(coord, value) %>%
    mutate(surf = factor(surf, levels = c("U", "L"))) %>%
    arrange(surf, x*ifelse(surf=="U", 1, -1)) %>%
    select(xp, yp, surf)
  return(coord)
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

AoATransform <- function(data, AoA) {
  #Assumes X is column 1 an dY is column 2
  ocolnames <- colnames(data[c(1,2)])
  colnames(data) <- c("x","y")
  data <- data %>%
    select(x, y) %>%
    mutate(
      r = sqrt(x^2 + y^2),
      theta = atan(y/x),
      theta = ifelse(is.na(theta),0,theta),
      theta = ifelse(x<0, theta + pi, theta),
      theta = theta - AoA*pi/180,
      x = r*cos(theta),
      y = r*sin(theta)
    ) %>%
    select(x, y)
  colnames(data) <- ocolnames
  return(data)
  }

AirfoilLineGen <- function(x, gradint = AirfoilGrads(x), AoA = 0,
                           surf = "upper", eq = "norm", focusdist = 0.5, len = 51, factor = 5) {
  # NOTE: Tan is broken, won't work!!
  gradint <- gradint %>%
    filter(surf == get("surf") & eq == get("eq")) %>%
    mutate(focusdist = focusdist, len = len) %>%
    mutate(xfocusdist = sign(m)*focusdist/sqrt(1+m^2)*ifelse(surf=="upper",1,-1)) %>%
    mutate(xfocus = x + xfocusdist, yfocus = y + xfocusdist*m) %>%
    mutate(xmax = x + factor*xfocusdist, ymax = y + factor*xfocusdist*m)
  # print(gradint)
  xvec = with(gradint, c(seq(x, xfocus, length.out = len), seq(xfocus, xmax, length.out = len)))
  yvec = with(gradint, c(seq(y, yfocus, length.out = len), seq(yfocus, ymax, length.out = len)))
  lvec = data.frame(xvec, yvec)
  lvec = AoATransform(lvec, AoA)
  return(distinct(lvec))
}

# ggplot(AirfoilLineGen(-0.4, surf = "upper")) + 
  # geom_point(aes(x = xvec, y = yvec))

# AoAtest = 10
# 
# ggplot() + 
#   geom_path(data = AoATransform(AirfoilCoord(), AoA = AoAtest), aes(x=xp, y=yp)) +
#   geom_point(data = AirfoilLineGen(x=-0.3, surf = "upper", AoA = AoAtest), aes(x=xvec, y=yvec)) + 
#   geom_point(data = AirfoilLineGen(x=-0, surf = "upper", AoA = AoAtest), aes(x=xvec, y=yvec)) + 
#   geom_point(data = AirfoilLineGen(x=0.3, surf = "upper", AoA = AoAtest), aes(x=xvec, y=yvec)) + 
#   geom_point(data = AirfoilLineGen(x=-0.3, surf = "lower", AoA = AoAtest), aes(x=xvec, y=yvec)) +
#   geom_point(data = AirfoilLineGen(x=-0, surf = "lower", AoA = AoAtest), aes(x=xvec, y=yvec)) + 
#   geom_point(data = AirfoilLineGen(x=0.3, surf = "lower", AoA = AoAtest), aes(x=xvec, y=yvec)) + 
#   coord_fixed()

