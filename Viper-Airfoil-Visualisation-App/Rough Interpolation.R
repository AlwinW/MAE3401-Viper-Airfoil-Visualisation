testraw <- read.table("test.dat", sep = " ",
                      skip = 12,
                      nrow = 9888)

test <- as.data.frame(matrix(c(t(testraw[2:6])), ncol=6))

colnames(test) <- c("X", "Y", "U", "V", "P", "vort_xy_plane")

# testline <- AirfoilLineGen(x = -0.3, focusdist = 1, factor = 15, AoA = 10)

# asdf <- as.data.frame(interpp(x = test$X, y = test$Y, z = test$U,
#         xo = testline$xvec, yo = testline$yvec))
# 
# ggplot(asdf) + geom_point(aes(x = x, y = y, colour = z))
VelocityProfile <- function(omesh, x, focusdist = 0.5, factor = 40, surf = "upper", AoA = 0) {
  interpline <- AirfoilLineGen(x, focusdist = focusdist, factor = factor, surf = surf, AoA = AoA)
  lmeshU <- suppressWarnings(
    as.data.frame(interpp(x = omesh$X, y = omesh$Y, z = omesh$U,
                          xo = interpline$xvec, yo = interpline$yvec)))
  lmeshV <- suppressWarnings(
    as.data.frame(interpp(x = omesh$X, y = omesh$Y, z = omesh$V,
                          xo = interpline$xvec, yo = interpline$yvec)))
  lmeshP <- suppressWarnings(
    as.data.frame(interpp(x = omesh$X, y = omesh$Y, z = omesh$P,
                          xo = interpline$xvec, yo = interpline$yvec)))
  lmeshVortXY <- suppressWarnings(
    as.data.frame(interpp(x = omesh$X, y = omesh$Y, z = omesh$vort_xy_plane,
                          xo = interpline$xvec, yo = interpline$yvec)))
  # lmesh <- left_join(lmeshU, lmeshV, c("X", "Y")) #Adds extra rows
  lmesh = cbind(lmeshU, lmeshV[3], lmeshP[3], lmeshVortXY[3])
  colnames(lmesh) <- c("X", "Y", "U", "V", "P", "vort_xy_plane")
  
  grad = tail(interpline,1) - head(interpline, 1)
  theta = atan(as.numeric(grad[2]/grad[1]))
  Um = abs(sin(theta))
  xo = interpline[1,1]
  yo = interpline[1,2]
  
  lmesh <- lmesh  %>%
    mutate(dist = sqrt((X-xo)^2 + (Y-yo)^2),
           Um = Um) %>%
    mutate(Udash = sign(theta) * (sin(theta) * U - cos(theta) * V),
           Vdash = ifelse(surf=="upper", 1, -1) * (cos(theta) * U + sin(theta) * V)) %>% ## NEEDS TO BE FIXED
    mutate(integrand = 1 - Udash/Um)
  return(list(theta, lmesh))
}


#--------------------------------------------
omesh <- test
x = -0.4
surf = "upper"
AoA = 10

# Plot mesh around a point
xm = x
ym = 0
r = 0.4
ggplot() + 
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x=xp, y=yp), colour = "red") + #airfoil
  geom_point(data = AirfoilLineGen(x=x, surf = surf, AoA = AoA), aes(x=xvec, y=yvec), colour = "red") + #line
  geom_point(data = omesh, aes(x = X, y = Y, colour = ifelse(U>0, 1, -1))) + #mesh
  xlim(xm - r, xm + r) + 
  ylim(ym - r, ym + r) + 
  coord_fixed()
ggplot() + 
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x=xp, y=yp), colour = "red") + #airfoil
  geom_point(data = AirfoilLineGen(x=x, surf = surf, AoA = AoA), aes(x=xvec, y=yvec), colour = "red") + #line
  geom_point(data = omesh, aes(x = X, y = Y, colour = ifelse(V>0, 1, -1))) + #mesh
  xlim(xm - r, xm + r) + 
  ylim(ym - r, ym + r) + 
  coord_fixed()

listout <- VelocityProfile(omesh = omesh, x = x, surf = surf, AoA = AoA)
theta = listout[[1]][1]
lmesh = listout[[2]]
