#----------------------------
#--- Boundary Layer
#============================

#--- Source Files ----
source("Helper Install Packages.R")
source("Function Load Data.R")
source("Function Airfoil Profile.R")
source("Function Interpolation.R")

#--- Initial Data ----
filedata <- LoadData("test-40.dat") ## CHANGE THIS TO YOUR FILE
NACA = 4412
a = -0.5
c = 1
AoA = -400 ## CHANGE THIS TO YOUR AoA
Re = 50
airfoildata <- AirfoilData(NACA, a, c)

#--- Sample Airfoil Plot ----
airfoilcoord <- AirfoilCoord(AoA = AoA, res = 100)

# Along a perpendicular lines ----
omesh = filedata
xvec = AirfoilSamp(seq(a, a+c, by = 0.015), cylinder = TRUE)

# Quicker run
# xvec = c(head(xvec, 30), tail(xvec, 1))

# Test run
BLtest <- InterpPerpLine(omesh, 0, AoA = AoA, surf = "lower")
ggplot(BLtest) + 
  geom_path(aes(x = Udash, y = dist, colour = "U'")) + 
  geom_path(aes(x = percentUm/100, y = dist, colour = "U'/Um")) +
  geom_path(aes(x = Um, y = dist)) +
  geom_path(aes(x = 1/Um, y = dist)) +
  ylim(c(0,1))
print(max(BLtest$dist[which.max(BLtest$Udash)]))

# ggplot(BLtest) +
#   geom_path(aes(x = dist, y = 1 - Udash/Um, colour = "1-U'/Um"))
# ggplot(BLtest) +
#   geom_path(aes(x = dist, y = 1 - Udash, colour = "1-U'"))

surf = "lower"
blggplotoverlay <- ggplot()
blggplotoverlayall <- ggplot()
for (x in xvec) {
  interp <- InterpPerpLine(omesh, x, AoA = AoA, surf = surf)
  # Find the row for max
  blU <- max(interp$Udash[c(rep(FALSE, 4),!is.na(interp$Udash[5:nrow(interp)]))])
  bl <- min(which(interp$Udash[5:nrow(interp)] >= blU - abs(blU)*0.02))
  blggplotoverlay <-  blggplotoverlay +
    geom_point(data = interp[1:bl,], aes(x = Udash/Um, y = dist, colour = x[1]))
  blggplotoverlayall <-  blggplotoverlayall +
    geom_point(data = interp, aes(x = Udash/Um, y = dist, colour = x[1]))
  print(bl)
}
blggplotoverlay
blggplotoverlayall



BLValues <- function(omesh, xvec, surf = "upper") {
  # Apply a function across each x in xvec
  interpval <- pblapply(xvec, function(x) {
    # Find the interpolations
    interp <- InterpPerpLine(omesh, x, AoA = AoA, surf = surf)
    # Find the row for max 
    blU <- max(interp$Udash[c(rep(FALSE, 4),!is.na(interp$Udash[5:nrow(interp)]))])
    bl <- min(which(interp$Udash[5:nrow(interp)] >= blU - abs(blU)*0.02))
    # ggplot(interp[1:bl,]) + geom_point(aes(x = Udash/Um, y = dist)) + ylim(0,NA)
    # Find the thicknesses
    # This assumes an integral that extends all the way to infinity
    # Alternatively, Ur = U / U(BL) and sum from 0 to dist(bl)
    thickness <- interp[1:bl,] %>%
      mutate(Ur = Udash / interp$Udash[bl]) %>%
      mutate(dispth = 1 - Ur,
             mometh = Ur*(1 - Ur),
             kineth = Ur*(1 - (Ur)^2)) %>%
      select(dist, dispth, mometh, kineth) %>%
      mutate(dispth = 1/2 * (dispth + lag(dispth,1)) * (dist - lag(dist,1)),
             mometh = 1/2 * (mometh + lag(mometh,1)) * (dist - lag(dist,1)),
             kineth = 1/2 * (kineth + lag(kineth,1)) * (dist - lag(dist,1)))
    thickness <- thickness[2:nrow(thickness),]
    # Return in a list in a list
    list(
      # summary
      summary = data.frame(
        xO = interp$x[1], surf = surf, AoA, Re, 
        distbl = interp$dist[bl], xbl = interp$x[bl], ybl = interp$y[bl], ubl = interp$Udash[bl],
        dispth = sum(thickness$dispth), mometh = sum(thickness$mometh), kineth = sum(thickness$kineth)),
      # interpolation
      interp = interp
    )
  })
  return(interpval)
}

BLLong <- function(interpval, out = "BL") {
  interpvalLs = data.frame()
  interpvalLi = data.frame()
  for (i in (1:length(interpval))) {
    interpvalLs = rbind(
      interpvalLs,
      interpval[[i]]$summary
    )
    interpvalLi = rbind(
      interpvalLi,
      interpval[[i]]$interp
    )
  }

  return(list(summary = interpvalLs, interp = interpvalLi))
}


asdfU <- BLValues(omesh, xvec, surf = "upper")
asdfUL <- BLLong(asdfU)
asdfL <- BLValues(omesh, xvec, surf = "lower")
asdfLL <- BLLong(asdfL)

asdfLong = list(
  summary = rbind(asdfUL$summary, asdfLL$summary),
  interp = rbind(asdfUL$interp, asdfLL$interp)
)
# Plot of U' i.e. perp to the normal from the airfoil
ggplot () +
  geom_point(data = asdfLong$interp, aes(x = x, y = y, colour = Udash)) +
  geom_point(data = filter(asdfLong$interp, Udash < -1.2), aes(x = x, y = y, colour = Udash), colour = "#BE2828") +
  geom_point(data = filter(asdfLong$interp, Udash > 1.2), aes(x = x, y = y, colour = Udash), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("U'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
  coord_fixed()

# BL PLOT
asdfbl <- filter(asdfLong$summary, dispth > 0)
xmin = min(c(asdfbl$xbl, airfoilcoord$x))
xmax = max(c(asdfbl$xbl, airfoilcoord$x))
ymin = min(c(asdfbl$ybl, airfoilcoord$y))
ymax = max(c(asdfbl$ybl, airfoilcoord$y))
ggplot() + 
  geom_path(data = asdfbl, aes(x = xbl, y = ybl), size = 2) +
  geom_point(data = asdfLong$interp, aes(x = x, y = y, colour = Udash)) +
  geom_point(data = filter(asdfLong$interp, Udash < -1.2), aes(x = x, y = y, colour = Udash), colour = "#BE2828") +
  geom_point(data = filter(asdfLong$interp, Udash > 1.2), aes(x = x, y = y, colour = Udash), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(xmin, xmax) +
  ylim(ymin, ymax) +
  scale_colour_gradientn("U'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
  coord_fixed()