#----------------------------
#--- Boundary Layer
#============================

#--- Source Files ----
source("Helper Install Packages.R")
source("Function Load Data.R")
source("Function Airfoil Profile.R")
source("Function Interpolation.R")

#--- Initial Data ----
filedata <- LoadData("test.dat") ## CHANGE THIS TO YOUR FILE
NACA = 4412
a = -0.5
c = 1
AoA = 10 ## CHANGE THIS TO YOUR AoA
Re = 50
airfoildata <- AirfoilData(NACA, a, c)

#--- Sample Airfoil Plot ----
airfoilcoord <- AirfoilCoord(AoA = AoA, res = 100)

# Along a perpendicular lines ----
omesh = filedata
xvec = AirfoilSamp(seq(a, a+c, by = 0.1), cylinder = TRUE)

# Quicker run
xvec = c(head(xvec, 3), tail(xvec, 1))

# Test run
BLtest <- InterpPerpLine(omesh, 0, AoA = AoA, surf = "upper")
ggplot(BLtest) + 
  geom_path(aes(x = Udash, y = dist, colour = "U'")) + 
  geom_path(aes(x = percentUm/100, y = dist, colour = "U'/Um")) +
  geom_path(aes(x = Um, y = dist)) +
  geom_path(aes(x = 1/Um, y = dist)) +
  ylim(c(0,1))
print(max(BLtest$dist[which.max(BLtest$Udash)]))

ggplot(BLtest) + 
  geom_path(aes(x = dist, y = 1 - Udash/Um, colour = "1-U'/Um"))
ggplot(BLtest) + 
  geom_path(aes(x = dist, y = 1 - Udash, colour = "1-U'"))


BLValues <- function(omesh, xvec, surf = "upper") {
  # Apply a function across each x in xvec
  interpval <- pblapply(xvec, function(x) {
    # Find the interpolations
    interp <- InterpPerpLine(omesh, x, AoA = AoA, surf = surf)
    # Find the row for max 
    bl <- which.max(interp$Udash)
    # Find the thicknesses
    thickness <- interp %>%
      mutate(dispth = 1 - Udash/Um,
             mometh = Udash/Um*(1 - Udash/Um),
             kineth = Udash/Um*(1 - (Udash/Um)^2)) %>%
      select(dist, dispth, mometh, kineth) %>%
      mutate(dispth = 1/2 * (dispth + lag(dispth,1)) * (dist - lag(dist,1)),
             mometh = 1/2 * (mometh + lag(mometh,1)) * (dist - lag(dist,1)),
             kineth = 1/2 * (kineth + lag(kineth,1)) * (dist - lag(dist,1)))
    # Return in a list in a list
    list(
      summary = data.frame(
        xO = x, surf = surf, AoA, Re, 
        distbl = interp$dist[bl], xbl = interp$x[bl], ybl = interp$y[bl],
        dispth = sum(thickness$dispth), mometh = sum(thickness$mometh), kineth = sum(thickness$kineth)),
      interp = interp
    )
  })
  return(interpval)
}

BLLong <- function(interpval, out = "BL") {
  for (i in (1:length(interpval))) {
    interpval
  }
}

InterpTest1U <- pblapply(xvec, function(x) {
  list(
    data.frame(xO = x, surf = "upper", AoA, Re),
    InterpPerpLine(omesh, x, AoA = AoA, surf = "upper")
  )
})
InterpTest1LongU = data.frame()
for (i in (1:length(InterpTest1U))) {
  InterpTest1LongU = rbind(InterpTest1LongU,
                           cbind(InterpTest1U[[i]][[1]], InterpTest1U[[i]][[2]]))
}

InterpTest1L <- pblapply(xvec, function(x) {
  list(
    data.frame(xO = x, surf = "lower", AoA, Re),
    InterpPerpLine(omesh, x, AoA = AoA, surf = "lower")
  )
})
InterpTest1LongL = data.frame()
for (i in (1:length(InterpTest1L))) {
  InterpTest1LongL = rbind(InterpTest1LongL,
                           cbind(InterpTest1L[[i]][[1]], InterpTest1L[[i]][[2]]))
}

InterpTest1Long = rbind(InterpTest1LongU, InterpTest1LongL)
save(InterpTest1Long, file = "InterpTest1Long.RData")
load(file = "InterpTest1Long.RData")

# Plot of U' i.e. perp to the normal from the airfoil
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = Udash)) +
  geom_point(data = filter(InterpTest1Long, Udash < -1.2), aes(x = x, y = y, colour = Udash), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, Udash > 1.2), aes(x = x, y = y, colour = Udash), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("U'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
  coord_fixed()