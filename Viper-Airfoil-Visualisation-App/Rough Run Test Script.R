#----------------------------
#--- Rough Running Script to Test Functions
#============================

#--- Source Files ----
source("Function Load Data.R")
source("Function Airfoil Profile.R")

#--- Initial Data ----
filedata <- LoadData("test.dat")
NACA = 4412
a = -0.5
c = 1
AoA = 10
airfoildata <- AirfoilData(NACA, a, c)

# Sample Airfoil Plot ----
airfoilcoord <- AirfoilCoord(AoA = AoA, res = 100)

ggplot(airfoilcoord, aes(x = x, y = y, colour = surf)) + 
  geom_path() +
  geom_point() +
  coord_fixed()


normalplot <- ggplot () +
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x=x, y=y))
xvec = seq(a, a+c, by = 0.05)
xvec = -2*a/c^2 * (xvec - a)^2 + a
xvec = xvec[2:(length(xvec)-1)]
for (x in xvec) {
  normalplot = normalplot+
    geom_point(data = AirfoilLineGen(x=x, surf = "upper", AoA = AoA, totaldist = 1), aes(x=x, y=y)) +
    geom_point(data = AirfoilLineGen(x=x, surf = "lower", AoA = AoA, totaldist = 1), aes(x=x, y=y))
}
normalplot + coord_fixed()
