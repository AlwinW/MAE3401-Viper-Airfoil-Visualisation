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

#--- Sample Airfoil Plot ----
airfoilcoord <- AirfoilCoord(AoA = AoA, res = 100)
ggplot(airfoilcoord, aes(x = x, y = y, colour = surf)) + 
  geom_path() +
  geom_point() +
  coord_fixed()

#--- Sample Plot of Normals to the Airfoil
xvec = seq(a, a+c, by = 0.05)
xvec = -2*a/c^3 * (xvec - a)^3 + a
xvec = c(xvec[2:(length(xvec)-1)], (a+c) - sign(a+c)*(a+c)*0.01)
focusdist = 0.2; totaldist = 0.5; len = 21
normalplot <- ggplot () +
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x = x, y = y))
for (x in xvec) {
  for (surf in c("upper", "lower")) {
    normalplot = normalplot +
      geom_point(data = AirfoilLineGen(x, AoA, surf, eq = "norm", focusdist, totaldist, len),
      aes(x = x, y = y))
  }
}
normalplot + coord_fixed()
