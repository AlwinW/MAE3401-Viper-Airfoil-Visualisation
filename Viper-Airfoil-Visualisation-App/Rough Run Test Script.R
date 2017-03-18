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

ggplot() +
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x=xp, y=yp)) +
  geom_point(data = AirfoilLineGen(x=-0.3, surf = "upper", AoA = AoA), aes(x=xvec, y=yvec)) +
  geom_point(data = AirfoilLineGen(x=-0, surf = "upper", AoA = AoA), aes(x=xvec, y=yvec)) +
  geom_point(data = AirfoilLineGen(x=0.3, surf = "upper", AoA = AoA), aes(x=xvec, y=yvec)) +
  geom_point(data = AirfoilLineGen(x=-0.3, surf = "lower", AoA = AoA), aes(x=xvec, y=yvec)) +
  geom_point(data = AirfoilLineGen(x=-0, surf = "lower", AoA = AoA), aes(x=xvec, y=yvec)) +
  geom_point(data = AirfoilLineGen(x=0.3, surf = "lower", AoA = AoA), aes(x=xvec, y=yvec)) +
  coord_fixed()