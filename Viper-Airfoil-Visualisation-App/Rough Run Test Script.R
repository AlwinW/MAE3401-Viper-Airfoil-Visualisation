#----------------------------
#--- Rough Running Script to Test Functions
#============================

source("Function Load Data.R")
source("Function Airfoil Profile.R")

filedata <- LoadData("test.dat")

NACA = 4412
a = -0.5
c = 1

airfoildata <- AirfoilData(NACA, a, c)

airfoilcoord <- AirfoilCoord(res = 100)

ggplot(airfoilcoord, aes(x = x, y = y, colour = surf)) + 
  geom_path() +
  geom_point()
