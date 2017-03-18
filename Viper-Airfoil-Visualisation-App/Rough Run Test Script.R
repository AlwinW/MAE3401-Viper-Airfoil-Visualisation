#----------------------------
#--- Rough Running Script to Test Functions
#============================

source("Function Load Data.R")

filedata <- LoadData("test.dat")

NACA = 4412
a = -0.5
c = 1

rubbish <- AirfoilData(NACA, a, c)
