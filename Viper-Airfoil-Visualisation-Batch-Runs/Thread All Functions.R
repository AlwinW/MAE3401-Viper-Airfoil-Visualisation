#---------------------------->
#--- Thread All Functions
#--- Alwin Wang MAE3401
#============================>

#--- Load Packages ----
source("Function Load Packages.R")      # For required packages

#--- Print Progress ----
source("Function pblapply.R")           # For PrintThreadProgress
threadname <- ThreadName()

#--- Load ALL file information  ----
source("Function Load Data.R")          # For fn "Load File"
# Load the filedata and unlist it
filedata <- LoadFile(filename, foldername)
list2env(filedata, envir = environment()); rm(filedata)       # N.B: local so must be passed as fn input

ThreadProgress(threadname, Re, AoA, "File Data Loaded")

#--- Run Airfoil Calculations ----
source("Function Airfoil Profile.R")    # For fn "AirfoilCoord", etc
list2env(airfoildata, envir = .GlobalEnv)                     # N.B: global so all fn can find it
airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)

ThreadProgress(threadname, Re, AoA, "Airfoil Coordinates Calculated")

#--- Interpolation on the airfoil----
source("Function Interpolations.R")     # For fn "InterpPoint", etc
airfoilsurfmesh <- InterpPoint(omesh, airfoilcoord, varnames = c("P", "vort_xy_plane"))

ThreadProgress(threadname, Re, AoA, "Airfoil Surface Interpolation Calculated")

# Plot cp AND vorticity

#--- Interpolation on Normals ----
source("Function Airfoil Normals.R")    # For "AirfoilGrads", etc
xvec = AirfoilSamp(seq(a, a+c, by = 0.5), cylinder = TRUE)
dist = NormalSamp(seq(0, 1.5, by = 0.05))
# Find the combined lvec for interpolation
lvec <- NormalLvec(xvec, dist, AoA)
interpval <- InterpProj(omesh, lvec, plotsurf = TRUE)

# SAVE INTERPVAL then delete it!

ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")

#--- Boundary Layer Calculations ----
source("Function Boundary Layers.R")    # For "BLCalcs", etc
xvec = AirfoilSamp(seq(a, a+c, by = 0.5), polyn = 5, cylinder = TRUE)
blvals = BLCalcs(omesh, xvec, AoA, Re)
bltheory = BLTheory(omesh, xvec, AoA, Re)
blplot = bind_rows(blvals, bltheory)

ThreadProgress(threadname, Re, AoA, "Boundary Layers Calculated")

#--- Velocity Profile Calculations ----
sep = 0.25
vec = VelProfileLvec(omesh, sep, blvals, AoA)
xvec = vec$xvec; lvec = vec$lvec; rm(vec)
velprofile = VelProfile(blvals, xvec, lvec, omesh, AoA, Re)
vptheory = Blasius(lvec, Re)

space.usage <- sort(sapply(ls(), function(x) format(object.size(get(x)), units = "auto")))
status = paste(ID, "Completed")

ThreadProgress(threadname, Re, AoA, "Completed")