# Set working directory when run from RStudio
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("//ad.monash.edu/home/User032/awan39/Documents/GitHub/MAE3401-Viper-Airfoil-Visualisation/Viper-Airfoil-Visualisation-Batch-Runs")

#--- Load MINIMAL source files for use ----
source("Function Load Packages.R")

# Note: Write out which folder(s) have been run and read them to determine which folder/s need to be run
# Alternavtively, you can have a list of which files have been run (more messy?)
# Or, you can automatically move the files out of the folder and then read them (more complicated!)
# AND order the list!
foldername = "Input_Data"
filelist <- list.files(foldername, pattern = "*.dat")

# Load the Airfoil
source("Function Load Data.R")
NACA = 4412
airfoildata <- LoadAirfoil(NACA, a = -0.5, c = 1)

# Load the pblapply wrapper to use (for progress bar)
source("Function pblapply.R")

#--- Initialise the cluster ----
logfile = paste0(format(Sys.time(), "%Y-%m-%dT%H.%M.%S"), ".txt") # logfile

filename = filelist[1]

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

#--- Interpolation on Normals ----
source("Function Airfoil Normals.R")    # For "AirfoilGrads", etc
xvec = AirfoilSamp(seq(a, a+c, by = 0.5), cylinder = TRUE)
dist = NormalSamp(seq(0, 1.5, by = 0.05))
# Find the combined lvec for 
lvec <- rbind(
  bind_rows(pblapply(xvec, NormalPoint, dist = dist, AoA = AoA, surf = "upper")),
  bind_rows(pblapply(xvec, NormalPoint, dist = dist, AoA = AoA, surf = "lower")))
interpval <- InterpProj(omesh, lvec, plotsurf = TRUE)

# SAVE INTERPVAL then delete it!

ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")

#--- Boundary Layer Calculations ----
source("Function Boundary Layers.R")
xvec = AirfoilSamp(seq(a, a+c, by = 0.5), cylinder = FALSE)


bl <- pblapply(xvec,
     function (x) {
       # Search along all normals to get 100% thickness
       dist = NormalSamp(seq(0, 18, length.out = 1e6))
       lvec <- rbind(
         bind_rows(lapply(x, NormalPoint, dist = dist, AoA = AoA, surf = "upper")),
         bind_rows(lapply(x, NormalPoint, dist = dist, AoA = AoA, surf = "lower")))
       blthickness = BLThickness(omesh, lvec)
       # Determine BL values
       dist = seq(0, max(blthickness$dist), length.out = 1e6)
       lvec <- rbind(
         bind_rows(lapply(x, NormalPoint, dist = dist, AoA = AoA, surf = "upper")),
         bind_rows(lapply(x, NormalPoint, dist = dist, AoA = AoA, surf = "lower")))
       blvalues = BLValues(omesh, lvec, blthickness)
       return(lvec)
     }
)
