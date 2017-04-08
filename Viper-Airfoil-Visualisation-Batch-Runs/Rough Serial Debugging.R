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
# Find the combined lvec for interpolation
lvec <- NormalLvec(xvec, dist, AoA)
interpval <- InterpProj(omesh, lvec, plotsurf = TRUE)

# SAVE INTERPVAL then delete it!

ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")

#--- Boundary Layer Calculations ----
source("Function Boundary Layers.R")    # For "BLCalcs", etc
xvec = AirfoilSamp(seq(a, a+c, by = 0.05), polyn = 5, cylinder = TRUE)
blvals = BLCalcs(omesh, xvec, AoA, Re)
bltheory = BLTheory(omesh, xvec, AoA, Re)
blplot = bind_rows(blvals, bltheory)


#--- Velocity Profile Calculations ----
# Domain for plotting
sep = 0.1
xvec = c(-0.497, seq(a + sep, a + c - sep, by = sep), 0.499)
distmax = max(blvals$dist[blvals$xO == max(blvals$xO)]) * 1.2
# Results (interpolation)
lvec <- NormalLvec(xvec, NormalSamp(seq(0, distmax, length.out = 50)), AoA)
velprofile <- InterpProj(omesh, lvec, linear = TRUE)
vpblvals = BLCalcs(omesh, xvec, AoA, Re)
vpbl = filter(vpblvals, method == "max") %>%
  select(xO, surf, thickness)

velprofile <- right_join(velprofile, vpbl, by = c("xO", "surf")) %>%
  mutate(bl = (dist <= thickness))

# Blasius Soln
blasius <- data.frame(
  eta = seq(0, 8, by = 0.5),
  uU = c(0.000, 0.1659, 0.3298, 0.4868, 0.6298, 0.7513, 0.8461, 0.9131, 0.9555,
         0.9795, 0.9916, 0.9969, 0.9990, 0.9997, 0.9999, 1.000, 1.000))
lvec <- lvec %>%
  mutate(eta = sqrt(Re * (x - a)) * dist/(x - a))
vptheory <- as.data.frame(spline(x = blasius$eta, y = blasius$uU, xout = lvec$eta))
vptheory <- data.frame(lvec, UUmblasius = vptheory$y) %>%
  mutate(UUmblasius = ifelse(eta >= 8, 1, UUmblasius))


ggplot(data = velprofile, 
       aes(x = ifelse(surf == "upper", 1, -1) * dist,
           ymin = x, ymax = x + UUmdash * sep / 1.5,
           group = interaction(surf, xO))) +
  geom_ribbon(aes(alpha = "out")) +
  geom_ribbon(data = filter(velprofile, bl == TRUE), aes(alpha = "in")) +
  coord_flip(xlim = c(-distmax, distmax), ylim = c(-0.6, 0.7)) +
  theme(aspect.ratio = (2*distmax)/(0.6 + 0.7))

ggplot(data = velprofile, aes(group = interaction(surf, xO))) +
  # geom_path(aes(x = ifelse(surf == "upper", 1, -1) * dist, y = x + UUmdash * 0.05)) +
  # geom_point(aes(x = ifelse(surf == "upper", 1, -1) * dist, y = x + UUmdash * 0.05))  +
  geom_ribbon(aes(x = ifelse(surf == "upper", 1, -1) * dist, ymin = x, ymax = x + UUmdash * sep/1.5), alpha = 0.2) +
  geom_path(data = vptheory, aes(x = ifelse(surf == "upper", 1, -1) * dist, y = x + UUmblasius * sep/1.5)) +
  # geom_path(data = blvals, aes(x = ifelse(surf == "upper", 1, -1) * dist, y = x, group = interaction(method, surf))) +
  ylim(-0.6, 0.7) +
  xlim(-distmax, distmax) + 
  coord_flip()

# GGPLOT: x = ifelse(surf == "upper", 1, -1) * dist
# GGPLOT: y = x + UUmdash * sep/20
# GGPLOT: coord_flip()





