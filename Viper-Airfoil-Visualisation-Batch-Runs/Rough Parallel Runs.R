# Set working directory when run from RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
cl <- makeCluster(detectCores(), outfile = logfile)               # start the cluster
clusterExport(cl, c("airfoildata", "foldername"))                 # add airfoildata to the cluster threads

#--- Thread calculation ----
thread <- pblapplycl( #pbapply::pblapply( #
  filelist, 
  function(filename) {                      # filename = filelist[1] for debugging
    source("Function Load Packages.R")      # For required packages
    #--- Load ALL the information from the file contents and name ----
    source("Function Load Data.R")          # For fn "Load File"
    # Load the filedata and unlist it
    filedata <- LoadFile(filename, foldername)
    list2env(filedata, envir = environment()); rm(filedata)       # N.B: local so must be passed as fn input
    
    print(paste(ID, "File Data Loaded"))
        
    #--- Run Airfoil Calculations ----
    source("Function Airfoil Profile.R")      # For fn "AirfoilCoord", etc
    list2env(airfoildata, envir = .GlobalEnv)                     # N.B: global so all fn can find it
    airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)
    
    print(paste(ID, "Airfoil Coordinaes Calculated"))
    
    #--- Interpolation on the airfoil----
    source("Function Interpolations.R")       # For fn "InterpPoint", etc
    airfoilsurfmesh <- InterpPoint(omesh, airfoilcoord, varnames = c("P", "vort_xy_plane"))
    
    print(paste(ID, "Airfoil Surface Interpolation Calculated"))
    
  },
  cl = cl
)

stopCluster(cl)





