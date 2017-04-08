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

#--- Location for Saving ----
savepath = ""
logfile = paste0(format(Sys.time(), "%Y-%m-%dT%H.%M.%S"), ".txt") # logfile

#--- Initialise the cluster ----
cl <- makeCluster(detectCores(), outfile = logfile)               # start the cluster
clusterExport(cl, c("airfoildata", "foldername", "logfile"))      # add airfoildata to the cluster threads

#--- Thread calculation ----
thread <- pblapplycl( # pbapply::pblapply( #
  filelist, 
  function(filename) {
    source("Thread All Functions.R")
    outcome = TreadAll(filename, foldername, airfoildata)
    return(outcome)
  },
  cl = cl,
  log = logfile
)

stopCluster(cl)

