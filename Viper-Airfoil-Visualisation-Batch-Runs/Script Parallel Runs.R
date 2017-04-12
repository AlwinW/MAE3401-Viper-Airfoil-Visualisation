#---------------------------->
#--- Run Post-processing on multi-threaded machine
#--- Alwin Wang MAE3401
#============================>

#--- Set the wd() if necessary ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("//ad.monash.edu/home/User032/awan39/Documents/GitHub/MAE3401-Viper-Airfoil-Visualisation/Viper-Airfoil-Visualisation-Batch-Runs")

#--- Load All the functions -----
source("Script All Functions.R")

#--- Load the Data in the Folder ----
# If using multiple computers set move = TRUE
temp = LoadFolder(foldername = "Input_Data", move = TRUE)
list2env(temp, envir = environment()); rm(temp)

#--- Load the AIrfoil Data ----
# Load the Airfoil
NACA = 4412
airfoildata <- LoadAirfoil(NACA, a = -0.5, c = 1)

#--- Location for Saving ----
saveplot = "Output_Plot"
savedata = "Output_Data"
if (!file.info(saveplot)$isdir) dir.create(saveplot, recursive = TRUE)
if (!file.info(savedata)$isdir) dir.create(savedata, recursive = TRUE)
logfile = paste0(format(Sys.time(), "%Y-%m-%dT%H.%M.%S"), ".txt")

#--- Thread Function ----
source("Script Threads.R")

#--- Initialise the cluster ----
# Start the cluster
cl <- makeCluster(detectCores(), outfile = logfile)
# Export objects into the cluster
clusterExport(cl, c("airfoildata", "foldername", 
                    "logfile", "savedata", "saveplot",
                    "ThreadAll"))

#--- Thread calculation ----
thread <- pblapplycl( # pbapply::pblapply( #
  # List of files to process
  filelist, 
  # Function
  ThreadAll,
  # Functions arguments
  foldername = foldername,
  airfoildata = airfoildata,
  savedata = savedata,
  saveplot = saveplot,
  # Cluster arguments
  cl = cl,
  log = logfile
)

# Stop the cluster
stopCluster(cl)


