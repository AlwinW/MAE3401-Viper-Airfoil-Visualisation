# Set working directory when run from RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Function Install Packages.R")
LoadPackages()

source("Function Load Data.R")
NACA = 4412
airfoildata <- AirfoilData(NACA, -0.5, 1)
folderdata <- LoadFolder()

source("Function pblapply.R")
source("Thread Function Calls.R")

# --- Trial Run
parallelCluster <- 
  makeCluster(detectCores(), outfile = paste0(format(Sys.time(), "%Y-%m-%dT%H.%M%.S%z"), ".txt"))
clusterExport(parallelCluster, c("airfoildata", "ThreadLoopTest"))

a1 <- pbapply::pblapply(
  folderdata,
  function(threaddata) {
   ThreadLoopTest(threaddata)
    # print(filedata$ID)
    # lapply(filedata, function(x) print(class(x)))
  }, cl = parallelCluster
)
stopCluster(parallelCluster)


# --- Full Run
parallelCluster <- 
  makeCluster(detectCores(), outfile = paste0(format(Sys.time(), "%Y-%m-%dT%H%M%S%z"), ".txt"))
clusterExport(parallelCluster, c("airfoildata", "ThreadAll"))

a1 <- pblapply(
  folderdata,
  function(filedata) {
    with(filedata, ThreadAll(ID, Re, AoA, filepath, filedata, airfoildata))},
  cl = parallelCluster
)
stopCluster(parallelCluster)

