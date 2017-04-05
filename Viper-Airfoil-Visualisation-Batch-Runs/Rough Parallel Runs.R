# Set working directory when run from RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Function Install Packages.R")
LoadPackages()

source("Function Load Data.R")
NACA = 4412
airfoildata <- AirfoilData(NACA, -0.5, 1)
folderdata <- LoadFolder()

source("Thread Function Calls.R")


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



parallelCluster <- makeCluster(detectCores(), outfile = paste0("parallel", ".txt"))
source("Function Install Packages.R")
a1 <- pblapply(
  1:10,
  function(filedata) {
    set <- getAllConnections()
    ans <-  unlist(summary.connection(set[length(set)]))[1]
    cat(paste(ans, Sys.getpid(), "\n"))
    # cat(getpb(pb), "\n")
  },
  cl = parallelCluster
)
stopCluster(parallelCluster)