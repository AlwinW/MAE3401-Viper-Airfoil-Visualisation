
source("Function Install Packages.R")
LoadPackages()

source("Function Load Data.R")
NACA = 4412
airfoildata <- AirfoilData(NACA, -0.5, 1)
folderdata <- LoadFolder()

source("Thread Function Calls.R")


parallelCluster <- makeCluster(parallel::detectCores())
clusterExport(parallelCluster, c("airfoildata", "ThreadAll"))

a1 <- pblapply(
  folderdata,
  function(filedata) {
    with(filedata, ThreadAll(ID, Re, AoA, filepath, filedata, airfoildata))},
  cl = parallelCluster
)
stopCluster(parallelCluster)









#---------------- Tests
# This function would go in another source file!
dummythread <- function(Re, AoA) {
  list2env(airfoildata, envir = .GlobalEnv)
  
  out = Re + AoA + r*100
  return(out)
}

source("Thread Function Calls.R")

parallelCluster <- makeCluster(parallel::detectCores())
clusterExport(parallelCluster, c("airfoildata", "dummythread"))

a1 <- pblapply(
  folderdata,
  function(filedata) {
    with(filedata, dummythread(Re, AoA))},
  cl = parallelCluster
  )
stopCluster(parallelCluster)