
source("Function Install Packages.R")
LoadPackages()

source("Function Load Data.R")
NACA = 4412
airfoildata <- AirfoilData(NACA, -0.5, 1)
folderdata <- LoadFolder()

parallelCluster <- parallel::makeCluster(parallel::detectCores())
asdfasfd <- parallel::parLapply(
  parallelCluster,
  folderdata,
  function(filedata) {
    attach(filedata)
    Re
    }
  )


stopCluster(parallelCluster)

a1 <-  lapply(
  folderdata,
  function(filedata) {
    with(filedata, Re + AoA)
  }
)