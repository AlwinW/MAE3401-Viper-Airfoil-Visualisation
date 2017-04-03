
source("Function Install Packages.R")
LoadPackages()

source("Function Load Data.R")
folderdata <- LoadFolder()

parallelCluster <- parallel::makeCluster(parallel::detectCores())
asdfasfd <- parallel::parLapply(
  parallelCluster,
  folderdata,
  function(filedata) {
    source("Function Load Data.R")
    NACA = 4412
    airfoildata <- AirfoilData(NACA, -0.5, 1)
    with(filedata, Re + AoA + NACA)
  }
)
stopCluster(parallelCluster)





parallelCluster <- parallel::makeCluster(parallel::detectCores())
asdfasfd <- parallel::parLapply(
  parallelCluster,
  folderdata,
  function(filedata) {
    
    # This function would go in another source file!
    dummythread <- function(Re, AoA) {
      source("Function Load Data.R")
      NACA = 4412
      airfoildata <- AirfoilData(NACA, -0.5, 1)
      out = Re + AoA + NACA
      return(out)
    }
    
    with(filedata, dummythread(Re, AoA))
    }
  )
stopCluster(parallelCluster)



a1 <-  lapply(
  folderdata,
  function(filedata) {
    with(filedata, Re + AoA + NACA)
  }
)