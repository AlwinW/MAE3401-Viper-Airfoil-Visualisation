
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


#--- PBAPPLY EXAMPLE
set.seed(1234)
n <- 2000
x <- rnorm(n)
y <- rnorm(n, crossprod(t(model.matrix(~ x)), c(0, 1)), sd = 0.5)
d <- data.frame(y, x)
mod <- lm(y ~ x, d)
ndat <- model.frame(mod)
B <- 5000
bid <- sapply(1:B, function(i) sample(nrow(ndat), nrow(ndat), TRUE))
fun <- function(z) {
  if (missing(z))
    z <- sample(nrow(ndat), nrow(ndat), TRUE)
  coef(lm(mod$call$formula, data=ndat[z,]))
}
parallelCluster <- makeCluster(parallel::detectCores())
clusterExport(parallelCluster, c("fun", "mod", "ndat", "bid"))
system.time(res1cl <- parLapply(cl = parallelCluster, 1:B, function(i) fun(bid[,i])))
pboptions(nout = 10)
system.time(res1pbcl <- pblapply(1:B, function(i) fun(bid[,i]), cl = parallelCluster))
pboptions(nout = 100)
system.time(res1pbcl <- pblapply(1:B, function(i) fun(bid[,i]), cl = parallelCluster))
stopCluster(parallelCluster)
#--- END

parallelCluster <- makeCluster(parallel::detectCores())
a1 <- pblapply(
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
    },
  cl = parallelCluster
  )
stopCluster(parallelCluster)



a1 <-  lapply(
  folderdata,
  function(filedata) {
    with(filedata, Re + AoA + NACA)
  }
)