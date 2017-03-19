#----------------------------
#--- Rough Running Script to Test Functions
#============================

#--- Source Files ----
source("Helper Install Packages.R")
source("Function Load Data.R")
source("Function Airfoil Profile.R")
source("Function Interpolation.R")

#--- Initial Data ----
filedata <- LoadData("test.dat")
NACA = 4412
a = -0.5
c = 1
AoA = 10
Re = 50
airfoildata <- AirfoilData(NACA, a, c)

#--- Sample Airfoil Plot ----
airfoilcoord <- AirfoilCoord(AoA = AoA, res = 100)
ggplot(airfoilcoord, aes(x = x, y = y, colour = surf)) + 
  geom_path() +
  geom_point() +
  coord_fixed()

#--- Sample Plot of Normals to the Airfoil
xvec = AirfoilSamp(seq(a, a+c, by = 0.05))
focusdist = 0.2; totaldist = 0.5; len = 21
normalplot <- ggplot () +
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x = x, y = y), size = 1.2)
for (x in xvec) {
  # x value
  normalplot = normalplot +
    geom_vline(xintercept = x, colour = "grey")
  # acutal plot
  for (surf in c("upper", "lower")) {
    normalplot = normalplot +
      geom_point(data = AirfoilLineGen(x, AoA, surf, eq = "norm", focusdist, totaldist, len),
      aes(x = x, y = y))
  }
}
normalplot + coord_fixed()

#--- Sample Interpolation Plot
omesh = filedata
# Along the airfoil
airfoilmesh <-  InterpPath(omesh, lvec = airfoilcoord, linear = TRUE)
airfoilmeshlong <- airfoilmesh %>%
  gather(var, value, -x, -y, -surf) %>%
  mutate(var = factor(var, levels = c("U", "V", "P", "vort_xy_plane"))) %>%
  group_by(var)
# ggplot(airfoilmeshlong) + 
#   geom_path(aes(x = x, y = y, colour = value)) + 
#   facet_wrap(~var, scales = "free") +
#   coord_equal() 

out <- by(data = airfoilmeshlong, INDICES = airfoilmeshlong$var, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(x = x, y = y, group = 1, colour = value)) +
    geom_path()
})
do.call(grid.arrange, out) # NEEDS TO BE FIXED ----

# Along a perpendicular lines
xvec = AirfoilSamp(seq(a, a+c, by = 0.2))
focusdist = 0.2; totaldist = 0.5; len = 21

pblapply(xvec, function(x) {
  list(
    data.frame(xO = x, surf, AoA, Re),
    InterpPath(omesh, x, AoA = AoA, surf = "upper", len = 5
   ))
})



data <- list(
  list(data.frame(subject = "A", year = "2016"),
       data.frame(results = rnorm(500), time = rnorm(500))
  ),
  list(data.frame(subject = "B", year = "2017"),
       data.frame(results = rnorm(500), time = rnorm(500))
  )
)

data.frame(
  subject = c(rep("A", 3), rep("B", 3)),
  year = c(rep(2016, 3), rep(2017, 3)),
  results = c(1, 2, 3, 7, 8, 9),
  time = c(4, 5, 6, 10, 11, 12)
)

df = data.frame()
for (i in (1:length(data))) {
  df = rbind(df,
             cbind(data[[i]][[1]], data[[i]][[2]]))
}

system.time({
  df = data.frame()
  for (i in (1:length(data))) {
    df = rbind(df,
               cbind(data[[i]][[1]], data[[i]][[2]]))
  }
})
