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
xvec = seq(a, a+c, by = 0.05)
xvec = -2*a/c^3 * (xvec - a)^3 + a
xvec = c(xvec[2:(length(xvec)-1)], (a+c) - sign(a+c)*(xvec[2]-a))
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
airfoilmesh <-  InterpLine(omesh, lvec = airfoilcoord, linear = TRUE)
airfoilmeshlong <- airfoilmesh %>%
  gather(var, value, -x, -y, -surf) %>%
  mutate(var = factor(var, levels = c("U", "V", "P", "vort_xy_plane"))) %>%
  group_by(var)
ggplot(airfoilmeshlong) + 
  geom_path(aes(x = x, y = y, colour = value)) + 
  facet_wrap(~var, scales = "free") +
  coord_equal() 


out <- by(data = airfoilmeshlong, INDICES = airfoilmeshlong$var, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(x = x, y = y, group = 1, colour = value)) +
    geom_path()
})
do.call(grid.arrange, out)
