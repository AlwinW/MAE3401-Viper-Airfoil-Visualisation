#----------------------------
#--- Rough Running Script to Test Functions
#============================

#--- Source Files ----
source("Helper Install Packages.R")
source("Function Load Data.R")
source("Function Airfoil Profile.R")
source("Function Interpolation.R")

#--- Initial Data ----
filedata <- LoadData("test.dat") ## CHANGE THIS TO YOUR FILE
NACA = 4412
a = -0.5
c = 1
AoA = -40 ## CHANGE THIS TO YOUR AoA
Re = 50
airfoildata <- AirfoilData(NACA, a, c)

#--- Sample Airfoil Plot ----
airfoilcoord <- AirfoilCoord(AoA = AoA, res = 100)
ggplot(airfoilcoord, aes(x = x, y = y, colour = surf)) + 
  geom_path() +
  geom_point() +
  coord_fixed()

#--- Sample Plot of Normals to the Airfoil ----
xvec = AirfoilSamp(seq(a, a+c, by = 0.05), cylinder = TRUE)

focusdist = 0.05; totaldist = 0.5; len = 21
normalplot <- ggplot () +
  geom_path(data = AoATransform(AirfoilCoord(), AoA = AoA), aes(x = x, y = y), size = 1.2)
for (x in xvec) {
  # x value
  if (AoA == 1 & x >= a) {
    normalplot = normalplot +
      geom_vline(xintercept = x, colour = "grey")
  }
  # acutal plot
  for (surf in c("upper", "lower")) {
    normalplot = normalplot +
      geom_point(data = AirfoilLineGen(x, AoA, surf, eq = "norm", focusdist, totaldist, len),
      aes(x = x, y = y))
  }
}
normalplot + coord_fixed()

#--- Sample Interpolation Plots ----
omesh = filedata
# Along the airfoil
airfoilmesh <-  InterpPath(omesh, lvec = airfoilcoord, linear = TRUE)
airfoilmeshlong <- airfoilmesh %>%
  gather(var, value, -x, -y, -surf) %>%
  mutate(var = factor(var, levels = c("U", "V", "P", "vort_xy_plane"))) %>%
  group_by(var)

ggplot(filter(airfoilmeshlong, var == "P"), aes(x = x, y = y, colour = value)) +
  geom_path() +
  geom_point() +
  geom_point(data = filter(airfoilmeshlong, var == "P" & value > 0.8), aes(x = x, y = y), colour = "#BE2828") +
  scale_colour_gradientn("P", colours = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.8, 0.8)) +
  coord_equal()
  

out <- by(data = airfoilmeshlong, INDICES = airfoilmeshlong$var, FUN = function(m) {
  m <- droplevels(m)
  m <- ggplot(m, aes(x = x, y = y, group = 1, colour = value)) +
    geom_path()
})
do.call(grid.arrange, out) # NEEDS TO BE FIXED ----

# Along a perpendicular lines ----
omesh = filedata
xvec = AirfoilSamp(seq(a, a+c, by = 0.025), cylinder = TRUE)

# Quicker run
xvec = c(head(xvec, 30), tail(xvec, 1))

InterpTest1U <- pblapply(xvec, function(x) {
  list(
    data.frame(xO = x, surf = "upper", AoA, Re),
    InterpPerpLine(omesh, x, AoA = AoA, surf = "upper")
    )
})
InterpTest1LongU = data.frame()
for (i in (1:length(InterpTest1U))) {
  InterpTest1LongU = rbind(InterpTest1LongU,
              cbind(InterpTest1U[[i]][[1]], InterpTest1U[[i]][[2]]))
}

InterpTest1L <- pblapply(xvec, function(x) {
  list(
    data.frame(xO = x, surf = "lower", AoA, Re),
    InterpPerpLine(omesh, x, AoA = AoA, surf = "lower")
  )
})
InterpTest1LongL = data.frame()
for (i in (1:length(InterpTest1L))) {
  InterpTest1LongL = rbind(InterpTest1LongL,
              cbind(InterpTest1L[[i]][[1]], InterpTest1L[[i]][[2]]))
}

InterpTest1Long = rbind(InterpTest1LongU, InterpTest1LongL)
save(InterpTest1Long, file = "InterpTest1Long.RData")
load(file = "InterpTest1Long.RData")

##BLUE - 3C4BA0, #RED - BE2828

# Plot of U' i.e. perp to the normal from the airfoil
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = Udash)) +
  geom_point(data = filter(InterpTest1Long, Udash < -1.2), aes(x = x, y = y, colour = Udash), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, Udash > 1.2), aes(x = x, y = y, colour = Udash), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("U'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
  coord_fixed()

# Plot of U'/Um, where Um is 1 x sin(theta)
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = percentUm)) +
  geom_point(data = filter(InterpTest1Long, percentUm < -150), aes(x = x, y = y, colour = percentUm), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, percentUm > 150), aes(x = x, y = y, colour = percentUm), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("U'/Um %", colours = brewer.pal(11, "RdYlBu"), limits = c(-150, 150)) +
  coord_fixed()

# Plot of Um, where Um is 1 x sin(theta)
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = Um)) +
  geom_point(data = filter(InterpTest1Long, Um < -1.2), aes(x = x, y = y, colour = Um), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, Um > 1.2), aes(x = x, y = y, colour = Um), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("Um", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
  coord_fixed()

# Plot of V' i.e. para to normal from the airfoil
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = Vdash)) +
  geom_point(data = filter(InterpTest1Long, Vdash < -0.8), aes(x = x, y = y, colour = Vdash), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, Vdash > 0.8), aes(x = x, y = y, colour = Vdash), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("V'", colours = brewer.pal(11, "RdYlBu"), limits = c(-0.8, 0.8)) +
  coord_fixed()

# Plot of V'/Vm, where Vm is 1 x cos(theta)
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = percentVm)) +
  geom_point(data = filter(InterpTest1Long, percentVm < -200), aes(x = x, y = y, colour = percentVm), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, percentVm > 200), aes(x = x, y = y, colour = percentVm), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("V'/Vm %", colours = brewer.pal(11, "RdYlBu"), limits = c(-200, 200)) +
  coord_fixed()

# Plot of Vm, where Vm is 1 x cos(theta)
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = Vm)) +
  geom_point(data = filter(InterpTest1Long, Vm < -1.2), aes(x = x, y = y, colour = Vm), colour = "#BE2828") +
  geom_point(data = filter(InterpTest1Long, Vm > 1.2), aes(x = x, y = y, colour = Vm), colour = "#3C4BA0") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("Um", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
  coord_fixed()

# Plot of Pressure
ggplot () +
  geom_point(data = InterpTest1Long, aes(x = x, y = y, colour = P)) +
  geom_point(data = filter(InterpTest1Long, P < -0.5), aes(x = x, y = y, colour = P), colour = "#3C4BA0") +
  geom_point(data = filter(InterpTest1Long, P > 0.5), aes(x = x, y = y, colour = P), colour = "#BE2828") +
  geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
  xlim(-1.2, 0.8) +
  ylim(-0.8, 0.8) +
  scale_colour_gradientn("P", colours = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5)) +
  coord_fixed()