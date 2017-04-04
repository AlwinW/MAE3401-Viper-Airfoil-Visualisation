#---------------------------->
#--- Thread Function Calls
#--- Alwin Wang MAE3401
#============================>

#--- Full functionality ----
# This thread goes through each of the main functions
ThreadAll <- function(ID, Re, AoA, filepath, omesh, airfoildata) {
  #--- Required files ----
  source("Function Install Packages.R")
  LoadPackages()
  source("Plot Settings.R")
  savepath = "Output_Data"
  source("Function Airfoil Profile.R")
  source("Function Airfoil Normals.R")
  source("Function Interpolations.R")
  
  #--- Manipulate input data ----
  # Expand the airfoil data into the current thread environment
  list2env(airfoildata, envir = .GlobalEnv)
  
  #--- Airfoil profile and plot ----
  # Airfoil coordinates
  airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)
  # Plot of the airfoil with pressure of the original mesh
  plot_airfoil = ggplot() +
    geom_point(data = omesh, aes(x = x, y = y, colour = P)) +
    geom_point(data = filter(omesh, P < -0.5), aes(x = x, y = y, colour = P), colour = "#3C4BA0") +
    geom_point(data = filter(omesh, P >  0.5), aes(x = x, y = y, colour = P), colour = "#BE2828") +
    geom_path(data = airfoilcoord, aes(x = x, y = y)) + 
    xlim(c(-1, 1)) +
    ylim(c(-1, 1)) +
    coord_fixed() +
    scale_colour_gradientn("P", colours = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5)) +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: Airfoil"))
  # Save the plot
  ggsave(paste0(ID, "_Airfoil.png"), plot = plot_airfoil, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  
  #--- Interpolation on Airfoil
  airfoilmesh <- InterpPoint(omesh, airfoilcoord)
  plot_pressure = ggplot(airfoilmesh, aes(x = x, y = P, colour = surf)) +
    geom_path(size = 1.2) +
    geom_point() +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: Pressure on Airfoil"))
  ggsave(paste0(ID, "_Pressure.png"), plot = plot_pressure, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  
  #--- Interpolation on Normals
  xvec = AirfoilSamp(seq(a, a+c, by = 0.05), cylinder = TRUE)
  dist = NormalSamp(seq(0, 0.8, by = 0.1))
  
  interpvalU <- pblapply(xvec, function(x) {
    # Find the interpolations
    lvec = NormalPoint(x, dist, AoA, surf = "upper")
    interp <- InterpProj(omesh, lvec)
    return(interp)
  })
  interpvalL <- pblapply(xvec, function(x) {
    # Find the interpolations
    lvec = NormalPoint(x, dist, AoA, surf = "lower")
    interp <- InterpProj(omesh, lvec)
    return(interp)
  })
  interpvalLong <- bind_rows(c(interpvalU, interpvalL))
  
  
  plot_Udash_Rough <- ggplot () +
    geom_point(data = interpvalLong, aes(x = xp, y = yp, colour = Udash)) +
    geom_point(data = filter(interpvalLong, Udash < -1.2), aes(x = xp, y = yp), colour = "#BE2828") +
    geom_point(data = filter(interpvalLong, Udash > 1.2), aes(x = xp, y = yp), colour = "#3C4BA0") +
    geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
    xlim(-1.2, 0.8) +
    ylim(-0.8, 0.8) +
    scale_colour_gradientn("U'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
    coord_fixed() +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: U'"))
  ggsave(paste0(ID, "_Udash_Rough.png"), plot = plot_Udash_Rough, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  
  plot_Vdash_Rough <- ggplot () +
    geom_point(data = interpvalLong, aes(x = xp, y = yp, colour = Vdash)) +
    geom_point(data = filter(interpvalLong, Udash < -1.2), aes(x = xp, y = yp), colour = "#BE2828") +
    geom_point(data = filter(interpvalLong, Udash > 1.2), aes(x = xp, y = yp), colour = "#3C4BA0") +
    geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
    xlim(-1.2, 0.8) +
    ylim(-0.8, 0.8) +
    scale_colour_gradientn("V'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
    coord_fixed() +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: V'"))
  ggsave(paste0(ID, "_Vdash_Rough.png"), plot = plot_Vdash_Rough, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
}
