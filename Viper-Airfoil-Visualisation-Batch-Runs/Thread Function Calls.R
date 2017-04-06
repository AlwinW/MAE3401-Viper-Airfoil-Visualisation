#---------------------------->
#--- Thread Function Calls
#--- Alwin Wang MAE3401
#============================>

# threaddata = folderdata[[1]]
# list2env(threaddata, envir = .GlobalEnv)
# omesh = filedata


ThreadLoopTest <- function(threaddata) {
  list2env(threaddata, envir = environment())
  omesh = filedata
  
  #--- Required files ----
  source("Function Load Packages.R")
  LoadPackages()
  source("Plot Settings.R")
  savepath = "Output_Data"
  source("Function Airfoil Profile.R")
  source("Function Airfoil Normals.R")
  source("Function Interpolations.R")
  source("Function Boundary Layers.R")
  source("Function pblapply.R")
  
  list2env(airfoildata, envir = environment())
  PrintThread("Files Loaded")
  cat("---------------------------------------------------------------\n")
  #
  airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)
  PrintThread("Airfoil Coordinates Plotted")
  #
  airfoilmesh <- InterpPoint(omesh, airfoilcoord)
  PrintThread("Pressure on Airfoil Plotted")
  #
  xvec = AirfoilSamp(seq(a, a+c, by = 0.2), cylinder = TRUE)
  dist = NormalSamp(seq(0, 0.8, by = 0.05))
  interpvalU <- pblapply(xvec, function(x) {
    lvec = NormalPoint(x, dist, AoA, surf = "upper")
    interp <- InterpProj(omesh, lvec, plotsurf = TRUE)
    return(interp)
  })
  interpvalL <- pblapply(xvec, function(x) {
    lvec = NormalPoint(x, dist, AoA, surf = "lower")
    interp <- InterpProj(omesh, lvec, plotsurf = TRUE)
    return(interp)
  })
  interpvalLong <- bind_rows(c(interpvalU, interpvalL))
  #
  PrintThread("U' and V' Calculated") 
  xvec = AirfoilSamp(seq(a, a+c, by = 0.2), cylinder = FALSE)
  blvalU <- pblapply(xvec, function(x) {
    blval = BLCalcs(omesh, x, surf = "upper", AoA, Re)
    return(blval)
  })
  blvalL <- pblapply(xvec, function(x) {
    blval = BLCalcs(omesh, x, surf = "lower", AoA, Re)
    return(blval)
  })
  blvalLong <- bind_rows(c(blvalU, blvalL))
  PrintThread("Boundary Layers Calculated") 
  #
  return(NULL)
}


#--- Full functionality ----
# This thread goes through each of the main functions
ThreadAll <- function(ID, Re, AoA, filepath, omesh, airfoildata) {

  #--- Required files ----
  source("Function Load Packages.R")
  LoadPackages()
  source("Plot Settings.R")
  savepath = "Output_Data"
  source("Function Airfoil Profile.R")
  source("Function Airfoil Normals.R")
  source("Function Interpolations.R")
  source("Function Boundary Layers.R")
  source("Function pblapply.R")
  
  #--- Manipulate input data ----
  # Expand the airfoil data into the current thread environment
  list2env(airfoildata, envir = environment())
  
  
  
  PrintThread("Files Loaded") # Print----
  cat("---------------------------------------------------------------\n")
  
  #--- Airfoil profile and plot ----
  # Airfoil coordinates
  airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)
  # Plot of the airfoil with pressure of the original mesh
  plot_airfoil = ggplot() +
    geom_point(data = omesh, aes(x = x, y = y, colour = P)) +
    geom_point(data = filter(omesh, P < -0.5), aes(x = x, y = y, colour = P), colour = "#3C4BA0") +
    geom_point(data = filter(omesh, P >  0.5), aes(x = x, y = y, colour = P), colour = "#BE2828") +
    geom_path(data = airfoilcoord, aes(x = x, y = y)) + 
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    scale_colour_gradientn("P", colours = rev(brewer.pal(11, "RdYlBu")), limits = c(-0.5, 0.5)) +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: Airfoil"))
  # Save the plot
  ggsave(paste0(ID, "_Airfoil.png"), plot = plot_airfoil, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  
  PrintThread("Airfoil Coordinates Plotted") # Print----
  
  #--- Interpolation on Airfoil ----
  airfoilmesh <- InterpPoint(omesh, airfoilcoord)
  plot_pressure = ggplot(airfoilmesh, aes(x = x, y = P, colour = surf)) +
    geom_path(size = 1.2) +
    geom_point() +
    scale_y_reverse() +
    scale_color_manual("Surface", values = c("#F8766D", "#00BBCC"), labels = c("Lower","Upper")) +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: Pressure on Airfoil"))
  ggsave(paste0(ID, "_Pressure.png"), plot = plot_pressure, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  
  PrintThread("Pressure on Airfoil Plotted") # Print----
  
  #--- Interpolation on Normals ----
  # xvec by = 0.05 and dist = 0.05 approx 2min for U and 2min for L on my laptop
  xvec = AirfoilSamp(seq(a, a+c, by = 0.05), cylinder = TRUE)
  dist = NormalSamp(seq(0, 0.8, by = 0.05))
  # Upper Surface
  interpvalU <- pblapply(xvec, function(x) {
    # Find the interpolations
    lvec = NormalPoint(x, dist, AoA, surf = "upper")
    interp <- InterpProj(omesh, lvec, plotsurf = TRUE)
    return(interp)
  })
  #Lower Surface
  interpvalL <- pblapply(xvec, function(x) {
    # Find the interpolations
    lvec = NormalPoint(x, dist, AoA, surf = "lower")
    interp <- InterpProj(omesh, lvec, plotsurf = TRUE)
    return(interp)
  })
  interpvalLong <- bind_rows(c(interpvalU, interpvalL))
  
  PrintThread("U' and V' Calculated") # Print----
  
  #U' Plot
  plot_Udash_Rough <- ggplot () +
    geom_point(data = interpvalLong, aes(x = xp, y = yp, colour = Udash)) +
    geom_point(data = filter(interpvalLong, Udash < -1.2), aes(x = xp, y = yp), colour = "#BE2828") +
    geom_point(data = filter(interpvalLong, Udash > 1.2), aes(x = xp, y = yp), colour = "#3C4BA0") +
    geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
    scale_colour_gradientn("U'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
    coord_fixed(xlim = c(-1.2, 0.8), ylim = c(-0.8, 0.8)) +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: U'"))
  ggsave(paste0(ID, "_Udash_Rough.png"), plot = plot_Udash_Rough, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  # V'plot
  plot_Vdash_Rough <- ggplot () +
    geom_point(data = interpvalLong, aes(x = xp, y = yp, colour = Vdash)) +
    geom_point(data = filter(interpvalLong, Udash < -1.2), aes(x = xp, y = yp), colour = "#BE2828") +
    geom_point(data = filter(interpvalLong, Udash > 1.2), aes(x = xp, y = yp), colour = "#3C4BA0") +
    geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
    scale_colour_gradientn("V'", colours = brewer.pal(11, "RdYlBu"), limits = c(-1.2, 1.2)) +
    coord_fixed(xlim = c(-1.2, 0.8), ylim = c(-0.8, 0.8)) +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: V'"))
  ggsave(paste0(ID, "_Vdash_Rough.png"), plot = plot_Vdash_Rough, path = savepath,
         width = 5, height = 4, scale = 1.2, dpi = 300)
  
  PrintThread("U' and V' Plotted") # Print----
  
  #--- Boundary Layer Thicknesses ----
  xvec = AirfoilSamp(seq(a, a+c, by = 0.01), cylinder = FALSE)
  # Upper Surface
  blvalU <- pblapply(xvec, function(x) {
    # Find the thicknesses
    blval = BLCalcs(omesh, x, surf = "upper", AoA, Re)
    return(blval)
  })
  # Lower Surface
  blvalL <- pblapply(xvec, function(x) {
    # Find the thicknesses
    blval = BLCalcs(omesh, x, surf = "lower", AoA, Re)
    return(blval)
  })
  blvalLong <- bind_rows(c(blvalU, blvalL))
  
  PrintThread("Boundary Layers Calculated") # Print----
  
  # Plot of Boundary Layers over the Airfoil
  plot_BLsurf_Rough <- ggplot() +
    geom_path(data = blvalLong,
              aes(x = xp, y = yp, colour = bldistname,
                  group = interaction(bldistname, surf))) +
    geom_path(data = airfoilcoord, aes(x = x, y = y), size = 1.2) +
    scale_color_discrete("BL Distance") +
    coord_fixed(xlim = c(-1.2, 0.8), ylim = c(-0.8, 0.8)) +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: Boundary Layer"))
  ggsave(paste0(ID, "_BLsurf_Rough.png"), plot = plot_BLsurf_Rough, path = savepath,
         width = 6, height = 4, scale = 1.2, dpi = 300)
  
  # Plot of Boundary Layers on a log-log plot
  plot_BLlog_Rough <- 
    ggplot(data = filter(blvalLong, bldistname %in% c("Thickness", "Theory")),
           aes(x = (xO - a), y = bldist, colour = bldistname, 
               group = interaction(bldistname, surf))) +
    geom_path(aes(linetype = surf)) +
    geom_point(aes(shape = surf)) +
    scale_color_discrete("BL Distance") +
    scale_x_log10() +
    scale_y_log10() +
    coord_fixed() +
    labs(title = paste("Re Number", Re, "and", "AoA", AoA, "deg: Boundary Layer"),
         x = "Location Along Chord",
         y = "Boundary Layer Thickness")
  ggsave(paste0(ID, "_BLlog_Rough.png"), plot = plot_BLlog_Rough, path = savepath,
         width = 6, height = 4, scale = 1.2, dpi = 300)
  
  PrintThread("Boundary Layers Plotted") # Print----
  
  return(NULL)
}
