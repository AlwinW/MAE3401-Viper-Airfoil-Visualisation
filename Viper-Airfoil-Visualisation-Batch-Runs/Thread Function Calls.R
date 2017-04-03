#----------------------------
#--- Thread Function Calls
#============================

#--- Full functionality ----
# This thread goes through each of the main functions
ThreadAll <- function(ID, Re, AoA, filepath, omesh, airfoildata) {
  #--- Required files ----
  source("Function Install Packages.R")
  LoadPackages()
  source("Plot Settings.R")
  savepath = "Output_Data"
  source("Function Airfoil Profile.R")
  
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
}
