#---------------------------->
#--- Function Plots
#--- Alwin Wang MAE3401
#============================>

#--- ggplot Theme ----
theme_set(theme_bw())
options(scipen = 10)
update_geom_defaults("path", list(size = 1))
update_geom_defaults("polygon", list(fill = "white", colour = "grey20", size = 1))

#--- Save to external file ----
PlotSave <- function(plot, path, ID, width, height) {
  filename = paste0(ID, gsub("plot", "", deparse(substitute(plot))), ".png")
  ggsave(filename, plot = plot, path = path,
         width = width, height = height, scale = 1.4, dpi = 600)
}

PlotTitle <- function(name) {
  paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), name)
}

#--- ggplot Title ----
PlotTitle <- function(Re, AoA, name) {
  title <- paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), name)
  return(title)
}

#--- Plot of airfoil on omesh ----
PlotAirfoil <- function(omesh, airfoilcoord, var, x, y, min, max,
                        Re, AoA, name, rev = FALSE) {
  aes_col <- gsub('var', var, 'ifelse(var < min, min, ifelse(var > max, max, var))')
  colours <- brewer.pal(9, "RdYlBu")
  if(rev == TRUE) colours = rev(colours)
  plot <- 
    ggplot(data = omesh, aes_string(x = x, y = y)) + 
    geom_polygon(data = airfoilcoord, aes(x = x, y = y)) +
    geom_point(data = omesh,
               aes_string(colour = aes_col)) +
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    scale_colour_gradientn(paste(name, "\n"),
                           colours = colours, limits = c(min, max)) +
    labs(title = PlotTitle(Re, AoA, name))
  return(plot)
}

#--- Plot of coefficient of pressure ----
PlotCp <- function(airfoilsurfmesh, 
                   Re, AoA) {
  plot <- 
    ggplot(airfoilsurfmesh, aes(x = xO - a, y = P * 2, linetype = surf)) +
    geom_path() +
    scale_y_reverse() +
    scale_linetype_manual(
      "Surface",
      values = c("twodash", "solid")) +
    labs(title = PlotTitle(Re, AoA, "Coefficient of Pressure"), 
         y = expression(C[p]), x = "x (Aerofoil Chord)")
  return(plot)
}

#--- Plot of vorticity ----
PlotVort <- function(airfoilsurfmesh, Re, AoA) {
  plot <- 
    ggplot(airfoilsurfmesh, aes(x = xO - a, y = vort_xy_plane, linetype = surf)) +
    geom_path() +
    scale_y_reverse() + 
    scale_linetype_manual(
      "Surface",
      values = c("twodash", "solid")) +
    labs(title = PlotTitle(Re, AoA, "Vorticity"), 
       y = "Vorticity in the x-y Plane", x = "x (Aerofoil Chord)")
  return(plot)
}


PlotStag <- function(interpnorms, stagnation, Re, AoA) {
  plot <- 
    ggplot(interpnorms, 
      aes(x = ifelse(surf == "upper", 1, -1)*xO2chord(xO, surf), y = Uclock, 
      group = interaction(surf, dist), 
      colour = surf, linetype = as.factor(dist))) +
    geom_path() +
    geom_point(data = stagnation, size = 3) +
    labs(title = PlotTitle(Re, AoA, "Stagnation Point"), 
         y = "Velocity Parallel (clockwise about the centre)", x = "Lower TE > Lower LE > Upper LE > Upper TE")
  return(plot)
}
