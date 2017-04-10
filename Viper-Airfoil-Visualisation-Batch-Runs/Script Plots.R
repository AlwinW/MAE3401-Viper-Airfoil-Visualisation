#---------------------------->
#--- Function Plots
#--- Alwin Wang MAE3401
#============================>

#--- ggplot Theme ----
# theme_set(theme_linedraw())
theme_set(theme_bw())
options(scipen = 10)
update_geom_defaults("path", list(size = 1))
update_geom_defaults("polygon", list(fill = "white", colour = "grey", size = 1))

#--- Save to external file ----
PlotSave <- function(plot, path, ID, width, height) {
  filename = paste0(ID, gsub("plot", "", deparse(substitute(plot))), ".png")
  ggsave(filename, plot = plot, path = path,
         width = width, height = height, scale = 1.4, dpi = 600)
}

PlotTitle <- function(name) {
  paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), name)
}

#--- Plot of airfoil on omesh ----
PlotAirfoil <- function(omesh, airfoilcoord, var, x, y, min, max,
                        Re, AoA, name, rev = FALSE) {
  aes_col <- gsub('var', var, 'ifelse(var < min, min, ifelse(var > max, max, var))')
  title <- paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), name)
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
    labs(title = title)
  return(plot)
}