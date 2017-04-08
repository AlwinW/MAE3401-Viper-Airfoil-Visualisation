#---------------------------->
#--- Function Plots
#--- Alwin Wang MAE3401
#============================>

#--- ggplot Theme ----
# theme_set(theme_linedraw())
theme_set(theme_bw())
options(scipen = 10)
update_geom_defaults("path", list(size = 1))
update_geom_defaults("polygon", list(fill = "white", colour = "black", size = 1))


PlotAirfoilSurf <- function(var, min, max) {
  aes_col <- gsub('var', var, 'ifelse(var < min, min, ifelse(var > max, max, var))')
  plot <- 
    ggplot(data = omesh, aes(x = x, y = y)) + 
    geom_point(data = omesh,
               aes_string(colour = aes_col)) +
    geom_polygon(data = airfoilcoord) +
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    scale_colour_gradientn(var,
                           colours = rev(brewer.pal(9, "RdYlBu")), limits = c(min, max))
  return(plot)
}
