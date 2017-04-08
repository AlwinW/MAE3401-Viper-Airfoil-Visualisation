#---------------------------->
#--- Thread All Functions
#--- Alwin Wang MAE3401
#============================>

#--- Full Functionality ----
TreadAll <- function(filename, foldername, airfoildata, savepath) {
  #--- Load Packages ----
  source("Function Load Packages.R")      # For required packages
  source("Function Plot.R")
  
  #--- Custom pblapply ----
  source("Function pblapply.R")           # For PrintThreadProgress
  threadname <- ThreadName()
  
  #--- Load ALL file information  ----
  source("Function Load Data.R")          # For fn "Load File"
  # Load the filedata and unlist it
  filedata <- LoadFile(filename, foldername)
  list2env(filedata, envir = environment()); rm(filedata)       # N.B: local so must be passed as fn input
  
  # >> File Data Loaded ----
    ThreadProgress(threadname, Re, AoA, "File Data Loaded
      ---------------------------------------------------------------")
  
  #--- Run Airfoil Calculations ----
  source("Function Airfoil Profile.R")    # For fn "AirfoilCoord", etc
  list2env(airfoildata, envir = .GlobalEnv)                     # N.B: global so all fn can find it
  airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)
  # >> Airfoil Coordinates Calculated ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Coordinates Calculated")
  
  #--- Interpolation on the airfoil----
  source("Function Interpolations.R")     # For fn "InterpPoint", etc
  airfoilsurfmesh <- InterpPoint(omesh, airfoilcoord, varnames = c("P", "vort_xy_plane"))
  # >> Airfoil Surface Interpolation Calculated ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Surface Interpolation Calculated")
  
  # Plots
  varmin = -0.5; varmax = 0.5
  var = "P"
  aes_col <- gsub('var', var, 'ifelse(var < varmin, varmin, ifelse(var > varmax, varmax, var))')
  plot <- 
    ggplot(data = omesh, aes(x = x, y = y)) + 
    geom_point(data = omesh,
               aes(colour = ifelse(P < varmin, varmin, ifelse(P > varmax, varmax, P)))) +
    geom_polygon(data = airfoilcoord) +
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    scale_colour_gradientn(var,
      colours = rev(brewer.pal(9, "RdYlBu")), limits = c(varmin, varmax))
  
  varmin = -20; varmax = 20
  var = "P"
  plot <- 
    ggplot(data = omesh, aes(x = x, y = y)) + 
    geom_point(data = omesh,
      aes(colour = ifelse(vort_xy_plane < varmin, varmin, ifelse(vort_xy_plane > varmax, varmax, vort_xy_plane)))) +
    geom_polygon(data = airfoilcoord) +
    coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
    scale_colour_gradientn(var,
      colours = rev(brewer.pal(9, "RdYlBu")), limits = c(varmin, varmax))
  # >> Airfoil Surface Values Plotted ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Surface Values Plotted")
  
  #--- Interpolation on Normals ----
  source("Function Airfoil Normals.R")    # For "AirfoilGrads", etc
  xvec = AirfoilSamp(seq(a, a+c, by = 0.5), cylinder = TRUE)
  dist = NormalSamp(seq(0, 1.5, by = 0.05))
  # Find the combined lvec for interpolation
  lvec <- NormalLvec(xvec, dist, AoA)
  interpval <- InterpProj(omesh, lvec, plotsurf = TRUE)
  
  
  # SAVE INTERPVAL then delete it!
  # >> Interpolation on Normals to Surface Calculated ----
    ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")
  
  
  #--- Boundary Layer Calculations ----
  source("Function Boundary Layers.R")    # For "BLCalcs", etc
  xvec = AirfoilSamp(seq(a, a+c, by = 0.5), polyn = 5, cylinder = TRUE)
  blvals = BLCalcs(omesh, xvec, AoA, Re)
  bltheory = BLTheory(omesh, xvec, AoA, Re)
  blplot = bind_rows(blvals, bltheory)
  # >> Boundary Layers Calculated ----
    ThreadProgress(threadname, Re, AoA, "Boundary Layers Calculated")
  
  
  #--- Velocity Profile Calculations ----
  sep = 0.25
  vec = VelProfileLvec(omesh, sep, blvals, AoA)
  xvec = vec$xvec; lvec = vec$lvec; rm(vec)
  velprofile = VelProfile(blvals, xvec, lvec, omesh, AoA, Re)
  vptheory = Blasius(lvec, Re)
  # >> Velocity Profiles Calculated ----
    ThreadProgress(threadname, Re, AoA, "Velocity Profiles Calculated")
  
  
  #--- Finish Function Call ----
  space.usage <- sapply(ls(), function(x) object.size(get(x)))
  status = paste("Completed: ", sum(space.usage)/1024^2, "Mb")
  # >> Thread Completed ----
    ThreadProgress(threadname, Re, AoA, status)
  return(status)
}


