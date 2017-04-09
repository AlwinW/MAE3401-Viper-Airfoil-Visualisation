#---------------------------->
#--- Thread All Functions
#--- Alwin Wang MAE3401
#============================>

# filename = filelist[1]

#--- Full Functionality ----
TreadAll <- function(filename, foldername, airfoildata, savedata, saveplot) {
  #--- Load Packages ----
  source("Function Load Packages.R")      # For required packages
  source("Function Plot.R")               # Plot settings and functions
  source("Function Save Load.R")          # For saving and loading data
  
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
  # >> Calcs Done ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Coordinates Calculated")
  
  # Plots
  plot_airfoil_P = 
    PlotAirfoil(omesh, airfoilcoord, "P", "x", "y", -0.2, 0.2, Re, AoA, "Pressure", rev = TRUE)
  plot_airfoil_vort = 
    PlotAirfoil(omesh, airfoilcoord, "vort_xy_plane", "x", "y", -20, 20, Re, AoA, "Vorticity", rev = TRUE)
  
  PlotSave(plot_airfoil_P, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_airfoil_vort, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
  ThreadProgress(threadname, Re, AoA, "Airfoil Surface Values Plotted")
  
  # >> Save Done ----
  ObjSave(airfoilcoord, plot_airfoil_P, plot_airfoil_vort,
          path = savedata, ID = ID)
  
  
  #--- Interpolation on the airfoil----
  source("Function Interpolations.R")     # For fn "InterpPoint", etc
  airfoilsurfmesh <- InterpPoint(omesh, airfoilcoord, varnames = c("P", "vort_xy_plane"))
  # >> Calcs Done ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Surface Interpolation Calculated")
  
  # Coeffients of pressure and vorticity
  plot_cp 
  ggplot(airfoilsurfmesh, aes(x = x, y = P * 2, linetype = surf)) +
    geom_path() +
    scale_y_reverse() +
    scale_linetype_manual("Surface",
      values = c("twodash", "solid"), labels = c("Upper", "Lower")) +
    
      
  
  #--- Interpolation on Normals ----
  source("Function Airfoil Normals.R")    # For "AirfoilGrads", etc
  xvec = AirfoilSamp(seq(a, a+c, by = 0.5), cylinder = TRUE)
  dist = NormalSamp(seq(0, 1.5, by = 0.05))
  # Find the combined lvec for interpolation
  lvec <- NormalLvec(xvec, dist, AoA)
  interpval <- InterpProj(omesh, lvec, plotsurf = TRUE)
  # >> Calcs Done ----
    ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")
  
  # Plots
  plot_Norm_Udash = 
    PlotAirfoil(interpval, airfoilcoord, "Udash", "xp", "yp", -1.2, 1.2, Re, AoA, "U'")
  plot_Norm_Vdash = 
    PlotAirfoil(interpval, airfoilcoord, "Vdash", "xp", "yp", -0.8, 0.8, Re, AoA, "V'")
  plot_Norm_UUmdash = 
    PlotAirfoil(interpval, airfoilcoord, "UUmdash", "xp", "yp", -1.2, 1.2, Re, AoA, "U'/Um'")
  plot_Norm_VVmdash = 
    PlotAirfoil(interpval, airfoilcoord, "VVmdash", "xp", "yp", -1.2, 1.2, Re, AoA, "V'/Vm'")
  plot_Norm_P = 
    PlotAirfoil(interpval, airfoilcoord, "Vdash", "xp", "yp", -0.2, 0.2, Re, AoA, "Pressure")
  plot_Norm_vort = 
    PlotAirfoil(interpval, airfoilcoord, "vort_xy_plane", "xp", "yp", -20, 20, Re, AoA, "Vorticity")
  
  PlotSave(plot_Norm_Udash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_Vdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_UUmdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_VVmdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_P, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_vort, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
  ThreadProgress(threadname, Re, AoA, "Airfoil Surface Values Plotted")
  
  # SAVE INTERPVAL then delete it!

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


