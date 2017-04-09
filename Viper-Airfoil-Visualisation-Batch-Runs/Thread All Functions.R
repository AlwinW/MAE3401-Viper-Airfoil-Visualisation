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
  ObjSave(airfoilcoord, omesh, plot_airfoil_P, plot_airfoil_vort,
          path = savedata, ID = ID)
  rm(plot_airfoil_P, plot_airfoil_vort)
  
  
  #--- Interpolation on the airfoil----
  source("Function Interpolations.R")     # For fn "InterpPoint", etc
  airfoilsurfmesh <- InterpPoint(omesh, airfoilcoord, varnames = c("P", "vort_xy_plane"))
  airfoilsurfmesh$surf = factor(airfoilsurfmesh$surf, levels = c("U", "L"),  labels = c("Upper", "Lower"))
  # >> Calcs Done ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Surface Interpolation Calculated")
  
  # Coeffients of pressure and vorticity
  plot_cp = ggplot(airfoilsurfmesh, aes(x = x, y = P * 2, linetype = surf)) +
    geom_path() +
    scale_y_reverse() +
    scale_linetype_manual("Surface",
      values = c("twodash", "solid"), labels = c(Upper = "Upper", Lower = "Lower")) +
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Coefficient of Pressure"), 
       y = expression(C[p]), x = "x (Airfoil Chord)")
  plot_vort = ggplot(airfoilsurfmesh, aes(x = x, y = vort_xy_plane, linetype = surf)) +
    geom_path() +
    scale_y_reverse() + 
    # scale_y_log10() + # Make it a log scale laters
    scale_linetype_manual("Surface",
      values = c("twodash", "solid"), labels = c(Upper = "Upper", Lower = "Lower")) +
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Vorticity"), 
         y = "Vorticity in the x-y Plane", x = "x (Airfoil Chord)")
  PlotSave(plot_cp, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_vort, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
    ThreadProgress(threadname, Re, AoA, "Airfoil Cp and Vort Plotted")
  # >> Save Done ----
  ObjSave(airfoilsurfmesh, plot_cp, plot_vort,
          path = savedata, ID = ID)
  rm(airfoilsurfmesh, plot_cp, plot_vort)
  
  #--- Interpolation on Normals ----
  source("Function Airfoil Normals.R")    # For "AirfoilGrads", etc
  xvec = AirfoilSamp(seq(a, a+c, by = 0.05), cylinder = TRUE)
  dist = NormalSamp(seq(0, 1.5, by = 0.05))
  # Find the combined lvec for interpolation
  lvec <- NormalLvec(xvec, dist, AoA)
  interpnorms <- InterpProj(omesh, lvec, plotsurf = TRUE)
  # >> Calcs Done ----
    ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")
  
  # Plots
  plot_Norm_Udash = 
    PlotAirfoil(interpnorms, airfoilcoord, "Udash", "xp", "yp", -1.2, 1.2, Re, AoA, "U'")
  plot_Norm_Vdash = 
    PlotAirfoil(interpnorms, airfoilcoord, "Vdash", "xp", "yp", -0.8, 0.8, Re, AoA, "V'")
  plot_Norm_UUmdash = 
    PlotAirfoil(interpnorms, airfoilcoord, "UUmdash", "xp", "yp", -1.2, 1.2, Re, AoA, "U'/Um'")
  plot_Norm_VVmdash = 
    PlotAirfoil(interpnorms, airfoilcoord, "VVmdash", "xp", "yp", -1.2, 1.2, Re, AoA, "V'/Vm'")
  plot_Norm_P = 
    PlotAirfoil(interpnorms, airfoilcoord, "Vdash", "xp", "yp", -0.2, 0.2, Re, AoA, "Pressure")
  plot_Norm_vort = 
    PlotAirfoil(interpnorms, airfoilcoord, "vort_xy_plane", "xp", "yp", -20, 20, Re, AoA, "Vorticity")
  
  PlotSave(plot_Norm_Udash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_Vdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_UUmdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_VVmdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_P, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_vort, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
    ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Plotted")
  # >> Save Done ----
  ObjSave(interpnorms, plot_Norm_Udash, plot_Norm_Vdash, plot_Norm_UUmdash, plot_Norm_VVmdash, plot_Norm_P, plot_Norm_vort, 
          path = savedata, ID = ID)
  rm(xvec, dist, lvec, interpnorms, plot_Norm_Udash, plot_Norm_Vdash, plot_Norm_UUmdash, plot_Norm_VVmdash, plot_Norm_P, plot_Norm_vort)

  #--- Boundary Layer Calculations ----
  source("Function Boundary Layers.R")    # For "BLCalcs", etc
  xvec = AirfoilSamp(seq(a, a+c, by = 0.05), polyn = 5, cylinder = TRUE)
  blvals = BLCalcs(omesh, xvec, AoA, Re)
  bltheory = BLTheory(omesh, xvec, AoA, Re) %>%
    mutate(x = xO)
  blplot = bind_rows(blvals, bltheory) %>%
    arrange(method, surf, ifelse(surf == "upper", 1, -1) * x) %>%
    mutate(method = factor(method, 
      levels = c("theory", "max", "tp", "UUm", "mag"),
      labels = c("Blasius", "99% Max", "Turning Pt", "99% U'/Um'", "vel magnitude")))
  # >> Boundary Layers Calculated ----
    ThreadProgress(threadname, Re, AoA, "Boundary Layers Calculated")
  
  # INSTAED OF ADDING EACH THING SEPARATELY, CAN I GROUP THEM AND ADD THEM TOEGHETER??
  
  # Plots
  plot_bl_methods = ggplot(data = blplot) +
    geom_path(aes(x = xp, y = yp, group = method, 
                  linetype = method, colour = method, size = method)) +
    geom_polygon(data = airfoilcoord, aes(x = x, y = y), colour = "grey") +
    coord_fixed(xlim = c(-0.8, 0.6), ylim = c(-0.6, 0.6)) +
    scale_colour_discrete("BL Thickness") +
    scale_linetype_manual("BL Thickness",values = c("solid", "dashed", "dotdash", "twodash", "longdash")) +
    # scale_colour_manual("BL Thickness", values = c("grey80", "grey90", "grey40", "grey70", "grey60")) +
    scale_size_manual("BL Thickness", values = c(1.2, 1, 1, 1, 1)) + 
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Boundary Layer"))
  
  plot_bl_max = ggplot(data = filter(blplot, method %in% c("Blasius", "99% Max"))) +
    geom_path(aes(x = xp, y = yp, group = method, 
                  linetype = method, colour = method, size = method)) +
    geom_polygon(data = airfoilcoord, aes(x = x, y = y), colour = "grey") +
    coord_fixed(xlim = c(-0.8, 0.6), ylim = c(-0.6, 0.6)) +
    scale_colour_discrete("BL Thickness") +
    scale_linetype_manual("BL Thickness",values = c("solid", "dashed", "dotdash", "twodash", "longdash")) +
    # scale_colour_manual("BL Thickness", values = c("grey80", "grey90", "grey40", "grey70", "grey60")) +
    scale_size_manual("BL Thickness", values = c(1.2, 1, 1, 1, 1)) + 
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Boundary Layer"))
    # NEED TO ADD FUNCTIONALITY FOR DISP MOME ETC 
  
  plot_bl_thick = ggplot(data = filter(blplot, method %in% c("Blasius", "99% Max")),
                      aes(x = x, group = method)) +
    geom_path(aes(y = ifelse(surf == "upper", 1, -1) * thickness)) +
    geom_path(aes(y = ifelse(surf == "upper", 1, -1) * dispthick)) +
    geom_path(aes(y = ifelse(surf == "upper", 1, -1) * momethick)) +
    geom_path(aes(y = ifelse(surf == "upper", 1, -1) * kinethick)) +
    coord_fixed(xlim = c(-0.8, 0.6), ylim = c(-0.6, 0.6))
    
  plot_bl_maxlog = ggplot(data = filter(blplot, method %in% c("Blasius", "99% Max")), 
                         aes(x = (x - a + 0.001), y = dist, group = interaction(surf, method), 
                             colour = interaction(surf, method))) +
    geom_path(aes(linetype = method)) +
    geom_point(aes(pch = surf)) +
    scale_y_log10() + 
    scale_x_log10() + 
    scale_linetype_manual("BL Thickness",
                          values = c("solid", "twodash")) + 
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Boundary Layer"),
         y = "Distance", x = "Length Along Chord")
  
  PlotSave(plot_bl_methods, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_bl_max, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_bl_thick, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_bl_maxlog, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
    ThreadProgress(threadname, Re, AoA, "Boundary Layers Plotted")
  # >> Save Done ----
    ObjSave(blplot, plot_bl_methods, plot_bl_max, plot_bl_thick, plot_bl_maxlog,
          path = savedata, ID = ID)
    rm(xvec, blvals, bltheory, interpnorms, plot_bl_methods, plot_bl_max, plot_bl_thick, plot_bl_maxlog)
  
  #--- Velocity Profile Calculations ----
  sep = 0.1
  vec = VelProfileLvec(omesh, sep, blplot, AoA)
  xvec = vec$xvec; lvec = vec$lvec; distmax = vec$distmax; rm(vec)
  velprofile = VelProfile(blplot, xvec, lvec, omesh, AoA, Re)
  vptheory = Blasius(lvec, Re)
  # >> Velocity Profiles Calculated ----
    ThreadProgress(threadname, Re, AoA, "Velocity Profiles Calculated")
  
  plot_vp <- ggplot(data = velprofile, 
         aes(x = ifelse(surf == "upper", 1, -1) * dist,
             group = interaction(surf, xO))) +
    geom_ribbon(aes(ymin = x, ymax = x + UUmdash * sep / 1.5, alpha = "out")) +
    geom_ribbon(data = filter(velprofile, bl == TRUE), aes(ymin = x, ymax = x + UUmdash * sep / 1.5, alpha = "in")) +
    geom_point(aes(y = x)) +
    geom_path(data = vptheory, aes(y = x + UUmblasius * sep/1.5),
              linetype = "5111") +
    geom_path(data = filter(blplot, method %in% c("Blasius", "99% Max")), 
              aes(y = x,
                  group = interaction(method),
                  linetype = method)) +
    coord_flip(xlim = c(-distmax, distmax), ylim = c(-0.6, 0.7)) +
    scale_alpha_manual("Boundary Layer", values = c(0.3, 0.5)) +
    scale_linetype_manual("Boundary Layer", values = c("dashed", "solid")) +
    theme(aspect.ratio = 0.6) +
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Velocity Profiles"),
         x = "Distance Along the Chord", y = "Distance from Airfoil Surface")
  
  PlotSave(plot_vp, saveplot, ID, width = 8, height = 4)
  # >> Plots Done ----
    ThreadProgress(threadname, Re, AoA, "Velocity Profiles Plotted")
  # >> Save Done ----
    ObjSave(velprofile, vptheory, plot_vp,
          path = savedata, ID = ID)
    rm(sep, vec, xvec, velprofile, vptheory, plot_vp)
  
  #--- Finish Function Call ----
  space.usage <- sapply(ls(), function(x) object.size(get(x)))
  status = paste("Completed: ", sum(space.usage)/1024^2, "Mb")
  # >> Thread Completed ----
    ThreadProgress(threadname, Re, AoA, status)
  return(status)
}


