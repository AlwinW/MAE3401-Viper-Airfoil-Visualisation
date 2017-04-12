#---------------------------->
#--- Thread Functions to Run
#--- Alwin Wang MAE3401
#============================>

# filename = filelist[2]; clean = FALSE

#--- Full Functionality ----
ThreadAll <- function(filename, foldername, airfoildata, savedata, saveplot, clean = TRUE) {
  #--- Set-up ----
  # Source required scripts for functions
  source("Script All Functions.R")
  source("Script Plots.R")
  # Threadname
  threadname <- ThreadName()
  
  #--- Load the Data ---
  # Load the filedata and unlist it
  filedata <- LoadFile(filename, foldername)
  list2env(filedata, envir = environment()); # rm(filedata)       # N.B: local so must be passed as fn input
  #--- >> File Data Loaded ----
  ThreadProgress(threadname, Re, AoA, "File Data Loaded
                 ---------------------------------------------------------------")
  
  #--- Run Airfoil Calculations ----
  list2env(airfoildata, envir = .GlobalEnv)                     # N.B: global so all fn can find it
  airfoilcoord <- AirfoilCoord(a, c + a, AoA, res = 100)
  #--- >> Calcs Done ----
  ThreadProgress(threadname, Re, AoA, "Aerofoil Coordinates Calculated")
  
  # Plots
  plot_airfoil_P = PlotAirfoil(omesh, airfoilcoord, "P", "x", "y", -0.2, 0.2, Re, AoA, "Pressure", rev = TRUE)
  plot_airfoil_vort = PlotAirfoil(omesh, airfoilcoord, "vort_xy_plane", "x", "y", -20, 20, Re, AoA, "Vorticity", rev = TRUE)
  
  PlotSave(plot_airfoil_P, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_airfoil_vort, saveplot, ID, width = 5, height = 4)
  #--- >> Plots Done ----
  ThreadProgress(threadname, Re, AoA, "Aerofoil Surface Values Plotted")
  
  #--- >> Save Done ----
  ObjSave(airfoilcoord, omesh,
          path = savedata, ID = ID)
  if (clean == TRUE) {
    rm(plot_airfoil_P, plot_airfoil_vort)
    invisible(gc())
  }
  
  #--- Interpolation on the airfoil----
  airfoilsurfmesh <- InterpPoint(omesh, airfoilcoord, varnames = c("P", "vort_xy_plane"))
  airfoilsurfmesh$surf = factor(airfoilsurfmesh$surf, levels = c("U", "L"),  labels = c("Upper", "Lower"))
  airfoilsurfsum <- airfoilsurfmesh %>% 
    select(-x, -y) %>%
    gather(key = key, value = value, -xO, -surf) %>%
    unite(newkey, surf, key) %>%
    spread(newkey, value) %>%
    mutate(Pnet = Lower_P - Upper_P)
  
  # >> Calcs Done ----
  ThreadProgress(threadname, Re, AoA, "Aerofoil Surface Interpolation Calculated")
  
  plot_cp = PlotCp(airfoilsurfmesh, Re, AoA)
  plot_vort = PlotVort(airfoilsurfmesh, Re, AoA)
  
  PlotSave(plot_cp, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_vort, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
  ThreadProgress(threadname, Re, AoA, "Aerofoil Cp and Vort Plotted")
  # >> Save Done ----
  ObjSave(airfoilsurfmesh, airfoilsurfsum,
          path = savedata, ID = ID)
  if (clean == TRUE) {
    rm(airfoilsurfmesh, airfoilsurfsum, plot_cp, plot_vort)
    invisible(gc())
  }
  
  #--- Interpolation on Normals ----
  xvec = AirfoilSamp(seq(a, a+c, by = 0.05), cylinder = TRUE)
  dist = NormalSamp(seq(0, 1.5, by = 0.05))
  # Find the combined lvec for interpolation
  lvec <- NormalLvec(xvec, dist, AoA)
  interpnorms <- InterpProj(omesh, lvec, plotsurf = TRUE)
  # >> Calcs Done ----
  ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Calculated")
  
  # Plots
  plot_Norm_Udash = PlotAirfoil(interpnorms, airfoilcoord, "Udash", "xp", "yp", -1.2, 1.2, Re, AoA, "U'")
  plot_Norm_Vdash = PlotAirfoil(interpnorms, airfoilcoord, "Vdash", "xp", "yp", -0.8, 0.8, Re, AoA, "V'")
  plot_Norm_UUmdash = PlotAirfoil(interpnorms, airfoilcoord, "UUmdash", "xp", "yp", -1.2, 1.2, Re, AoA, "U'/Um'")
  plot_Norm_VVmdash = PlotAirfoil(interpnorms, airfoilcoord, "VVmdash", "xp", "yp", -1.2, 1.2, Re, AoA, "V'/Vm'")
  plot_Norm_P = PlotAirfoil(interpnorms, airfoilcoord, "Vdash", "xp", "yp", -0.2, 0.2, Re, AoA, "Pressure")
  plot_Norm_vort = PlotAirfoil(interpnorms, airfoilcoord, "vort_xy_plane", "xp", "yp", -20, 20, Re, AoA, "Vorticity")
  
  PlotSave(plot_Norm_Udash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_Vdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_UUmdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_VVmdash, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_P, saveplot, ID, width = 5, height = 4)
  PlotSave(plot_Norm_vort, saveplot, ID, width = 5, height = 4)
  # >> Plots Done ----
  ThreadProgress(threadname, Re, AoA, "Interpolation on Normals to Surface Plotted")
  # >> Save Done ----
  ObjSave(interpnorms,
          path = savedata, ID = ID)
  if (clean == TRUE) {
    rm(xvec, dist, lvec, interpnorms, 
     plot_Norm_Udash, plot_Norm_Vdash, plot_Norm_UUmdash, plot_Norm_VVmdash, plot_Norm_P, plot_Norm_vort)
    invisible(gc())
  }
  
  #--- Separation and Stagnation Points ----
  xvec = AirfoilSamp(seq(a, a+c, by = 1e-3), cylinder = TRUE)
  dist = c(1e-3, 2e-3)
  # Find the combined lvec for interpolation
  lvec <- NormalLvec(xvec, dist, AoA)
  interpnorms <- InterpProj(omesh, lvec, plotsurf = TRUE) %>%
    mutate(Uclock = ifelse(surf == "upper", 1, -1) * Udash)
  
  stagnation <- interpnorms %>%
    ungroup() %>%
    filter(dist == min(dist), abs(Udash < 0.01)) %>%
    arrange(ifelse(surf == "upper", 1, -1)*xO2chord(xO, surf)) %>%
    mutate(sign = sign(Uclock * lag(Uclock, 1))) 
  stagnation = stagnation[which(stagnation$sign != 1 & !is.na(stagnation$sign)),]
  
  # ADD COMMENTS ETC
  
  plot_stag = PlotStag(interpnorms, stagnation, Re, AoA)
  PlotSave(plot_stag, saveplot, ID, width = 5, height = 4)
  
  ObjSave(stagnation,
          path = savedata, ID = ID)
  if (clean == TRUE) {
    rm(xvec, dist, lvec, interpnorms, stagnation, plot_stag)
    invisible(gc())
  }
  
  
  #--- Boundary Layer Calculations ----
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
  # # rm(xvec, blvals, bltheory, plot_bl_methods, plot_bl_max, plot_bl_thick, plot_bl_maxlog)
  
  #--- Velocity Profile Calculations ----
  sep = 0.1
  vec = VelProfileLvec(omesh, sep, blplot, AoA)
  xvec = vec$xvec; lvec = vec$lvec; distmax = vec$distmax; # rm(vec)
  velprofile = VelProfile(blplot, xvec, lvec, omesh, AoA, Re)
  vptheory = Blasius(lvec, Re)
  # >> Velocity Profiles Calculated ----
  ThreadProgress(threadname, Re, AoA, "Velocity Profiles Calculated")
  
  ashift = -0.5
  
  plot_vp <- ggplot(data = velprofile, 
                    aes(x = ifelse(surf == "upper", 1, -1) * dist,
                        group = interaction(surf, xO))) +
    # Airfoil
    geom_path(data = data.frame(x = c(0.001, 0.001, -0.001, -0.001), y = c(-0.5, 0.5, 0.5, -0.5)), 
              aes(x = x, y = y - ashift, linetype = "Aerofoil Surface"), 
              colour = "black", size = 0.5,
              inherit.aes = FALSE) +
    # Flow in BL
    geom_ribbon(aes(ymin = xO - ashift, ymax = xO + UUmdash * sep / 1.5 - ashift, alpha = "out")) +
    # Flow out of BL
    geom_ribbon(data = filter(velprofile, bl == TRUE), 
                aes(ymin = xO - ashift, ymax = xO + UUmdash * sep / 1.5 - ashift, alpha = "in")) +
    # Blasius soln for velocity profiles
    geom_path(data = vptheory, aes(y = xO + UUmblasius * sep/1.5 - ashift),
              linetype = "5111") +
    # Boundary Layers
    geom_path(data = filter(blplot, method %in% c("Blasius", "99% Max")), 
              aes(y = xO - ashift,
                  group = interaction(method),
                  colour = method)) +
    # Legends
    scale_alpha_manual("Flow", values = c(0.3, 0.1), labels = c("Boundary Layer", "Free Stream")) +
    scale_linetype_manual("Airfoil", values = c("solid"), labels = c("Surface")) + 
    # scale_linetype_manual("Boundary Layer", values = c("dashed", "solid")) +
    # Plot transformations & Labels
    coord_flip(xlim = c(-distmax, distmax), ylim = c(-0.1, 1.2)) +
    theme(aspect.ratio = 0.6) +
    labs(title = paste("Re Number", Re, "AoA", paste(AoA, "deg:", sep = ""), "Velocity Profiles"),
         x = "Distance Along the Chord", y = "Distance from the Leading Edge")
  
  PlotSave(plot_vp, saveplot, ID, width = 7, height = 4)
  # >> Plots Done ----
  ThreadProgress(threadname, Re, AoA, "Velocity Profiles Plotted")
  # >> Save Done ----
  ObjSave(velprofile, vptheory, plot_vp,
          path = savedata, ID = ID)
  # rm(sep, xvec, lvec, velprofile, vptheory, plot_vp)
  
  #--- Finish Function Call ----
  space.usage <- sapply(ls(), function(x) object.size(get(x)))
  status = paste("Completed: ", sum(space.usage)/1024^2, "Mb")
  # >> Thread Completed ----
  ThreadProgress(threadname, Re, AoA, status)
  return(status)
}
