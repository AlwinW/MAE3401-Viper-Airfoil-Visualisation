#---------------------------->
#--- Install Required Packages
#--- Alwin Wang MAE3401
#============================>
#--- Packages to Install ----
packages <- c(
  "parallel",
  "gridExtra",
  "dplyr",
  "purrr",
  "tidyr",
  "data.table",
  "pbapply",
  "ggplot2",
  "RColorBrewer",
  "akima"
)

#--- Install Missing Packages ----
LoadPackages <- function() {
  # Find missing packages
  newpackages <- packages[!(packages %in% installed.packages()[,"Package"])]
  # Install missing packages if need be
  if (length(newpackages))
    install.packages(newpackages)
  # Load packages
  suppressWarnings(suppressMessages(
    temp <- lapply(packages, require, character.only = TRUE)))
}

#--- Call packages to load ----
LoadPackages()
#------------------------------------------------------------------------

#---------------------------->
#--- Functions to  Load Data ----
#--- Alwin Wang MAE3401
#============================>
#--- Load a folder ----
# Allows the option of moving the folder to a new one based on date-time
LoadFolder <- function(foldername = "Input_Data", move = FALSE) {
  # Get a list of all the files in the current filder
  filelist <- list.files(foldername, pattern = "*.dat")
  # Determine if the files need to be moved
  if (move == FALSE) return(list(foldername = foldername, filelist = filelist))
  # Make a new folder
  newfolder = format(Sys.time(), "%Y-%m-%dT%H.%M.%S")
  dir.create(paste0(foldername, "/", newfolder), recursive=TRUE)
  # Determine the relative file paths
  oldpath = paste0(foldername, "/", filelist)
  newpath = paste0(foldername, "/", newfolder, "/", filelist)
  file.rename(from = oldpath,
              to = newpath)
  foldername = paste0(foldername, "/", newfolder)
  filelist <- list.files(foldername, pattern = "*.dat")
  return(list(foldername = foldername, filelist = filelist))
}

#--- Load a Single File ----
# This takes a file name and reads the file.
# It determines the start and the end point of the mesh and then
# reshapes the long format into a table of:
#   x | y | U | V | P | vort_xy_plane
LoadFile <- function(filename, foldername = NULL) {
  # Note: Future dev: time
  # Determine the metadata
  Re = as.numeric(
    unlist(strsplit(unlist(strsplit(filename, "Re"))[2], "AoA"))[1])
  AoA = as.numeric(
    unlist(strsplit(unlist(strsplit(filename, "AoA"))[2], ".dat"))[1])
  # Concatenate to find the correct filename
  if (!is.null(foldername)) {}
  filepath = paste(foldername, filename, sep = "/")
  # print(filepath)
  # Determine the starting and ending lines
  rstart = grep("DT=", readLines(filepath))[1]
  rend = grep(" 1 ", readLines(filepath))[1]
  # Read the data from the filepath given
  filedata <- read.table(filepath, sep = " ",
                         skip = rstart,
                         nrow = rend - rstart - 1)
  # Re-shape the data into correct columns
  filedata <- as.data.frame(matrix(c(t(filedata[2:6])), ncol=6))
  # Give the columns of the data appropriate names
  colnames(filedata) <- c("x", "y", "U", "V", "P", "vort_xy_plane")
  # Combind the filedata and metadata into one list
  filedata <- list(
    ID = paste0("Re", sprintf("%04d", Re), 
                "AoA", sprintf("%03d", AoA)),    
    Re = Re,
    AoA = AoA,
    filepath = filepath,
    omesh = filedata
  )
  # Return the data from the file
  return(filedata)
}


#--- Summarise Airfoil Data ----
# Given a NACA, it puts the important information into the global environment
LoadAirfoil <- function(NACA, a, c, env = FALSE) {
  # Max camber; Location of max; Thickness
  m = (NACA %/% 1000) / 100
  p = (NACA %/% 100 %% 10) / 10
  t = (NACA %% 100) / 100
  # Chord; x-shift
  c = 1
  a = - 1/2
  # Cylinder approximation of radius r and centre at (xc, yc) on the camber line
  r = 1.1019*t^2*c
  rootfind <- uniroot(
    function(x) (m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2))^2 + ((x-a)/c)^2 - r^2,
    lower = a, upper = a + p*c,
    tol = 1e-9)
  xc = rootfind$root
  yc = m/p^2 * (2*p*((xc-a)/c) - ((xc-a)/c)^2)
  # Sampling with cylinder approximiation
  thetac = atan(yc/(xc-a))      # Angle between the horizontal and raduis from (0,a) to (xc, yc)
  xsamp = xc - r*cos(3*thetac)  # No. cyl points = No. points between a and xsamp 
  # Output
  airfoildata = list(
    m = m, p = p, t = t, c = c, a = a,
    r = r,xc = xc, yc = yc, thetac = thetac, xsamp = xsamp)
  if (env == TRUE)
    list2env(airfoildata, envir = .GlobalEnv)
  return(airfoildata)
}
#------------------------------------------------------------------------

#---------------------------->
#--- Function Save Load
#--- Alwin Wang MAE3401
#============================>
#--- Save a R Data Object ----
ObjSave <- function(..., path, ID) {
  objects = list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  invisible(sapply(1:length(objects), function(i) {
    filename = paste0(path, "/", ID, "_", object_names[i], ".rds")
    saveRDS(objects[i], filename)
  }))
}
#------------------------------------------------------------------------

#---------------------------->
#--- Custom pblapply
#--- Alwin Wang MAE3401
#============================>
#--- Functions for the Thread ----
ThreadName <- function() {
  set <- getAllConnections()
  thread <-  unlist(summary.connection(set[length(set)]))[1]
  threadname = paste0(thread, sprintf("% 6d", Sys.getpid()), " :")
  threadname = gsub("->", "  ", threadname)
  return(threadname)
}

ThreadProgress <- function(threadname = "", Re, AoA, msg) {
  cat(paste(threadname, 
            paste0("Re", sprintf("% 4d", Re)),
            paste0("AoA", sprintf("% 3d", AoA)),
            format(Sys.time(), "%X"), "|", msg, "\n"))
}

#--- Custom pblapply for parallel ----
# This is the cluster case for the parallel::pblapply function
pblapplycl <- function (X, FUN, ..., cl = NULL, log = NULL, msgID = NULL, msg = NULL) 
{
  #--- Function to write to external file ----
  system = Sys.info()[4]
  PrintProgress <- function(i, B, start.t, log) {
    if (!is.null(log)) {
      # Print to console
      cat("\n")
      print(proc.time() - start.t)
      # Write to file
      cat(paste(paste0("\n", system), "Progress", round(i/B * 100, 0), "%","\n"), 
          file = log, append = TRUE)
    }
  }
  
  #--- Functions for the Thread ----
  set <- getAllConnections()
  thread <-  unlist(summary.connection(set[length(set)]))[1]
  threadname = paste(thread, sprintf("%04d", Sys.getpid()), ":")
  threadname = gsub("->", "  ", threadname)
  ThreadProgress <- function(threadname = "", msgID = "", msg, i, B, log) {
    if (!is.null(msg)) {
      cat(paste(threadname, msgID, format(Sys.time(), "%X"), "|", 
                msg, round(i/B * 100, 0), "%", "\n"),
          file = log, append = TRUE)
      
    }
  }
  
  #--- Manipulate the function ----
  # Rename the function to FUN
  FUN <- match.fun(FUN)
  # Ensure X is a list
  if (!is.vector(X) || is.object(X)) {
    X <- as.list(X)}
  
  #---- Determine the cluster type ----
  # Set the cluster if specified
  if (!is.null(cl)) {
    if (.Platform$OS.type == "windows") {
      if (!inherits(cl, "cluster")) 
        cl <- NULL
    }
    else {
      if (inherits(cl, "cluster")) {
        if (length(cl) < 2L) 
          cl <- NULL
      }
      else {
        if (cl < 2) 
          cl <- NULL
      }
    }
  }
  
  #--- Cluster Code ----
  # Get the number of times the progress bar is updated - typ 100
  nout <- as.integer(getOption("pboptions")$nout)
  # Normal lapply function
  if (is.null(cl)) {
    # Start Timing
    start.t = proc.time()
    # Print to console
    cat("\n")
    
    # No progress bar, apply a normal lapply (base)
    if (!dopb()) 
      return(lapply(X, FUN, ...))
    # Progress bar
    Split <- splitpb(length(X), 1L, nout = nout)
    B <- length(Split)
    pb <- startpb(0, B)
    on.exit(closepb(pb), add = TRUE)
    # Split the input X into components for parallel running and update the pb
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      rval[i] <- list(lapply(X[Split[[i]]], FUN, ...))
      setpb(pb, i)
      # WRITE OUT PROGRESS
      ThreadProgress(threadname, msgID, msg, i, B, log)
      print(pb)
    }
  }
  # Cluster Code
  else {
    # Start Timing
    start.t = proc.time()
    # WRITE OUT PROGRESS
    PrintProgress(0, 1, start.t, log)
    
    # Forking available on Windows
    if (inherits(cl, "cluster")) {
      # No progress bar, apply a normal parLapply (parallel)
      if (!dopb()) 
        return(parallel::parLapply(cl, X, FUN, ...))
      # Progress bar
      Split <- splitpb(length(X), length(cl), nout = nout)
      B <- length(Split)
      pb <- startpb(0, B)
      on.exit(closepb(pb), add = TRUE)
      # Split the input X into components for parallel running and update the pb
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::parLapply(cl, X[Split[[i]]], 
                                            FUN, ...))
        # Update the progress bar
        setpb(pb, i)
        # WRITE OUT PROGRESS
        PrintProgress(i, B, start.t, log)
      }
    }
    # Forking not available on windows
    else {
      # No progress bar, apply a normal mclapply (parallel)
      if (!dopb()) 
        return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
      # Progress bar
      Split <- splitpb(length(X), as.integer(cl), nout = nout)
      B <- length(Split)
      pb <- startpb(0, B)
      on.exit(closepb(pb), add = TRUE)
      # Split the input X into components for parallel running and update the pb
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::mclapply(X[Split[[i]]], 
                                           FUN, ..., mc.cores = as.integer(cl)))
        setpb(pb, i)
        # WRITE OUT PROGRESS
        PrintProgress(i, B, start.t, log)
      }
    }
  }
  # Recombine the result(s)
  rval <- do.call(c, rval, quote = TRUE)
  names(rval) <- names(X)
  # Return the result
  rval
}





# The purpose of this function is to print progress to
# an exernal file

pblapply_test <- function (X, FUN, ..., msg = NULL, cl = NULL) 
{
  set <- getAllConnections()
  thread <-  unlist(summary.connection(set[length(set)]))[1]
  thread <- paste(thread, sprintf("%04d", Sys.getpid()), ":")
  PrintThread <- function(msg) {
    cat(paste(thread, ID, format(Sys.time(), "%X"), "|", msg, "\n"))
  }
  
  # Rename the function to FUN
  FUN <- match.fun(FUN)
  # Ensure X is a list
  if (!is.vector(X) || is.object(X)) 
    X <- as.list(X)
  # Set the cluster if specified
  if (!is.null(cl)) {
    if (.Platform$OS.type == "windows") {
      if (!inherits(cl, "cluster")) 
        cl <- NULL
    }
    else {
      if (inherits(cl, "cluster")) {
        if (length(cl) < 2L) 
          cl <- NULL
      }
      else {
        if (cl < 2) 
          cl <- NULL
      }
    }
  }
  # Get the number of times the progress bar is updated - typ 100
  nout <- as.integer(getOption("pboptions")$nout)
  # Normal lapply function
  if (is.null(cl)) {
    if (!dopb()) 
      return(lapply(X, FUN, ...))
    Split <- splitpb(length(X), 1L, nout = nout)
    B <- length(Split)
    pb <- startpb(0, B)
    on.exit(closepb(pb), add = TRUE)
    rval <- vector("list", B)
    for (i in seq_len(B)) {
      rval[i] <- list(lapply(X[Split[[i]]], FUN, ...))
      setpb(pb, i)
      # WRITE OUT PROGRESS
      if (!is.null(msg)) {
        PrintThread(paste(msg, pb, "%"))
      }
      # I want to drop a line here!
    }
  }
  # Cluster Code
  else {
    if (inherits(cl, "cluster")) {
      if (!dopb()) 
        return(parallel::parLapply(cl, X, FUN, ...))
      Split <- splitpb(length(X), length(cl), nout = nout)
      B <- length(Split)
      pb <- startpb(0, B)
      on.exit(closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::parLapply(cl, X[Split[[i]]], 
                                            FUN, ...))
        setpb(pb, i)
        # WRITE OUT PROGRESS
        if (!is.null(msg)) {
          PrintThread(paste(msg, pb, "%"))
        }
      }
    }
    else {
      if (!dopb()) 
        return(parallel::mclapply(X, FUN, ..., mc.cores = as.integer(cl)))
      Split <- splitpb(length(X), as.integer(cl), nout = nout)
      B <- length(Split)
      pb <- startpb(0, B)
      on.exit(closepb(pb), add = TRUE)
      rval <- vector("list", B)
      for (i in seq_len(B)) {
        rval[i] <- list(parallel::mclapply(X[Split[[i]]], 
                                           FUN, ..., mc.cores = as.integer(cl)))
        setpb(pb, i)
        # WRITE OUT PROGRESS
        if (!is.null(msg)) {
          PrintThread(paste(msg, pb, "%"))
        }
      }
    }
  }
  rval <- do.call(c, rval, quote = TRUE)
  names(rval) <- names(X)
  rval
}
#------------------------------------------------------------------------

#---------------------------->
#--- Functions for Airfoil Calculations
#--- Alwin Wang MAE3401
#============================>
#--- Transform (x,y) based on AoA ----
# Takes the original data and assumes col 1 is x and col 2 is y
# x and y are transformed then joined back to the other original columns
AoATransform <- function(odata, AoA) {
  # Store the input data and column names
  ocolnames <- colnames(odata)
  data <- odata[c(1,2)]
  colnames(data) <- c("x", "y")
  # Apply the transformation
  data <- data %>%
    mutate(
      r = sqrt(x^2 + y^2),
      theta = atan(y/x),
      theta = ifelse(is.na(theta),0,theta),
      theta = ifelse(x < 0, theta + pi, theta),
      theta = theta - AoA*pi/180,
      x = r*cos(theta),
      y = r*sin(theta)
    ) %>%
    select(x, y)
  # Re-combine the new data with the old data and restore colnames
  odata[c(1,2)] = data
  colnames(odata) <- ocolnames
  return(odata)
}


#--- Camber and Thickness Curves ----
# Takes a given x and maps it to (x, y) on the airfoil.
# Note that these x values may be different!!
AirfoilCurve <- function(x = 0, out = "all") {
  # Test if x is within range
  on = ifelse(x >= a & x <= (a + c), 1, 0) # allows for root-finding
  # Determine the camber line yc
  yc = ifelse(x < p * c + a, 
              m/p^2 * (2*p*((x-a)/c) - ((x-a)/c)^2),
              m/(1-p)^2  * (1 - 2*p + 2*p*((x-a)/c) - ((x-a)/c)^2)
  )
  # Determine the gradient of the camber line dycdx
  dycdx = ifelse(x < p * c + a,
                 2*m/p^2 * (p - (x-a)/c),
                 2*m/(1-p)^2 * (p - (x-a)/c)
  )
  # Determine the magnitude and direction of the thickness
  theta = atan(dycdx)
  yt = 5*t*(0.2969*sqrt(abs((x-a)/c)) - 0.1260*((x-a)/c) - 0.3516*((x-a)/c)^2 +
              0.2843*((x-a)/c)^3 - 0.1036*((x-a)/c)^4)
  # Add the thickness to the camber line
  xU = x - yt*sin(theta)
  yU = yc + yt*cos(theta)
  xL = x + yt*sin(theta)
  yL = yc - yt*cos(theta)
  # Output depending on the out parameter
  if(out == "all")
    summary = data.frame(x, yc, dycdx, theta, yt,  xU, yU,  xL, yL)
  else if(out == "coord")
    summary = data.frame(x, xU, yU, xL, yL)
  else if (out == "upper")
    summary = data.frame(x = xU, y = yU)
  else if (out == "lower")
    summary = data.frame(x = xL, y = yL)
  # Return the output
  return(summary * on)
}


#--- (x, y) Airfoil Coord Given xvec ----
# Determines the airfoil coordinates on the upper and lower surfaces
# for the domain xmin to xmax
# Direction: Lower TE > Lower LE > Upper LE > Upper TE
AirfoilCoord <- function(xmin = a, xmax = c + a, AoA = 0, res = 100) {
  # Cluster points around LE and TE
  xvec = abs(a) * sin(seq(xmin, xmax, length.out = res)*pi/c)
  # Generate coordinates in a tidy format
  coord <-  AirfoilCurve(xvec, out = "coord") %>%
    rename(xO = x) %>%
    gather(key, value, -xO) %>%
    mutate(coord = substr(key,1,1), surf = substr(key, 2,2)) %>%
    select(-key) %>%
    spread(coord, value) %>%
    mutate(surf = factor(surf, levels = c("L", "U"))) %>%
    arrange(surf, xO * ifelse(surf == "U", 1, -1)) %>%
    mutate(surf = as.character(surf)) %>%
    select(x, y, surf)
  coord = AoATransform(coord, AoA = AoA)
  return(coord)
}


#--- Find xO for req xL or xU ----
# Using root finding techniques, the value of x for xO = xL, xU is found
Airfoilx <- function(xO,  surf = "upper", tol = 1e-9, out = "x") {
  # Use the rooting finding in {stats} to find the root
  rootfind <- uniroot(function(x) AirfoilCurve(x, out = surf)$x - xO,
                      lower = a, upper = a + c,
                      tol = tol)
  # Ouput depending on out parameter
  if(out == "x")
    return(rootfind$root)
  else if(out ==  "all")
    return(rootfind)
  else if(out == "str")
    return(str(rootfind))
}
#------------------------------------------------------------------------

#---------------------------->
#--- Functions for Interpolations
#--- Alwin Wang MAE3401
#============================>
#--- Interpolate at a given point ----
# This uses the interpolation function from akima: interpp
# ASSUMES that x and y points are the first two columns
InterpPoint <- function(omesh, lvec, varnames = c("U", "V", "P", "vort_xy_plane"),
                        linear = TRUE, extrap = FALSE) {
  # Loop through each variable to interpolate
  suppressWarnings(
    lmesh <- lapply(
      varnames,
      function(var) {
        interpvar <- as.data.frame(interpp(
          x = omesh$x, y = omesh$y, z = omesh[[var]],
          xo = lvec[[1]], yo = lvec[[2]],
          linear = linear,
          extrap = extrap))
        interpvar <- interpvar[3]
        colnames(interpvar) <- var
        return(interpvar)
      })
  )
  # Append these interpolations to the original data
  lmesh <- bind_cols(lvec, lmesh)
  # Return output
  return(lmesh)
}


#--- Vector Proj of Interpolation ----
InterpProj <- function(omesh, lvec, varnames = c("U", "V", "P", "vort_xy_plane"),
                       linear = TRUE, extrap = FALSE, plotsurf = FALSE) {
  # Interpolate  to find the varibles
  lmesh <- InterpPoint(omesh, lvec, varnames, linear, extrap)
  # Use vector projection parallel to the normal
  lmesh <- lmesh %>%
    mutate(surf = ifelse(plotsurf == FALSE & surf == "upper" & dely > 0, "upper", "lower")) %>%
    mutate(surf = ifelse(plotsurf == FALSE & surf == "lower" & dely < 0, "lower", "upper")) %>%
    # Um and Vm using vector projections
    mutate(Umdash = sqrt((1 - (delx)/dist^2 * delx)^2 + (-(delx)/dist^2 * dely)^2),
           Vmdash = (delx)/dist) %>%
    # Udash and Vdash found by using vector projections
    mutate(Udash = sqrt((U - (delx*U + dely*V)/dist^2 * delx)^2 + (V - (delx*U + dely*V)/dist^2 * dely)^2),
           Vdash = (delx*U + dely*V)/dist) %>%
    # Correct values at the surface
    mutate(Udash = ifelse(dist != 0, Udash, 0),
           Vdash = ifelse(dist != 0, Vdash, 0)) %>%
    mutate(Udash = ifelse(dist != 0, Udash, 0),
           Vdash = ifelse(dist != 0, Vdash, 0)) %>%
    # sign of Udash found by cross product, upper vs lower
    mutate(Udash = sign(dely*U - delx*V) * ifelse(surf == "upper", 1, -1) * Udash,
           Umdash = sign(dely) * ifelse(surf == "upper", 1, -1) * Umdash) %>%
    # Normalised velocity ratio
    mutate(UUmdash = Udash/Umdash,
           VVmdash = Vdash/Vmdash)
  
  # print(paste(lmesh$dist, lmesh$Udash))
  
  # Group lmesh by xO
  lmesh = lmesh %>%
    group_by(surf, xO, add = TRUE) %>%
    arrange(surf, xO)
  
  return(lmesh)
}



#------------------------------------------------------------------------

#---------------------------->
#--- Functions for Airfoil Normals
#--- Alwin Wang MAE3401
#============================>
#--- Creating xvec for norms ----
# Allows for better, more evened sampling of normals
# A cyclinder approximation is used for the leading edge
AirfoilSamp <- function(xvec, polyn = 3, del = c*8e-6, cylinder = FALSE) {
  # Sample according to a cubic function
  xvec = c * ((xvec - a)/c)^polyn + a
  # Add extra x values for interpolation if cylinder used
  if (cylinder != FALSE & xvec[1] == a) {
    # Determine the number of points from -theta_c to theta_c
    xadd = seq(-0.0001, -thetac,  
               length.out = ceiling(length(xvec[xvec < xsamp])/2 + 1))
    xadd = xadd[xadd != -thetac]
    # 'encode it' and combine
    xadd = a - abs(a) + xadd
    # Return the result depending on what's required
    if (cylinder == TRUE)
      xvec = c(xadd, xvec)
    if (cylinder == "only")
      xvec = xadd
  }
  # Remove any unecessary LE
  xvec = xvec[xvec != a]
  # Adjust the TE value
  if (xvec[length(xvec)] == a + c)
    xvec[length(xvec)] = a + c - sign(a + c)*abs(a + c)*del
  
  return(xvec)
}


#--- Grad: NACA ----
# Numerically finds the gradients etc using the equations for
# the airfoil
AirfoilGradNACA <- function(xO, surf, del) {
  # Determine the value of x for xO on the airfoil and neighbours
  x = Airfoilx(xO, surf = surf)
  x = c(x-del, x, x + del)
  # Determine the values
  surfval = AirfoilCurve(x, out = surf)
  # Estimate the gradients
  surfval <- mutate(surfval,
                    dydx = (y - lag(y, 1)) / (x - lag(x, 1)),
                    dydxave = (dydx + lag(dydx, 1)) / 2)
  dydx = surfval$dydxave[3]
  # Determine normal and tagential equations for output
  out <- list(out = data.frame(
    surf = surf,
    eq = c("tan", "norm"),
    # x = surfval$x[2],
    x = xO,
    y = surfval$y[2],
    m = c(dydx, -1/dydx)) %>%
      mutate(c = -m*x + y)
  )
  return(out)
}


#--- Grad: Cylinder ----
# Algebraically finds the gradients etc using the geometry of
# a cyclinder
AirfoilGradCyl <- function(xO, surf, del) {
  thetaO = xO - a + abs(a)
  thetaO = ifelse(surf == "upper", 1, -1) * thetaO
  mN = tan(thetaO)
  # Generate the output
  out <- list(out = data.frame(
    surf = surf,
    eq = c("tan", "norm"),
    x = xc -r*cos(thetaO),
    y = yc -r*sin(thetaO),
    m = c(-1/mN, mN)) %>%
      mutate(c = -m*x + y)
  )
  return(out)
}

#--- Grad: Given some x ----
# Determines the gradients etc at a point x on the airfoil
AirfoilGrads <- function(xO, surf = "upper", del = c*1e-8, out = "all") {
  # Determine which geometry to use: airfoil or cylinder
  out <- ifelse(xO < a,
                AirfoilGradCyl(xO, surf, del),
                AirfoilGradNACA(xO, surf, del))
  return(out)
}


#--- Creating dist vector for interp ----
NormalSamp <- function(dist, polyn = 3) {
  distmax = max(dist)^(polyn-1)
  dist = dist^polyn /(distmax)
  return(dist)
}


#--- (x,y) distance dist from xO ----
# Given: xO and surf
# Finds: (x, y) at a normal distance away from the surface
NormalPoint <- function(xO, dist, AoA = 0, surf = "upper", eq = "norm", gradint = NA) {
  # Find the gradient at the xO point
  gradint <- ifelse(is.na(gradint), AirfoilGrads(xO, surf = surf), gradint) 
  gradint <- gradint[[1]]
  # Note in AirfoilGrads it rootfinds for x already
  # Determine the location of (xp, yp) for a given distance
  gradint <- gradint %>%
    filter(surf == get("surf") & eq == get("eq")) %>%
    cbind(xO, ., dist) %>%
    mutate(xdist = sign(m) * dist/sqrt(1+m^2) * ifelse(surf=="upper",1,-1)) %>%
    mutate(xp = x + xdist,
           yp = y + xdist * m)
  # Transform the (xp, yp) and (x, y) coordinates and find the vector normal to the surface
  lvec <- cbind(
    AoATransform(distinct(gradint[c("xp", "yp")]), AoA),
    AoATransform(distinct(gradint[c("x", "y")]), AoA),
    gradint[c("xO", "dist", "surf", "eq")]
  ) %>%
    mutate(delx = xp - x, dely = yp - y)
  return(lvec)
}


#--- Combine x and surf into one lvec ----
NormalLvec <- function(xvec, dist, AoA = 0, 
                       surf = factor(c("upper", "lower"), levels = c("lower", "upper"))) {
  # Combine various x and surfvals efficiently using lapply and single gradint call
  suppressWarnings(
    lvec <- bind_rows(pblapply(
      surf,
      function(surfval) {
        bind_rows(lapply(xvec, NormalPoint, 
                         dist = dist, AoA = AoA, surf = surfval))
      }
    ))
  )
  return(lvec)
}
#------------------------------------------------------------------------

#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>

#--- Find the BL Thickness ----
# This method can be used on a single xO or a vector xvec
BLThickness <- function(omesh, lvec, varnames = c("U", "V"), 
                        methodlevels = c("max", "tp", "UUm", "mag", "theory")) {
  # Find the interpolation along the points of lvec
  interpval <- InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE) %>%
    select(xO, dist, surf, Udash, Vdash, UUmdash)
  #--- 100% Boundary Layer Thickness ----
  # Manipulate the data to find the boundary layer thickness
  interpval <- interpval %>%
    # Remove potentially problematic flow at the wall %>%
    filter(abs(Udash) > 1e-3) %>%
    # Turning Point
    mutate(tp.lag1 = lead(Udash, 1) - Udash) %>%
    mutate(tp.sign1 = sign(tp.lag1 * lag(tp.lag1)),
           tp.lag2 = tp.lag1 - lag(tp.lag1, 1)) %>%
    # UUm
    mutate(uum.diff = 1 - UUmdash) %>%
    mutate(uum.sign = sign(lead(uum.diff,1) * uum.diff)) %>%
    # Magintude of vel
    mutate(mag.vel = 1 - sqrt(Udash^2 + Vdash^2)) %>%
    mutate(mag.sign = sign(lead(mag.vel,1) * mag.vel))
  
  # Using the max thickness
  blthickmax <- interpval %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.max(Udash)) %>%
    mutate(method = "max")
  # Using the turning point
  blthicktp <- interpval %>%
    filter(tp.sign1 == -1, tp.lag2 < 0) %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.min(dist))%>%
    mutate(method = "tp")
  # Using Uum equal to 1
  blthickuum <- interpval %>%
    filter(uum.sign == -1) %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.min(dist))%>%
    mutate(method = "UUm")
  # Using the magnitude of the velocity
  blthickmag <- interpval %>%
    filter(mag.sign == -1) %>%
    select(xO, dist, surf, Udash, UUmdash) %>%
    slice(which.min(dist))%>%
    mutate(method = "mag")
  
  # Remove the object to conserve RAM
  rm(interpval)
  
  # Output of thicknesses (100%)
  blthickness = rbind(blthickmax, blthicktp, blthickuum, blthickmag) %>%
    mutate(method = factor(method, levels = methodlevels)) %>%
    group_by(method, add = TRUE) %>%
    arrange(surf, xO, method)
  
  return(blthickness)
}


#--- Find the BL Values ----
# The various values of thicknesses
BLValues <- function(omesh, lvec, blthickness, varnames = c("U", "V")) {
  # Find the interpolation along the points of lvec
  interpval <- InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE) %>%
    select(xp, yp, x, xO, dist, surf, Udash, Vdash, UUmdash)
  #--- Determine the distances ----
  blvals = list()
  for (i in 1:nrow(blthickness)) {
    # print(i)
    soln <- interpval %>%
      ungroup() %>%
      filter(xO == blthickness$xO[i],
             surf == blthickness$surf[i],
             dist <= blthickness$dist[i],
             Udash < blthickness$Udash[i] * 0.99) %>%
      slice(which.max(dist))
    
    # HANDLES EXCEPTION WHERE SOLUTION NOT FOUND, i.e. h too big!!
    if (length(soln$dist) != 1) next
    
    # Determinet the integrand for the integration
    integrand <- interpval %>%
      ungroup() %>%
      filter(xO == blthickness$xO[i],
             surf == blthickness$surf[i],
             dist <= soln$dist) %>%
      mutate(Ur = Udash / soln$Udash,
             dispthick = 1 - Ur,
             momethick = Ur * (1 - Ur),
             kinethick = Ur * (1 - (Ur)^2)) %>%
      select(dispthick, momethick, kinethick)
    
    # Values for numerical integration
    n = nrow(integrand)
    h = soln$dist/n     # NOTE: Clearly the input lvec must have had an equally spaced lvec
    
    if (n >= 2) { # Enough points?
      n38 = (n - 1) %/% 3 # Number of 3/8 rules to be applied
      ntr = (n - 1) %% 3  # Number of trap rules to be applied (at the end)
      
      # Trap Rule
      nptr = ntr
      integrandtr = integrand[1:(nptr + 1),]
      distancestr = 1/2 * h *
        (2 * apply(integrandtr, 2, sum) - 
           integrandtr[1,] -
           integrandtr[ntr + 1,])
      # 3/8 Rule
      np38 = (3*n38 + 1)  # Number of points in the integrand for 3/8
      integrand38 = integrand[(nptr + 1):(nptr + np38),]
      distances38 = 3/8 * h * 
        (3 * apply(integrand38, 2, sum) -
           apply(integrand38[rep(c(TRUE, FALSE, FALSE), length.out = np38),], 2, sum) -
           integrand38[1,] -
           integrand38[np38,])
      # Sum the distances together
      distances = distances38 + distancestr
    } else { # Not enough points
      distances = data.frame(
        dispthick = 0, momethick = 0, kinethick = 0
      )
    }
    
    # Calculate the xp and yp
    
    blvals[[i]] <- cbind(
      soln, thickness = soln$dist, distances, method = blthickness$method[i])
  }
  blvals = bind_rows(blvals)
  return(blvals)
}


#--- Combine to give BL Calcs ----
# This gives the thickness etc of a boundary layer
BLCalcs <- function (omesh, xvec, AoA, Re, varnames = c("U", "V"), 
                     methodlevels = c("max", "tp", "UUm", "mag", "theory")) {
  # NOTE: Combine into BL Calc Laterz
  # Search using a distance step
  h1 = 0.01
  dist1 = NormalSamp(seq(0, 18, by = h1))
  lvec <- NormalLvec(xvec, dist1, AoA, c("upper", "lower"))
  rm(dist1)
  
  blthickness = BLThickness(omesh, lvec)
  rm(lvec)
  
  # Search along a smaller distance step
  h2 = 2e-5
  dist2 = seq(min(blthickness$dist) - h1, max(blthickness$dist)  + h1, by = h2)
  dom <- as.matrix(sapply(blthickness$dist,
                          function(dist) dist2 > (dist - h1) & dist2 < (dist + h1)))
  dom = apply(dom, 1, sum)
  dom = ifelse(dom != 0, TRUE, FALSE)
  dist2 = dist2[dom]
  lvec <- NormalLvec(xvec, dist2, AoA, c("upper", "lower"))
  rm(dist2)
  
  blthickness = BLThickness(omesh, lvec)
  rm(lvec)
  
  # Determine BL Thickness values
  h = 1e-4
  dist = seq(0, max(blthickness$dist), by = h)
  lvec <- NormalLvec(xvec, dist, AoA, c("upper", "lower"))
  rm(dist)
  
  blvals = BLValues(omesh, lvec, blthickness)
  
  return(blvals)
}


#--- Theoretical Distance ----
# Find the theoretical distance
BLTheory <- function(omesh, xvec, AoA, Re, varnames = c("U", "V"), 
                     surf = factor(c("upper", "lower"), levels = c("lower", "upper")), 
                     methodlevels = c("max", "tp", "UUm", "mag", "theory")) {
  # Remove x values from a cylindrical approximation
  xvec = xvec[xvec > a & xvec < a + c]
  # Determine the points for the theoretical distances
  lvec <-suppressWarnings(
    lvec <- bind_rows(pblapply(surf,
                               function(surfval) {
                                 bind_rows(lapply(xvec,
                                                  function(x) {
                                                    dist = 5 * (x - a) / sqrt(Re * (x - a))
                                                    NormalPoint(x, dist, AoA, surf = surfval)
                                                  }))
                               })))
  
  interpval <- InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE) %>%
    select(xp, yp, xO, dist, surf, Udash, Vdash, UUmdash)
  
  bltheory = data.frame(
    interpval, thickness = interpval$dist, method = factor("theory", levels = methodlevels)
  )
}


#--- Velocity Profile Points ----
VelProfileLvec <- function(omesh, sep, blvals, AoA = 0, length.out = 100,
                           surf = factor(c("upper", "lower"), levels = c("lower", "upper"))) {
  # Domain for plotting
  xvec = c(-0.497, seq(a + sep, a + c - sep, by = sep), 0.499)
  distmax = max(blvals$dist[blvals$xO == max(blvals$xO)]) * 1.2
  # lvec for interpolation
  lvec <- NormalLvec(xvec, NormalSamp(seq(0, distmax, length.out = length.out)), AoA, surf)
  return(list(distmax = distmax, xvec = xvec, lvec = lvec))
}


#--- Velocity Profile Calculations ----
VelProfile <- function(blvals, xvec, lvec, omesh, AoA = 0, Re, method = "max") {
  # Results (interpolation)
  velprofile <- InterpProj(omesh, lvec, linear = TRUE, plotsurf = FALSE)
  # Determine which part(s) are in the boundary layer
  vpblvals = BLCalcs(omesh, xvec, AoA, Re)
  vpbl = filter(vpblvals, method == method) %>%
    select(xO, surf, thickness) ## I SHOULD ADD THIS ITO THE VELPROFILE I SO GET THE EXACT CROSSIGN POINT
  velprofile <- right_join(velprofile, vpbl, by = c("xO", "surf")) %>%
    mutate(bl = (dist <= thickness))
  # Return the output
  return(velprofile)
}

Blasius <- function(lvec, Re) {
  # Blasius Soln
  blasius <- data.frame(
    eta = seq(0, 8, by = 0.5),
    uU = c(0.000, 0.1659, 0.3298, 0.4868, 0.6298, 0.7513, 0.8461, 0.9131, 0.9555,
           0.9795, 0.9916, 0.9969, 0.9990, 0.9997, 0.9999, 1.000, 1.000))
  # Determine eta
  lvec <- lvec %>%
    mutate(eta = sqrt(Re * (x - a)) * dist/(x - a))
  # Caculate Blasius for the various points
  vptheory <- as.data.frame(spline(x = blasius$eta, y = blasius$uU, xout = lvec$eta))
  vptheory <- data.frame(lvec, UUmblasius = vptheory$y) %>%
    mutate(UUmblasius = ifelse(eta >= 8, 1, UUmblasius))
  return(vptheory)
}
#------------------------------------------------------------------------

