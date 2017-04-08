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
    select(xp, yp, xO, dist, surf, Udash, Vdash, UUmdash)
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


#--- 