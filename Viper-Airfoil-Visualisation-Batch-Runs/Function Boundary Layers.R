#---------------------------->
#--- Functions for Boundary Layers
#--- Alwin Wang MAE3401
#============================>

#--- Find the BL Thickness ----
# This method can be used on a single xO or a vector xvec
BLThickness <- function(omesh, lvec, varnames = c("U", "V")) {
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
    mutate(method = factor(method, levels = c("max", "tp", "UUm", "mag"))) %>%
    group_by(method, add = TRUE) %>%
    arrange(surf, xO, method)
  
  return(blthickness)
}


#--- Find the BL Values ----
# The various values of thicknesses
BLValues <- function(omesh, lvec, blthickness, varnames = c("U", "V")) {
  # Find the interpolation along the points of lvec
  interpval <- InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE) %>%
    select(xO, dist, surf, Udash, Vdash, UUmdash)
  #--- Determine the distances ----
  blvalues = list()
  for (i in 1:nrow(blthickness)) {
    # print(i)
    soln <- interpval %>%
      ungroup() %>%
      filter(xO == blthickness$xO[i],
             surf == blthickness$surf[i],
             dist <= blthickness$dist[i],
             Udash < blthickness$Udash[i] * 0.99) %>%
      slice(which.max(dist))
    
    if (length(soln$dist) != 1) {
      # HANDLES EXCEPTION WHERE SOLUTION NOT FOUND, i.e. h too big!!
      blvalues[[i]] <- data.frame(
        xO = blthickness$xO[i], dist = NA, surf = blthickness$surf[i], Udash = NA, Vdash = NA, UUmdash = NA,
        thickness = NA, dispthick = NA, momethick = NA, kinethick = NA,
        method = blthickness$method[i])
      next
    }
    
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
    
    # REPLACE with a 3/8 rule!
    distances <- (
      apply(integrand, 2, sum) -
        1/2 * integrand[1,] -
        1/2 * integrand[nrow(integrand),]) *
      soln$dist/(nrow(integrand)/2)
    
    blvalues[[i]] <- cbind(
      soln, thickness = soln$dist, distances, method = blthickness$method[i])
  }
  blvalues = bind_rows(blvalues)
  return(blvalues)
}


#--- Combine to give BL Calcs ----
BLCalcs <- function (omesh, xvec, AoA, Re, varnames = c("U", "V")) {
  # NOTE: Combine into BL Calc Laterz
  # Search using a distance step
  h1 = 0.01
  dist1 = NormalSamp(seq(0, 18, by = h1))
  lvec <- bind_rows(pblapply(
    c("upper", "lower"),
    function(surfval) bind_rows(lapply(xvec, NormalPoint, dist = dist1, AoA = AoA, surf = surfval))))
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
  lvec <- bind_rows(pblapply(
    c("upper", "lower"),
    function(surfval) bind_rows(lapply(xvec, NormalPoint, dist = dist2, AoA = AoA, surf = surfval))))
  rm(dist2)
  
  blthickness = BLThickness(omesh, lvec)
  rm(lvec)
  
  # Determine BL Thickness values
  h = 1e-3
  length.out = round(max(blthickness$dist)/h)
  length.out = length.out + (4 - length.out%% 3)
  dist = seq(0, max(blthickness$dist), length.out = length.out)
  lvec <- bind_rows(pblapply(
    c("upper", "lower"),
    function(surfval) bind_rows(lapply(xvec, NormalPoint, dist = dist, AoA = AoA, surf = surfval))))
  rm(dist)
  
  
  blvalues = BLValues(omesh, lvec, blthickness)
  
  return(blvalues)
}
