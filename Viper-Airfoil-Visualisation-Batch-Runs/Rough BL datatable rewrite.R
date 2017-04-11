
xvec = AirfoilSamp(seq(a, a+c, by = 0.5), polyn = 5, cylinder = TRUE)

BLCalcs <- function (omesh, xvec, AoA, Re, varnames = c("U", "V"), 
                     methodlevels = c("max", "tp", "UUm", "mag", "theory")) {
  # Search using a distance step
  h1 = 0.01
  dist1 = NormalSamp(seq(0, 18, by = h1))
  lvec <- NormalLvec(xvec, dist1, AoA, c("upper", "lower"))
  rm(dist1)
  # Find the first round of thicknesses
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
  # Find the second round of thicknesses
  blthickness = BLThickness(omesh, lvec)
  rm(lvec)
  
  # Determine BL Thickness values
  h = 1e-4
  dist = seq(0, max(blthickness$dist), by = h)
  lvec <- NormalLvec(xvec, dist, AoA, c("upper", "lower"))
  rm(dist)
 
}

BLThickness <- function(omesh, lvec, varnames = c("U", "V"), 
                        methodlevels = c("theory", "max", "tp", "UUm", "mag"),
                        outcols = c("xO", "dist", "surf", "Udash", "Vdash", "UUmdash")) {
  # Interpolate
  interpval <- data.table(InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE))
  # Manipulate data.table
  numericalmethod <- interpval[abs(Udash) >  1e-3, outcols, with = FALSE] %>%
    # Order the data
    # setorder(., surf, xO) %>%
    # Turning Point
    .[,tp.lag1 := lead(Udash,1) - Udash, by = c("surf", "xO")] %>%
    .[,tp.sign1 := sign(tp.lag1 * lag(tp.lag1)), by = c("surf", "xO")] %>%
    .[,tp.lag2 := tp.lag1 - lag(tp.lag1, 1), by = c("surf", "xO")] %>%
    # UUm
    .[,uum.diff := 1 - UUmdash, by = c("surf", "xO")] %>%
    .[,uum.sign := sign(lead(uum.diff,1) * uum.diff), by = c("surf", "xO")] %>%
    # Magintude of vel
    .[,mag.vel := 1 - sqrt(Udash^2 + Vdash^2), by = c("surf", "xO")] %>%
    .[,mag.sign := sign(lead(mag.vel,1) * mag.vel), by = c("surf", "xO")]
  
  blthickmax <- numericalmethod[, outcols, with = FALSE] %>%
    .[, .SD[which.max(Udash)], by = c("surf", "xO")] %>%
    .[, method := "max"]
  blthicktp <- numericalmethod[tp.sign1 == -1 & tp.lag2 < 0, outcols, with = FALSE] %>%
    .[, .SD[which.min(dist)], by = c("surf", "xO")] %>%
    .[, method := "tp"]
  blthickuum <- numericalmethod[uum.sign == -1, outcols, with = FALSE] %>%
    .[, .SD[which.min(dist)], by = c("surf", "xO")] %>%
    .[, method := "UUm"]
  
  blthickmag <- numericalmethod[mag.sign == -1, outcols, with = FALSE] %>%
    .[, .SD[which.min(dist)], by = c("surf", "xO")] %>%
    .[, method := "mag"]
  
  rm(numericalmethod)
  blthickness = rbind(blthickmax, blthicktp, blthickuum, blthickmag) %>%
    .[, method := factor(method, levels = methodlevels)] %>%
    setorder(., surf, xO)
  
  
  blthickmax <- numericalmethod[, outcols, with = FALSE] %>%
    .[, .SD[(which.max(Udash)-1):(which.max(Udash) + 1)], by = c("surf", "xO")] %>%
    .[, method := "max"]
  
  TP <- function(x, y) {
    numerator = x[1]^2*(y[2]-y[3]) + x[3]^2*(y[1]-y[2]) + x[2]^2*(y[3]-y[1])
    denominator = x[3]*(y[2]-y[1]) + x[2]*(y[1]-y[3]) + x[1]*(y[3]-y[2])
    xtp = -1/2 * numerator/denominator
    if(is.na(xtp)) xtp = x[2]
    return(xtp)
  }
  
  sapply(split(blthickmax, by = c("surf", "xO"), drop = TRUE),
         function(points) {
           xtp = TP(points$dist, points$Udash)
           skew = points$dist[2] - mean(points$dist)
           xmid = points$dist[2]
           return(data.table(xtp = xtp, xmid = xmid, skew = skew))
         }
  )
  
  
  
  return(blthickness)
}



BLValues <- function(omesh, lvec, blthickness, varnames = c("U", "V"),
                     outcols = c("xO", "dist", "surf", "Udash", "Vdash", "UUmdash")) {
  # Find the interpolation along the points of lvec
  interpval <- data.table(InterpProj(omesh, lvec, varnames = varnames, plotsurf = FALSE))
  interpval = do.call("rbind", replicate(4, interpval, simplify = FALSE)) %>%
    .[,method := rep(c("max", "tp", "UUm", "mag"), each = nrow(interpval))]
  # format(object.size(integrand), unit = "Mb")
  # Reduce the blthickness data.table to be smaller
  thickness = blthickness[, "thickness" := dist][, c("surf", "xO", "method", "thickness")]
  # Right Join with integrand
  integrand = interpval[thickness, on = c("surf", "xO", "method")] %>%
    # Filter only columns in the thickness
    .[dist < thickness]
  
  
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
