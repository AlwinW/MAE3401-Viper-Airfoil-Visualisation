#----------------------------
#--- Functions for Airfoil Calculations
#============================

#--- Transform a set of (x,y) based on an AoA (in deg) ----
AoATransform <- function(data, AoA) {
  # Assumes X is column 1 and Y is column 2
  # Store the input data
  odata <- data
  ocolnames <- colnames(data)
  colnames(data[c(1,2)]) <- c("x", "y")
  # Apply the transformation
  data <- select(data, x, y) %>%
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