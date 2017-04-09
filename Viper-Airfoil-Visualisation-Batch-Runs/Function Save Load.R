#---------------------------->
#--- Function Save Load
#--- Alwin Wang MAE3401
#============================>


#--- Save a R Data Object ----
ObjSave <- function(..., path, ID) {
  silent = lapply(
    as.list(substitute(list(...)))[-1L],
    function(object) {
      filename = paste0(path, "/", ID, "_", deparse(substitute(object)), ".rds")
      saveRDS(object, filename)
         })
}

# Objects
x = 1:10
y = letters[1:10]
# Save location
folder = "Output_Data"

# Save a single object
ObjSave <- function(object, folder) {
  filename = paste0(folder, "/", deparse(substitute(object)), ".rds")
  saveRDS(object, filename)
}
ObjSave(x, folder)  # Works fine. Output: x.rds

# Save multiple objects
ObjSave <- function(..., folder) {
  l = list(...)
  names(l) <- as.character(substitute(list(...)))[-1L]
  invisible(lapply(
    list(...),
    function(object) {
    filename = paste0(folder, "/", deparse(substitute(object)), ".rds")
    saveRDS(object, filename)}
  ))
}
ObjSave(x, y, folder = folder)
# Creates a single object "X[[i]].rds"
# When I use readRDS, it gives the last object i.e. y
