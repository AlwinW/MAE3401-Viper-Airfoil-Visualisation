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
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  sapply(1:length(objects), function(i) {
    filename = paste0(folder, "/", object_names[i], ".rds")
    saveRDS(objects[i], filename)
  })
}
ObjSave(x, y, folder = folder)
# Creates a single object "X[[i]].rds"
# When I use readRDS, it gives the last object i.e. y
