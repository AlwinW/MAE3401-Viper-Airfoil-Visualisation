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
