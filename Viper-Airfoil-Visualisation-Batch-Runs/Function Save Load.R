#---------------------------->
#--- Function Save Load
#--- Alwin Wang MAE3401
#============================>


#--- Save a R Data Object ----
ObjSave <- function(..., path, ID) {
  silent = lapply(list(...),
                  function(object) {
                    filename = paste0(path, "/", ID, "_", object, ".rds")
                    saveRDS(object, filename)
         })
}
