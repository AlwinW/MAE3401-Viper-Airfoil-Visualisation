#---------------------------->
#--- Custom pblapply
#--- Alwin Wang MAE3401
#============================>

#--- Functions for the Thread ----
set <- getAllConnections()
thread <-  unlist(summary.connection(set[length(set)]))[1]
thread <- paste(thread, sprintf("%04d", Sys.getpid()), ":")
PrintThread <- function(msg, thread = thread, ID = ID) {
  cat(paste(thread, ID, format(Sys.time(), "%X"), "|", msg, "\n"))
}

# The purpose of this function is to print progress to
# an exernal file

pblapply <- function (X, FUN, ..., msg = NULL, cl = NULL) 
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