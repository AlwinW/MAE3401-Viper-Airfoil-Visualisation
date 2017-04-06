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