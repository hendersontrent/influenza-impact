#' Function for removing all objects except those designated as 'keepers'
#' 
#' @param keepers objects not to removes
#' @param verbose logical; whether or not to print a message listing all the objects being removed
#' @author Peter Ellis; this is a fork of a function originally in the \code{frs} (Free Range Statistics) package.
#' @export
cleanup_env <- function(keepers = NULL, verbose = FALSE){
  objs <- ls(pos = 1)
  objs <- objs[!objs %in% keepers]
  if(verbose){
    message(paste("Removing:", paste(objs, collapse = ", ")))  
  }
  rm(list = objs, pos = 1)
}