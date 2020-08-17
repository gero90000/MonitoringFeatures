#' Creates Memory of original values of solver object
#'
#' @param solvertraj 
#' @param classAdd 
#'
#' @return
#' @export
#'
#' @examples
min_max_memory <- function(solvertraj, classAdd){
  # Get the environment for this nstance of the function.
  thisEnv <- environment()
  min_inc <- min(solvertraj$incumbant)
  max_inc <- max(solvertraj$incumbant)
  
  # Create the list used to represent an object for this class
  solver_spec <- list(
    # Define the environment where this list is defined so it can ber reffered to later
    thisEnv = thisEnv,

    ## getter and setter
    getEnv = function(){
      return(get("thisEnv", thisEnv))
    },
    getMin = function(){
      return(get("min_inc",thisEnv))
    },
    getMax = function(){
      return(get("max_inc",thisEnv))
    }
  )
  # Define the value of the list within the current environment.
  # assign function definition
  '
  assign(x, value, pos = -1, envir = as.environment(pos),
  inherits = FALSE, immediate = TRUE)
  '
  assign('this', solver_spec, envir=thisEnv)
  ## Set the name for the class
  class(solver_spec) <- append(class(solver_spec), classAdd)
  return(solver_spec)
}




