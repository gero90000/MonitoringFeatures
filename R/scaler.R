# S3 way
# requires to apend class element as the following
# does not work so far... only when loaded into environment  :(...

#' Generic Function
#'
#' @param solvertraj 
#' @param ... 
#'
#' @return
#' @export
scalerOwn <- function (solvertraj, ...) {
  UseMethod("scalerOwn", solvertraj)
}

#' Method for res_eax Object
#'
#' @param solvertraj 
#' @param ... 
#'
#' @return
#' @export
scalerOwn.scale_multi <- function(solvertraj, ...){
  resls = list()
  ls = c("incumbant", "average.fitness")
  min_inc = min(solvertraj$incumbant)
  max_inc = max(solvertraj$incumbant)
  
  for(i in 1:length(ls)){
    tobe_scaled = ls[i]
    tmp = sapply(solvertraj[tobe_scaled], function(x){
      (x-min_inc) / (max_inc -min_inc)
    })
    resls[[i]] = tmp
  }
  #attr(solvertraj,'MIN_MAX_scaled') <- TRUE
  return(resls)
} 

#' Method for Scalar values
#'
#' @param LB 
#' @param min_inc 
#' @param max_inc 
#'
#' @return
#' @export
#'
#' @examples
scalerOwn.scale_Scalar = function(LB, min_inc, max_inc, ...){
  scaled_scalar = (LB - min_inc) / (max_inc - min_inc)
  scaled_scalar = scaled_scalar %>% as.numeric(.)
  return(scaled_scalar)
}

#' Method for res_eax Object
#'
#' @param solvertraj 
#' @param ... 
#'
#' @return
#' @export
scalerOwn.scale_other <- function(solvertraj, ...){
  res = list()
  plls = list()
  shapirols = list()
  resls = list()
  ls = c("incumbant", "average.fitness")
  
  for(i in 1:length(ls)){
    col = ls[i]
    # 1) shrink range
    tmp = sapply(solvertraj[col], function(x){
      x - min(solvertraj["incumbant"])
    })
    # 2) use natural logarithm
    tmp_2 = sapply(tmp, log)
    tmp_2[length(tmp_2)] <- 0L
    res[[i]] = tmp_2
    
    # TODO: fix
    plot_trans = plot(density(tmp_2))
    plls[[i]] = plot_trans
    
    tmp_test = shapiro.test(tmp_2)
    shapirols[[i]] <- tmp_test
  }
  
  resls = list.append(resls,
                      scales = res,
                      plots = plls,
                      shapirols = shapirols)
  
  #attr(solvertraj,'SHRINKAGE_scaled') <<- TRUE
  return(resls)
} 
