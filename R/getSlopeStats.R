# TODO:
# put roxygen2 comments here
# @author: Jacob, Pascal, Bjoern
# @Feature_5: "SLOPE STATISTCS"
# @description: 
#   For each pair of improvements pi = (ti,fi) and pj = (tj,fj) 
#   with i < j compute the slope of the linear function through pi and pj. 
#   Let s1 ,... , sk be the actual slopes compute summary statistics.

# TODO: clarify if min slope should be -7 or 0 (*-1 or not in min and max)
# old: get_slope_stats
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
getSlopeStats = function(solver_traj){
  result = list()
  versions = list()

  stat_flag = T
  if(length(solver_traj$iter) == 1){
    message("WARNING: \n no slopes computable since trajectory has length 1.")
    stat_flag = F
    resls_INCL_ZERO = list()
    resls_EXCL_ZERO = list()
    versions = list.append(versions,
                           resls_EXCL_ZERO = resls_EXCL_ZERO,
                           resls_INCL_ZERO = resls_INCL_ZERO)
  } else {
    resls = list()
    for(i in 1:(length(solver_traj$iter)-1)){
      y1 = solver_traj[i, "incumbant"]
      x1 = solver_traj[i, "iter"]
      
      for(j in (i+1):length(solver_traj$iter)){
        y2 = solver_traj[j, "incumbant"]
        x2 = solver_traj[j, "iter"] 
        m = (y2-y1) / (x2-x1)
        name = paste("slope", "_" , i, "_", j, sep = "")
        resls[[name]] = m
      }
    }
    resls_INCL_ZERO = unlist(resls)
    # consider only "real" improvements
    tmp = resls[which(resls[] != 0)]
    resls_EXCL_ZERO = unlist(tmp)
    versions = list.append(versions,
                           resls_EXCL_ZERO = resls_EXCL_ZERO,
                           resls_INCL_ZERO = resls_INCL_ZERO)
  }
  name = "slopes"
  names_ls = list('resls_EXCL_ZERO_impro', 'resls_INCL_ZERO_impro')
  for(k in 1:length(versions)){
    slope_stat_ls = unlist(versions[[k]])
    slope_stats = makeStats(name, slope_stat_ls, stat_flag)
    name_for_result_ls = unlist(names_ls[k]) %>% as.character(.)
    result[[name_for_result_ls]] = slope_stats
  }
  return(result)
}

# @author: Jacob, Pascal, Bjoern
# @Feature_5.2: "SLOPE STATISTCS_2"
# @description: 
#   Slope of linear function through (t1,f1) and (tn,fn). 
#   Save this slope s'i for each i = 2,...,n and have a look at the curve (ti,s'i). 
#   Count the number of changes of direction, summary statistics etc.

# TODO: make statistic about direction changes
# old: get_slope_direction_stats
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
getSlopeDirectionStats = function(solver_traj){
  result = list()
  resls = list()
  stat_flat = T

  if(length(solver_traj$iter) == 1){
    message("WARNING: \n no slopes direction stats computable since trajectory has length 1.")
    Num_direction_change = 0L
    stat_flat = F
  } else {
    x2 = length(solver_traj$iter) - 1
    #<=> x2 = solver_traj[length(solver_traj$iter), "iter"]
    y2 = solver_traj[length(solver_traj$iter), "incumbant"]
    for(i in 1:(length(solver_traj$iter)-1)){
      y1 = solver_traj[i, "incumbant"]
      x1 = solver_traj[i, "iter"]
      y2 = y2
      x2 = x2
      m = (y2-y1) / (x2-x1)
      name = paste("slope", "_" , i-1, "_", x2, sep = "")
      resls[[name]] = m
    }
    resls_unls = unlist(resls)
    # assumption: only slope increases are direction changes
    Num_direction_change = 0
    for(i in 1:(length(resls_unls)-1)){
      if( (resls_unls[i] - resls_unls[i+1]) > 0 ){
        Num_direction_change = Num_direction_change + 1
      } 
    }
  }
  name = "slopes_i_n"
  slope_n_i_stat_ls = unlist(resls)
  slope_2_stats = makeStats(name, slope_n_i_stat_ls, stat_flat)

  result = list.append(result,
                       slope_2_stats = slope_2_stats,
                       Num_direction_change = Num_direction_change)
  return(result)
}

#' Title
#'
#' @param solvertraj 
#'
#' @return
#' @export
#'
#' @examples
linear_slope_analyzer = function(solvertraj){
  resls = list()
  res = list()
  
  f = function(x, m ,b){
    y = (m * x) + b
    return(y)
  }
  
  for(i in 1:9){
    res = list()
    x_rel = switch(i,
                   0.1,
                   0.2,
                   0.3,
                   0.4,
                   0.5,
                   0.6,
                   0.7,
                   0.8,
                   0.9
    ) 
    x_abs = (solvertraj[length(solvertraj[, "iter"]) ,"iter"] * x_rel) %>% round(., 0L)
    
    x1 = 0L
    x2 = x_abs
    y1 = solvertraj[1, "incumbant"]
    y2 = solvertraj[x2, "incumbant"]
    
    b = solvertraj[1, "incumbant"]
    m = (y2-y1) / (x2-x1)
    
    gret = 0L
    lt = 0L
    for(j in 1:x2){
      tmp = f(j, m, b)
      if(round(tmp, 3) <= round(res_eax$trajectory[j, "incumbant"], 3)){
        gret = gret + 1L
      } else {
        lt = lt + 1L
      }
    }
    res = list.append(res, 
                      gret = gret,
                      lt = lt)
    resls[[i]] = res
  }
  return(resls)
}









