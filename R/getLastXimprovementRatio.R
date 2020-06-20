# TODO: 
# put roxygen2 comments here
# @author: Jacob, Pascal, Bjoern
# @Feature_4.1: "IMPROVEMENT STATISICS"
# @description: 
#   Ratio of improvement in last x% steps and the total improvement.

# old: last_X_improvement
getLastXimprovement = function(solver_traj, last_x_percent){
  result = list()
  resls = list()
  res = 0L

  for(i in 1:length(solver_traj$iter)){
    if(i == 1){
      resls[[i]] = 0L
    } 
    res = solver_traj[i-1, "incumbant"] - solver_traj[i, "incumbant"]
    resls[[i]] = res
  }
  resls_df = as.data.frame(unlist(resls))
  names(resls_df) = "improvement"
  total_impr = sum(resls_df$improvement)

  x = last_x_percent
  index = (length(resls_df$improvement) * (1-x)) %>% round(., 0)
  n = length(resls_df$improvement)
  if(index == n){
    last_x_impr = 0L 
  } else {
    last_x_impr = resls_df[index:n, "improvement"] %>% sum(.)
  }
  last_x_total_ratio = tryCatch(
    {
      last_x_impr / total_impr
    },
    error = function(cond) {
      message("last improvements seem to be 0")
      message(cond)
      return(0L)
    },
    finally = {}
  )
  result = list.append(result,
                       total_impr = total_impr,
                       last_x_impr = last_x_impr,
                       last_x_total_ratio = last_x_total_ratio,
                       percent_obs = x)
  return(result)
}
