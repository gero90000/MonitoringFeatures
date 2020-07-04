# TODO: 
# put roxygen2 comments here
# @author: Jacob, Pascal, Bjoern
# @Feature_4.1: "IMPROVEMENT STATISICS"
# @description: 
#   Ratio of improvement in last x% steps and the total improvement.

# old: last_X_improvement
# TODO: paramterize with "backwards" = T/F and sum up from beginning or end accordingly 
getLastXimprovement = function(solver_traj, p_percent_observation=NULL, backwards=TRUE, from_to = FALSE, from=NULL, to=NULL){
  result = list()
  resls = list()
  res = 0L

  for(i in 1:length(solver_traj$iter)){
    if(i == 1L){
      resls[[i]] = 0L
    } 
    res = solver_traj[i-1, "incumbant"] - solver_traj[i, "incumbant"]
    resls[[i]] = res
  }
  resls_df = as.data.frame(unlist(resls))
  names(resls_df) = "improvement"
  total_impr = sum(resls_df$improvement)

  if(from_to == FALSE){
    if(!is.null(p_percent_observation)){
      if(backwards == TRUE){ # in case we want to look at the last p%
        index = (length(resls_df$improvement) * (1-p_percent_observation)) %>% round(., 0L)
        n = length(resls_df$improvement)
        if(index == n){
          obs_p_impr = 0L 
        } else {
          obs_p_impr = resls_df[index:n, "improvement"] %>% sum(.)
        }
      } else { # in case we want to look at the first p%
        index = (length(resls_df$improvement) * p_percent_observation) %>% round(., 0L)
        obs_p_impr = resls_df[1:index, "improvement"] %>% sum(.)
      }
    } else {
      message("WARNING: p_percent_observation parameter has not been set! --> Return NA")
      return(NA)
    }
  } else { # in case we want to look from p% to q%
    if(!is.null(from) & !is.null(to)){
      index_from = (length(resls_df$improvement) * from) %>% round(., 0L)
      index_to = (length(resls_df$improvement) * to) %>% round(., 0L)

      if(index_from <= index_to){
        obs_p_impr = resls_df[index_from:index_to, "improvement"] %>% sum(.)
      } else {
          message("WARNING: parameter from must be equal or less than to! --> Return NA")
          return(NA)
      }
    } else{
        message("WARNING: set from and to parameter! --> Return NA")
        return(NA)
    }
  }
  p_obs_total_ratio = tryCatch(
    {
      obs_p_impr / total_impr
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
                       obs_p_impr = obs_p_impr,
                       p_obs_total_ratio = p_obs_total_ratio,
                       percent_obs = p_percent_observation,
                       backwards = backwards,
                       from_to = from_to, 
                       from = from, 
                       to = to)
  return(result)
}
