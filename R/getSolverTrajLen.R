#' SolverTraj Length and Stats
#'
#' @param solvertraj 
#' @param which_one 
#'
#' @return
#' @export
#'
#' @examples
solvertraj_len = function(solvertraj, which_one){
  len_sum = 0
  len_sum_ls = c()
  resls = list()
  
  for(i in 1:length(solvertraj$iter)){
    if(i == length(solvertraj$iter)){
      break
    }
    iter_current = solvertraj[i, "iter"] #starts in 0
    iter_next = solvertraj[i+1, "iter"]
    
    y_current = solvertraj[iter_current+1, which_one]
    y_next =  solvertraj[iter_next+1, which_one]
    
    if(y_current == y_next){
      len_sum = len_sum + 1L # // 1L == iter_next - iter_current
      len_sum_ls = c(len_sum_ls, len_sum)
    } else {
      a2 = (y_current - y_next)^2
      b2 = 1L # 1^2
      c = (a2 + b2) %>% sqrt(.)
      len_sum = len_sum + c
      len_sum_ls = c(len_sum_ls, len_sum)
    }
  }
  len_sum_ls = diff(len_sum_ls) #((diff(len_sum_ls) - 1) * 1000)
  len_stats = makeStats("len_traj", len_sum_ls, stat_flag = T)

  actual_len = len_sum - (length(solvertraj$iter) - 1L) # amnt iterations 
  
  resls = list.append(resls,
                      len_solvertraj = len_sum,
                      actual_len = actual_len,
                      len_stats = len_stats)
  return(resls)
}

