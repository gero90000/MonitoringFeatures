# TODO
# put roxygen comment here
# @author: Jacob, Pascal, Bjoern 
# @IDEA_ID_1.3: "CURVATURE: AREA BETWEEN TRAJ-CURVE AND LB"
# @description: 
#  Area between curve and some lower-bound for the tour-length, 
#  e.g., sum of MST-edges (costly), 
#        sum of |V| cheapest edges or 
#        |V| * e_cheapest.

# 1) @CALL: getFeatureSet() --> getDistanceFeatureSet() <--getDistanceFeatureSet.cpp
'// get shortest lengths
NumericVector ddd = clone(dd);
std::partial_sort(ddd.begin(), ddd.begin() + n_cities, ddd.end());
sum_of_lowest_edge_values = std::accumulate(ddd.begin(), ddd.begin() + n_cities, 0.0);'

# old: incCurve_lb_area
getIncLB_area = function(solver_traj, lb){
  resls = list()
  area_ls = list()
  stat_flag = T

  if (attr(res_eax$trajectory, 'plateaunized_called') == F){
    solver_traj = findPlat(solver_traj)
  }
  if(length(solver_traj$iter) == 1){
    cat("WARNING: since trajectory length is 1, no INC-LB area computable.")
    #area_ls = list()
    area_sum = 0L
    stat_flag = F
  } else {
    #area_ls = list()
    area = 0L
    area_sum = 0L
    for (i in 2:length(solver_traj$iter)) {
      w = 1L #res_eax$trajectory[i, "iter"] - res_eax$trajectory[i - 1, "iter"]
      h = solver_traj[i - 1, "incumbant"] - lb
      area = w * h
      area_sum = area_sum + area
      area_ls[[i-1]] = area
    }  
  }
  name = "areas"
  area_stat_ls = unlist(area_ls)
  area_stats = makeStats(name, area_stat_ls, stat_flag)
  resls = list.append(resls,
                      area_ls = area_ls,
                      area_sum = area_sum,
                      area_stats = area_stats)
  return(resls)
}

# incLB_area_plot
makeIncLB_area_plot = function(solver_traj, lb, IncLB_stat){
  ggplot(data=solver_traj, aes(x = iter, y = incumbant)) +
    geom_step() +
    geom_line(mapping = aes(x = iter, y = lb), color = "tomato", size = 2.5) +
    geom_ribbon(mapping = aes(ymin=lb, 
                              ymax=incumbant), fill="cyan", alpha=0.3)+
    ylim(c(lb/1.01, solver_traj[1, "incumbant"])) +
    annotate("text", x = (solver_traj[length(solver_traj$iter), "iter"] - solver_traj[1, "iter"]) /4 , 
             y = (lb + solver_traj[1, "incumbant"]) / 2, 
             label = paste("Sum of Area: ", round(IncLB_stat$area_sum, 2), sep = " "), 
             color = "black", size = 3) +
    ggtitle("Area between LB and Incumbent")
}





