# TODO:
# put roxygen2 comment here
# @author:Bjoern
# @Feature_7: "CONVEX HULL RATIO"
# @description: 
#   Ratio of points on convex hull of points (ti, fi), 
#   i = 1,... , n and reference point (tn, max(fi)).

# old: convex_hull_ratio
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
getCHULLratio = function(solver_traj){
  resls = list()
  X = as.matrix(solver_traj[, c("iter", "incumbant")], ncol = 2)
  #chull(X)
  X = rbind(X, c(max(solver_traj$iter), max(solver_traj$incumbant)))
  # convex hull spanning points
  hpts = chull(X)
  # do not count the reference point (tn, max(fi)) 
  chull_spanning_points = length(hpts) - 1  
  points_on_chull = length(solver_traj$iter) - chull_spanning_points

  ratio_chull_pointsOnchull = chull_spanning_points / points_on_chull
  ratio_points_on_chull_allPoints = points_on_chull / length(solver_traj$iter) 
  ratio_chull_allPoints = chull_spanning_points / length(solver_traj$iter) 
  
  resls = list.append(resls,
                      all_points_traj = length(solver_traj$iter),
                      chull_spanning_points = chull_spanning_points,
                      points_on_chull = points_on_chull,
                      ratio_chull_pointsOnchull = ratio_chull_pointsOnchull,
                      ratio_points_on_chull_allPoints = ratio_points_on_chull_allPoints,
                      ratio_chull_allPoints = ratio_chull_allPoints
                      )
  return(resls)
}

# old: chull_plot
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
makeCHULL_plot = function(solver_traj){
  X = as.matrix(solver_traj[, c("iter", "incumbant")], ncol = 2)
  X = rbind(X, c(max(solver_traj$iter), max(solver_traj$incumbant)))

  hpts = chull(X)
  hpts = c(hpts, hpts[1])
  
  plot(X, cex = 0.5)
  lines(X[hpts, ], col = "grey")
  points(X[hpts, ], col = "magenta", pch = 13, cex=2, lwd = 2)
  points(X[-hpts, ], col = "black", cex = 0.5, pch = 16)
}


