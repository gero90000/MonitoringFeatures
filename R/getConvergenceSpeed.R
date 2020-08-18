# TODO:
# put roxygen2 comment here 
# @author: Bjoern 
# @IDEA_ID_1.1: "CONVERGENCE SPEED"
# @description: 
#  area between incumbent lower bound and avg fitness upper bound 
#  over trajectory --> exhibit %-wise contribution per rectangle

#' Title
#'
#' @param solver_traj 
#' @param growth 
#'
#' @return
#' @export
#'
#' @examples
getAreaAVGInc = function(solver_traj, growth){
  solver_traj$area_between_AVGfit_Inc = NA
  solver_traj$area_between_AVGfit_Inc_iter = NA
  if(growth == TRUE){
    solver_traj$area_between_AVGfit_Inc_growth = NA
    solver_traj$area_between_AVGfit_Inc_iter_growth = NA
  }
  area_ft = 0L
  dec_plc = 3L
  
  if(length(solver_traj$iter) == 1L){
    message("WARNING: \n No Areas between Incumnbent and \n 
             AVG fitness of population computable due to trajectory length.")
    attr(solver_traj,'area_INC_AVG') <- FALSE
  } else {
    for (i in 2L:length(solver_traj$iter)) {
        width_t = solver_traj[i, "time.passed"] - solver_traj[i - 1L, "time.passed"]
        width_t_it = solver_traj[i, "iter"] - solver_traj[i - 1L, "iter"]
        height_f = solver_traj[i - 1L, "average.fitness"] - solver_traj[i - 1L, "incumbant"]
        height_f_it = solver_traj[i - 1L, "average.fitness"] - solver_traj[i - 1L, "incumbant"]
      
        area_ft = width_t * height_f
        area_ft_it = width_t_it * height_f_it
        area_ft = area_ft %>% round(., dec_plc)
        area_ft_it = area_ft_it %>% round(., dec_plc)
        
        solver_traj[i, "area_between_AVGfit_Inc"] = area_ft
        solver_traj[i, "area_between_AVGfit_Inc_iter"] = area_ft_it
        if(i == 2L){
          next
        } else if (growth == TRUE) {
          solver_traj[i, "area_between_AVGfit_Inc_growth"] = 
            (area_ft / solver_traj[i - 1L, "area_between_AVGfit_Inc"]) %>% round(., dec_plc)
          solver_traj[i, "area_between_AVGfit_Inc_iter_growth"] = 
            (area_ft_it / solver_traj[i - 1L, "area_between_AVGfit_Inc_iter"]) %>% round(., dec_plc)
      }
    }
    attr(solver_traj,'area_INC_AVG') <- TRUE
  }
  return(solver_traj)
}

# TODO: add the simple convergence Speed idea to ConvSpeed_1:
#       (first_dist - last_dist) / distance  
#' Title
#'
#' @param solver_traj 
#' @param timebased 
#'
#' @return
#' @export
#'
#' @examples
getConvSpeed_1 = function(solver_traj, timebased){
  convIter = 0L
  convIter_2 = 0L 
  convTime = 0L
  convTime_2 = 0L

  area_improvement_iter = 0L
  area_improvement_time = 0L

  rect_stats_iter = NA
  rect_stats_time = NA

  resls = list()
  if(attr(solver_traj,'area_INC_AVG') == T){
    n = solver_traj %>% .[length(.$iter), "iter"] #length(solver_traj$iter) - 1L
    firstAreaVal = solver_traj[2L, "area_between_AVGfit_Inc_iter"]
    lastAreaVal = solver_traj[n, "area_between_AVGfit_Inc_iter"]

    convIter = ((lastAreaVal / firstAreaVal) / n) 
    convIter_2 = ((firstAreaVal - lastAreaVal) / n)                 # +++++ new ++++++ 
    area_improvement_iter = 1L - (lastAreaVal / firstAreaVal)

    name = "rectangle_iter"
    rect_stat_ls = unlist(solver_traj$area_between_AVGfit_Inc_iter) %>% .[!is.na(.)]
    rect_stats_iter = makeStats(name, rect_stat_ls, stat_flag = T)

    if(timebased == TRUE){
      firstAreaVal = solver_traj[2L, "area_between_AVGfit_Inc"]
      lastAreaVal = solver_traj[n, "area_between_AVGfit_Inc"]
      convTime = ((lastAreaVal / firstAreaVal) / 5L)  #n            # divided by n --> not to useful, use cutoff time instead
      convTime_2 = ((firstAreaVal - lastAreaVal) / 5L) #n  
      area_improvement_time = 1 - (lastAreaVal / firstAreaVal)

      name = "rectangle_time"
      rect_stat_ls = unlist(solver_traj$area_between_AVGfit_Inc_iter) %>% .[!is.na(.)]
      rect_stats_time = makeStats(name, rect_stat_ls, stat_flag = T)
    } else {
      name = "rectangle_time"
      rect_stat_ls = unlist(solver_traj$area_between_AVGfit_Inc_iter) %>% .[!is.na(.)]
      rect_stats_time = makeStats(name, rect_stat_ls, stat_flag = F)
    }
    resls= list.append(resls, 
                       convIter = convIter,
                       convIter_2 = convIter_2,
                       convTime = convTime,
                       convTime_2 = convTime_2, 

                       area_improvement_iter = area_improvement_iter,  # TODO change name to relative not area as total
                       area_improvement_time = area_improvement_time,

                       rect_stats_iter = rect_stats_iter,
                       rect_stats_time = rect_stats_time
                       )
  } else {
    message("WARNING: \n No ConvSpeed_1 data since no Area between Incumnbent and AVG fitness of poulation")
    resls= list.append(resls, 
                       convIter = convIter,
                       convIter_2 = convIter_2,
                       convTime = convTime,
                       convTime_2 = convTime_2,

                       area_improvement_iter = area_improvement_iter,
                       area_improvement_time = area_improvement_time,

                       rect_stats_iter = rect_stats_iter,
                       rect_stats_time = rect_stats_time
                       )
  }
  return(resls)
}

# @author: Bjoern 
# @IDEA_ID_1.2: "CONVERGENCE SPEED"
# @description: 
#  create triangle, by taking triangle side as f_avg(1) - f_inc(1) and peak by 
#  centroid = (f_avg(n) - f_inc(n)) / 2
#  take upper and lower triangle side by f_avg(1)-centroid and f_inc(1)-centroid edge
#  get convergence measure by dividing triangle area by idealized trapezoid or rectangle area 

# old: calc_areas
#' Title
#'
#' @param solver_traj 
#' @param triangle 
#' @param trapezoid 
#'
#' @return
#' @export
#'
#' @examples
calcTrigonometricAreas = function(solver_traj, triangle, trapezoid){
  areals = list()

  if(length(solver_traj$iter) == 1L){
    message("WARNING: \n No Polygons in trajectory due to trajectoy length.")
    #attr(solver_traj,'Trigonometrics') <- FALSE
    setattr(solver_traj,"trigonometrics", FALSE)
    areals = list.append(areals, 
                         triangle = 0L, 
                         tri_peak = 0L, 
                         tri_sideA= 0L,
                         tri_sideB = 0L, 
                         tri_sideC = 0L,
                         trapezoid = 0L)
  } else {
    n = length(solver_traj$iter)
    if(triangle == TRUE){
      tri_peak = solver_traj[n, "incumbant"] + (solver_traj[n, "average.fitness"] - 
                                                  solver_traj[n, "incumbant"]) / 2
      tri_sideA = solver_traj[1L, "average.fitness"] - solver_traj[1L, "incumbant"]
      tri_sideB =  ((n - 1)^2 + (solver_traj[1L, "average.fitness"] - 
                                   tri_peak)^2) %>% sqrt(.)
      tri_sideC = if(tri_peak > solver_traj[1L, "incumbant"]) {
        ((n - 1)^2 + (tri_peak - solver_traj[1L, "incumbant"])^2) %>% sqrt(.)
      } else {
        ((n - 1)^2 + (solver_traj[1, "incumbant"] - tri_peak)^2) %>% sqrt(.)
      }
      # AREA TRIANGLE
      s = (tri_sideA + tri_sideB + tri_sideC) / 2L
      tri_area = (s * (s-tri_sideA) * (s-tri_sideB) * (s-tri_sideC)) %>% sqrt(.)
      areals = list.append(areals, 
                           triangle = tri_area, 
                           tri_peak = tri_peak, 
                           tri_sideA= tri_sideA,
                           tri_sideB = tri_sideB, 
                           tri_sideC = tri_sideC)
    }
    if(trapezoid == TRUE){
      # AREA TRAPEZOID
      trap_area = (((solver_traj[1, "average.fitness"] - solver_traj[1, "incumbant"]) +
                      (solver_traj[n, "average.fitness"] - 
                         solver_traj[n, "incumbant"])) / 2) * (n - 1)
      areals = list.append(areals, trapezoid = trap_area)
    }
    #attr(solver_traj,'Trigonometrics') <- TRUE
    setattr(solver_traj,"trigonometrics", TRUE)  # by reference
  }
  return(areals)
}

#' Title
#'
#' @param solver_traj 
#' @param tri_area 
#' @param trap_area 
#'
#' @return
#' @export
#'
#' @examples
getConvSpeed_2 = function(solver_traj, tri_area, trap_area){
  resls = list()

  if(attr(solver_traj,'trigonometrics') == T){
    convergence_idealized = tri_area / trap_area
    convergence_idealized_norm = (convergence_idealized - 0.5) / (1-0.5)
    convergence_step_tri = tri_area / sum(solver_traj$area_between_AVGfit_Inc_iter, na.rm = TRUE)
  } else {
    message("WARNING: \n No ConvSpeed_2 data since trajectory does not exhibit trigonometrics (due to length).")
    convergence_idealized = 0L
    convergence_idealized_norm = 0L
    convergence_step_tri = 0L
  }
  resls = list.append(resls, 
                      convergence_idealized = convergence_idealized,
                      convergence_idealized_norm = convergence_idealized_norm,
                      convergence_step_tri = convergence_step_tri)
  return(resls)
}

# +++ new +++
#' Title
#'
#' @param solvertraj 
#'
#' @return
#' @export
#'
#' @examples
getConvSpeed_3 = function(solvertraj){
  
  # TODO: not needed anymore when 
  normalize_y = function(x, solvertraj, Y){
    normalizer = base::mean(solvertraj[, Y])
    x_norm = x / 1L #normalizer
    return(x_norm)
  }

  resls = list()
  res = list()
  ls = list("incumbant", "average.fitness")

  for(i in 1:length(ls)){
    
    y_type = unlist(ls[i]) %>% as.character(.)
    y1 = (solvertraj %>% .[1, y_type]) %>% normalize_y(., solvertraj, y_type)
    y2 = (solvertraj %>% .[length(.$iter), y_type]) %>% normalize_y(., solvertraj, y_type)
    print(y1)
    print(y2)

    # use logarithm to shrink the x range adequately, since y value is normalized
    x1 = 0L
    x2 = (solvertraj %>% .[length(.$iter), "iter"]) #%>% log10(.)
    print(x2)
    
    m = (y2-y1) / (x2-x1)
    res[y_type] = m
  }
  
  conv_speed = as.double(res["average.fitness"]) / as.double(res["incumbant"])
  
  resls = list.append(resls,
                      conv_speed = conv_speed,
                      slopes = res)
  return(resls)
}

# TODO: incorporate "Knees" (cf. Feature list)
# old: convQuality
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
getConvQuality = function(solver_traj){
  resls = list()
  
  if(length(solver_traj$iter) == 1){
    message("WARNING: No quality data because of trajectory length.")
    dist_begin = 0L
    dist_end = 0L
    quality_drift = 0L
  } else {
    n = length(solver_traj$iter)
    highest_AVG_iter = solver_traj[1, "average.fitness"]
    lowest_AVG_iter = solver_traj[n, "average.fitness"]
    highest_Inc_iter = solver_traj[1, "incumbant"]
    lowest_Inc_iter = solver_traj[n, "incumbant"]
    
    dist_begin = highest_Inc_iter / highest_AVG_iter 
    dist_end = lowest_Inc_iter / lowest_AVG_iter 
    quality_drift = dist_end - dist_begin
  }
  resls = list.append(resls, 
                      dist_begin = dist_begin,
                      dist_end = dist_end,
                      quality_drift = quality_drift)
  return(resls)
}

# old: convspeed_2_plot
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
makeConvSpeed_2_plot = function(solver_traj){
  areasls = calcTrigonometricAreas(solver_traj, TRUE, TRUE)
  tri_peak =   areasls$tri_peak
  tri_sideA = areasls$tri_sideA
  tri_sideB =  areasls$tri_sideB
  tri_sideC = areasls$tri_sideC
  
  slope_AVG = (solver_traj[length(solver_traj$iter), "average.fitness"] - 
                 solver_traj[1, "average.fitness"]) / ((length(solver_traj$iter)-1) - 0) 
  slope_INC = (solver_traj[length(solver_traj$iter), "incumbant"] - 
                 solver_traj[1, "incumbant"]) / ((length(solver_traj$iter)-1) - 0) 
  slope_TRI1 = (tri_peak - solver_traj[1, "average.fitness"]) / 
    ((length(solver_traj$iter)-1) - 0) 
  slope_TRI2 = (tri_peak - solver_traj[1, "incumbant"]) / 
    ((length(solver_traj$iter)-1) - 0) 
  #build tri polygon (all 3 position coordinates)
  tri_poly = data.frame(x=c(0,
                            0,
                            length(solver_traj$iter) - 1), 
                        y=c(solver_traj[1, "average.fitness"],
                            solver_traj[1, "incumbant"], 
                            tri_peak))
  ggplot(data=solver_traj) +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "blue") +
    geom_step(mapping=aes(x=iter, y=average.fitness), color = "tomato") +
    ggtitle("AVG fitness vs. Incumbent") +
    geom_abline(intercept = solver_traj[1, "average.fitness"], 
                slope = slope_AVG, color="tomato", 
                linetype="dashed", size=0.6) +
    geom_abline(intercept = solver_traj[1, "incumbant"], 
                slope = slope_INC, color="blue", 
                linetype="dashed", size=0.6) +
    geom_abline(intercept = solver_traj[1, "average.fitness"], 
                slope = slope_TRI1, color="black", size=0.3) +
    geom_abline(intercept = solver_traj[1, "incumbant"], 
                slope = slope_TRI2, color="black", size=0.3) + 
    geom_point(mapping = aes(x = (length(iter)-1)-0, y = tri_peak), 
               shape = 10, size = 4, color = "brown") +
    geom_point(mapping = aes(x = 0, y = solver_traj[1, "incumbant"]), 
               shape = 10, size = 4, color = "brown") +
    geom_point(mapping = aes(x = 0, y = solver_traj[1, "average.fitness"]), 
               shape = 10, size = 4, color = "brown") +
    geom_polygon(data=tri_poly, aes(x, y), fill="cyan", alpha = 0.3) +
    geom_segment(aes(x = 0, y = solver_traj[1, "incumbant"], 
                     xend = 0, yend = solver_traj[1, "average.fitness"]), 
                     color = "black", size = 0.3) +
    geom_segment(aes(x = 0, y = solver_traj[1, "incumbant"], 
                     xend = 0, yend = solver_traj[1, "average.fitness"]), 
                 color = "black", linetype = "dotted") +
    geom_segment(aes(x = length(iter) - 1, 
                     y = solver_traj[length(iter), "incumbant"], 
                     xend = length(iter) - 1, 
                     yend = solver_traj[length(iter), "average.fitness"]), 
                     color = "black", linetype = "dotted")
}

# since we punish non-convergence behavior (cf. last big plateau),
# we want to still have our convergence speed based on the comparison 
# between the triangle (wo last plateau) and the step function area (w plateau)
# old: convspeed_2_plot_strict
#' Title
#'
#' @param solver_traj 
#' @param solver_traj_wo_plat 
#'
#' @return
#' @export
#'
#' @examples
makeConvSpeed_2_strict_plot = function(solver_traj, solver_traj_wo_plat){
  areasls = calcTrigonometricAreas(solver_traj_wo_plat, TRUE, TRUE)
  tri_peak =   areasls$tri_peak
  tri_sideA = areasls$tri_sideA
  tri_sideB =  areasls$tri_sideB
  tri_sideC = areasls$tri_sideC
  
  slope_AVG = (solver_traj_wo_plat[length(solver_traj_wo_plat$iter), "average.fitness"] - 
                 solver_traj_wo_plat[1, "average.fitness"]) / ((length(solver_traj_wo_plat$iter)-1) - 0) 
  slope_INC = (solver_traj_wo_plat[length(solver_traj_wo_plat$iter), "incumbant"] - 
                 solver_traj_wo_plat[1, "incumbant"]) / ((length(solver_traj_wo_plat$iter)-1) - 0) 
  slope_TRI1 = (tri_peak - solver_traj_wo_plat[1, "average.fitness"]) / 
    ((length(solver_traj_wo_plat$iter)-1) - 0) 
  slope_TRI2 = (tri_peak - solver_traj_wo_plat[1, "incumbant"]) / 
    ((length(solver_traj_wo_plat$iter)-1) - 0) 
  #build tri polygon (all 3 position coordinates)
  tri_poly = data.frame(x=c(0,
                            0,
                            length(solver_traj_wo_plat$iter) - 1), 
                        y=c(solver_traj_wo_plat[1, "average.fitness"],
                            solver_traj_wo_plat[1, "incumbant"], 
                            tri_peak))
  ggplot(data=solver_traj) +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "blue") +
    geom_step(mapping=aes(x=iter, y=average.fitness), color = "tomato") +
    ggtitle("AVG fitness vs. Incumbent") +
    geom_abline(intercept = solver_traj[1, "average.fitness"], 
                slope = slope_AVG, color="tomato", 
                linetype="dashed", size=0.6) +
    geom_abline(intercept = solver_traj[1, "incumbant"], 
                slope = slope_INC, color="blue", 
                linetype="dashed", size=0.6) +
    geom_abline(intercept = solver_traj[1, "average.fitness"], 
                slope = slope_TRI1, color="black", size=0.3) +
    geom_abline(intercept = solver_traj[1, "incumbant"], 
                slope = slope_TRI2, color="black", size=0.3) + 

    geom_point(mapping = aes(x = (length(solver_traj_wo_plat$iter)-1)-0, y = tri_peak), 
               shape = 10, size = 4, color = "brown") +

    geom_point(mapping = aes(x = 0, y = solver_traj[1, "incumbant"]), 
               shape = 10, size = 4, color = "brown") +
    geom_point(mapping = aes(x = 0, y = solver_traj[1, "average.fitness"]), 
               shape = 10, size = 4, color = "brown") +
    geom_polygon(data=tri_poly, aes(x, y), fill="cyan", alpha = 0.3) +
    geom_segment(aes(x = 0, y = solver_traj[1, "incumbant"], 
                     xend = 0, yend = solver_traj[1, "average.fitness"]), 
                     color = "black", size = 0.3) +
    geom_segment(aes(x = 0, y = solver_traj[1, "incumbant"], 
                     xend = 0, yend = solver_traj[1, "average.fitness"]), 
                 color = "black", linetype = "dotted") +
    geom_segment(aes(x = length(iter) - 1, 
                     y = solver_traj[length(iter), "incumbant"], 
                     xend = length(iter) - 1, 
                     yend = solver_traj[length(iter), "average.fitness"]), 
                     color = "black", linetype = "dotted")
}

