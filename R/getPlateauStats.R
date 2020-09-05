# TODO:
# put roxygen2 comments here
# @author: Jacob, Pascal, Bjoern 
# @Feature_2.1: "SUCCESS RATIO"
# @description: 
#  Ratio of successful iterations and total iterations from [0, 1]

# identify plateaus in df (the first redundant row is a plateau and increment counter ++)
# helper for @calcSuccessRatio()
# unsuccesful runs == 1 step length in Plateau 
# old: countPlateauLength
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
getPlateauLength = function(solver_traj){
  # brauchen wir nicht mehr da wir ja immer mit findplat() anfangen 
  #solver_traj = findPlat(solver_traj)#getPlateauDF(solver_traj) 
  tmpDF = plyr::ddply(solver_traj, .(incumbant), nrow)
  num_NoSuccess = sum(tmpDF$V1) - length(tmpDF$V1)
  return(num_NoSuccess)
}

# FIXED: get rid of global init settings
# old: calcSuccessRatio
#' Title
#'
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
getSuccessRatio = function(solver_traj){
  resls = list()
  # Plateau length == number of noSuccess steps in trajectory
  if(attr(solver_traj,'plateaunized') == T){
    num_NoSuccess = getPlateauLength(solver_traj)
  } else {
    message("WARNING: \n since no plateaus found there cannot be unsuccesful iterations.")
    num_NoSuccess = 0L
  }
  if(num_NoSuccess == 0){
    successRatio = 1L
  } else {
    successRatio = 1.0 - (num_NoSuccess / (length(solver_traj$iter)-1L))
    #init.platFound()
  }
  resls = list.append(resls,
                      successRatio = successRatio,
                      num_NoSuccess = num_NoSuccess,
                      num_total = (length(solver_traj$iter))-1L)
  return(resls)
}

# @Feature_2.2: "SUMMARY STATS OF PLATEAUS"
# @description: 
#  Summary statistics of length of plateaus.
#  (goes into the calcSuccessRatio() object)
#  mean, sd, min, max, total length

# old: calcPlateauStats
#' Title
#'
#' @param solver_traj 
#' @param successRatio 
#'
#' @return
#' @export
#'
#' @examples
getPlateauStats = function(solver_traj, successRatio){
  resls = list()
  plat_stat_ls = list()
  stat_flag = T
  
  if(successRatio < 1){     # <=> isTRUE(platFound__)
    tmpDF = plyr::ddply(solver_traj, .(incumbant), nrow)
    corrector = data.frame(x=rep(1, length(tmpDF$V1))) 
    tmpDF_stat = tmpDF$V1 - corrector
    # take out the 0 length Plateaus because those are "no Plateaus"
    # and would therefore bias the Plateau Statsitics
    tmpDF_stat = as.data.frame(tmpDF_stat[which(tmpDF_stat$x != 0), ])
    colnames(tmpDF_stat) = "x" 
    # "extra stats" (object specific)
    #num_NoSuccess = sum(tmpDF_stat$x)
    # regular stats
    plat_stat_ls = unlist(tmpDF_stat$x)
 
    #plat_stats = list.append(plat_stats, num_NoSuccess = num_NoSuccess)
  } else {
    message("WARNING: since no plateaus existent, no plateau statistics computable")
    stat_flag = F
    tmpDF = list()
  }
  name = "Plateau_Length"
  plat_stats = makeStats(name, plat_stat_ls, stat_flag)
  resls = list.append(resls,
                      plat_stats = plat_stats,
                      plateau_data = tmpDF)
  return(resls)
}

# @Feature_2.3: "SUMMARY STATS OF PLATEAUS START"
# @description: 
#  Summary statistics of the starting points of plateaus.
#  IDEA: 1forth whatever die incumbant[n] - incumbant[1] distance 
#  and count Plateaus within intervals

# old: getPlateau_DivAmnt
#' Title
#'
#' @param solver_traj 
#' @param interval 
#' @param successRatio 
#'
#' @return
#' @export
#'
#' @examples
getPlateauStartStats = function(solver_traj, interval, successRatio){
  resls = list()
  plat_start = list()
  
  if(successRatio < 1){   # TODO based on attributes plateaunized  
    intervals = interval
    tmp = plyr::ddply(solver_traj, .(incumbant), nrow)
    plats = tmp[which(tmp$V1 > 1),]
    plats_positions = solver_traj[which(solver_traj$incumbant %in% 
                                          plats$incumbant), c("iter", "incumbant")]

    plats$abs_X = NA 
    plats$abs_Y = plats$incumbant
    for(i in plats$incumbant){
      tmp_pos_batch = plats_positions[which(i == plats_positions$incumbant), ]
      plats[which(plats$incumbant == i), "abs_X"] = min(tmp_pos_batch$iter)
    }

    plats$rel_X = NA 
    plats$rel_Y = NA 
    plats$rel_X_vol = NA
    plats$rel_Y_vol = NA

    for(i in 1:length(plats$incumbant)){
      plats[i, "rel_X"] = plats[i, "abs_X"] / (length(solver_traj$iter) - 1L)
      plats[i, "rel_Y"] = (solver_traj[1, "incumbant"] - plats[i, "abs_Y"]) / 
                          (solver_traj[1, "incumbant"] - solver_traj[length(solver_traj$iter), "incumbant"])

      # volume based (decreasing from 1 in both dimensions)
      plats[i, "rel_X_vol"] = (1L - plats[i, "rel_X"]) %>% abs(.)
      #plats[i, "rel_X_vol"] = (1L - (plats[i, "abs_X"] / (length(solver_traj$iter) - 1L))) %>% abs(.)
      
      plats[i, "rel_Y_vol"] = (1L - plats[i, "rel_Y"]) %>% abs(.)
      #plats[i, "rel_Y_vol"] = (plats[i, "abs_Y"] - solver_traj[length(solver_traj$iter), "incumbant"]) /
      #  (solver_traj[1, "incumbant"] - solver_traj[length(solver_traj$iter), "incumbant"])
    }

    characteristic_volume_data = plats[which(plats$incumbant == max(plats$incumbant)), c("rel_X_vol", "rel_Y_vol") ]
    characteristic_volume_X = characteristic_volume_data$rel_X_vol[1] %>% as.double(.)
    characteristic_volume_Y = characteristic_volume_data$rel_Y_vol[1] %>% as.double(.)
    characteristic_volume = characteristic_volume_X * characteristic_volume_Y

    plat_start_X_abs = makeStats("plat_start_X_abs", plats$abs_X)
    plat_start_X_rel = makeStats("plat_start_X_rel", plats$rel_X)
    plat_start_Y_abs = makeStats("plat_start_Y_abs", plats$abs_Y)
    plat_start_Y_rel = makeStats("plat_start_Y_rel", plats$rel_Y)

    x_abs = plats_positions$iter[1]
    y_abs = plats_positions$incumbant[1]
    plats_positions = aggregate(plats_positions$iter, 
                                list(plats_positions$incumbant), mean) %>% purrr::map_df(., rev)

    #plat_boarder_Y = plats_positions[1, 1] %>% as.double(.)
    #plat_boarder_X = plats_positions[1, 2] %>% as.double(.)
    Plats_area_begin_X =  x_abs / (length(solver_traj$iter)-1)

    if(length(tmp$V1) == 1){ # in case it the traj is only one (big) plateau
      Plats_area_begin_Y = 0L
    } else {
      Plats_area_begin_Y = (solver_traj[1, "incumbant"] - y_abs) / 
                           (solver_traj[1, "incumbant"] - solver_traj[length(solver_traj$iter), "incumbant"])
    }
    plat_start = list.append(plat_start,
                             Plats_area_begin_X = Plats_area_begin_X,
                             Plats_area_begin_X_abs = x_abs,
                             Plats_area_begin_Y = Plats_area_begin_Y,
                             Plats_area_begin_Y_abs = y_abs,

                             plat_start_X_abs_Stats = plat_start_X_abs,
                             plat_start_X_rel_Stats = plat_start_X_rel,
                             plat_start_Y_abs_Stats = plat_start_Y_abs,
                             plat_start_Y_rel_Stats = plat_start_Y_rel,

                             characteristic_volume_X = characteristic_volume_X,
                             characteristic_volume_Y = characteristic_volume_Y,
                             characteristic_volume = characteristic_volume,

                             plats = plats,
                             plats_positions = plats_positions
                             )
    distance = (tmp[nrow(tmp), "incumbant"] - tmp[1, "incumbant"])  %>% as.double(.)
    start = (tmp[1, "incumbant"])  %>% as.double(.)
    interval_points = list()
    
    for(i in 1:intervals){
      p = (start + (i/intervals) * distance) %>% round(., 2)
      interval_points[i] = p 
    }
    interval_points = unlist(interval_points)
    tmp_grt1 = tmp[which(tmp$V1 > 1), ]
    
    interval_contains = list()
    counter_ls = list()
    counter = 0L
    for(i in 1:length(interval_points)){
      anchor = i
      interval = sort(interval_points, decreasing = F)[i]
      for(j in 1:length(tmp_grt1$incumbant)){
        check = sort(tmp_grt1$incumbant, decreasing = F)[j]
        # when in first interval no "&" check for lower bound needed
        if(anchor == 1){
          if(check <= interval){
            counter = counter + 1
          }
        } else {
          if(check <= interval & check > sort(interval_points, decreasing = F)[anchor-1]){
            counter = counter + 1
          }
        }
      }
      if(anchor == 1){
        counter_ls[[paste("interval <=", interval_points[i], sep = " ")]] = counter 
      } else {
        counter_ls[[paste(interval_points[i-1], "< interval <=", interval_points[i], sep = " ")]] = counter 
      }
      if(counter > 0){
        interval_contains[[i]] = interval
      }
      counter = 0
    }
  } else {
    message("WARNING: \n since no plateaues existent statistics about plat starts are not available (thus NA).")
    plat_start = list.append(plat_start,
                             Plats_area_begin_X = NA,
                             Plats_area_begin_X_abs = NA,
                             Plats_area_begin_Y = NA,
                             Plats_area_begin_Y_abs = NA,

                             plat_start_X_abs_Stats = NA,
                             plat_start_X_rel_Stats = NA,
                             plat_start_Y_abs_Stats = NA,
                             plat_start_Y_rel_Stats = NA,

                             characteristic_volume_X = NA,
                             characteristic_volume_Y = NA,
                             characteristic_volume = NA,

                             plats = NA,
                             plats_positions = NA
                             )
    interval_points = NA
    interval_contains = NA
    counter_ls = list()
    tmp = list()
  }
  resls = list.append(resls, 
                      plat_start = plat_start,
                      interval_points = interval_points,
                      interval_contains = interval_contains,
                      counter_ls = counter_ls, 
                      data = tmp
                      ) #tmp_rev, # data
  return(resls)
}

# Wrapper for plotting the outome of @getPlateau_DivAmnt()
# data$interval Points works in vline because data is passed before data$data is extracted in ggplot
#' Title
#'
#' @param data 
#' @param incumbant 
#' @param V1 
#'
#' @return
#' @export
#'
#' @examples
generateIntervalplot = function(data, incumbant = "incumbant", V1 = "V1") {
  if(is.null(data[["data"]])){
    writeLines("ToDo: ERRORHANDLING here")
  }
  else {
    total = 10L
    pb = txtProgressBar(min = 0, max = total, style = 3)
    for(i in 1:total){
      Sys.sleep(0.08)
      setTxtProgressBar(pb, i)
    }
    close(pb)
    x = mean(data$data$incumbant) + mean(data$data$incumbant)/280
    plot = ggplot(data = data$data, aes_string(x=incumbant, y = V1)) +
      geom_point(size = 2, color = "purple") +
      geom_line(color = "grey", size = 0.3) +
      scale_x_continuous(trans = "reverse") +
      geom_abline(intercept = 1.8, slope = 0, color = "tomato") +
      geom_abline(intercept = 1, slope = 0, color = "black") +
      geom_vline(xintercept = data$interval_points, linetype = "dotted") +
      geom_vline(xintercept = data$interval_points, linetype = "solid", color = "black", alpha = 0.3) +
      geom_vline(xintercept = as.double(data$data[1, "incumbant"])) +
      annotate("text", x = x , y = 1.89, label = "Plateau Boarder", color = "tomato", size = 3) +
      ylab("Plateau Length (-1)") + 
      xlab("fitness(Incumbant)")
    return(plot)
  }
}

#' Title
#'
#' @param test_ls 
#' @param solver_traj 
#'
#' @return
#' @export
#'
#' @examples
makePlateau_plot = function(test_ls, solver_traj){
  C = test_ls$plat_start$plats_positions
  hlines = unlist(test_ls$interval_contains)

  ggplot(data=solver_traj) +
    annotate("rect", xmin = 0 , 
                     ymin = hlines[length(hlines)], 
                     xmax = Inf , 
                     ymax = solver_traj[1, 'incumbant'], alpha = 0.3, fill = "green") +
    annotate("rect", xmin = 0 , 
                     ymin = solver_traj[length(solver_traj$iter), 'incumbant'], 
                     xmax = Inf, 
                     ymax = hlines[length(hlines)], alpha = 0.3, fill = "tomato") +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "black") +
    geom_hline(yintercept = hlines, linetype = "dotted", color = "black", alpha = 0.98) +
    geom_hline(yintercept = test_ls$interval_points, linetype = "dotted", color = "black", alpha = 0.98) +
    ggtitle("Incumbent Trajectory") +
    geom_point(data = C, mapping = aes(x = x, y = Group.1), pch = 10, size = 5.5, color = "red", alpha = 1)
}


