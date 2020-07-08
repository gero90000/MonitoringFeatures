# TODO:
# put roxygen2 comments here
# @author: Jacob, Pascal, Bjoern 
# @Feature_3.1: "VERTICAL GAPS"
# @description: 
#  Summary statistics of vertical gap(s).

# old: get_VG_stats
# TODO: maybe introduce quantile steps (here 0.05 as parameter "quant_by", and vg-threshold setter for drops[] as "quant_vg")
getVG_stats = function(solver_traj){
  result = list()
  VG_stat_ls = list()
  vg_threshold = NA
  vg_df = list()
  a = list()
  stat_flag = T

  if(length(solver_traj$iter) == 1){
    message("WARNING: \n there can be no VG since trajectory length is 1/0")
    #attr(solver_traj,'vertical_gaps') <- FALSE
    setattr(solver_traj, "vertical_gaps", FALSE)
    stat_flag = F
  } else {
    resls = list()
    res = 0
    # create DF with improvements_i = f(i-1) - f(i)
    for(i in 1:length(solver_traj$iter)){
      if(i == 1){
        resls[[i]] = 0
      } 
      res = solver_traj[i-1, "incumbant"] - solver_traj[i, "incumbant"]
      resls[[i]] = res
    }
    resls_df = as.data.frame(unlist(resls))
    names(resls_df) = "improvement"
    #-----------------------------------------
    #needed for visualization purpose later 
    improvements = resls_df                                    
    #-----------------------------------------
    # kill the 0s from the DF                                              
    resls_df = as.data.frame(resls_df[which(resls_df$improvement != 0), ])  # TODO: maybe not even needed
    names(resls_df) = "improvement"
    # claculate stats which allow gap derivation (here incuding the 0)
    #min_impr = min(resls_df$improvement); max_impr = max(resls_df$improvement)
    #mean_gap_impr = mean(resls_df$improvement); median_gap_impr = median(resls_df$improvement)
    #sd_impr = sd(resls_df$improvement)
    if(length(resls_df$improvement) == 0 | sum(resls_df$improvement) == 0){ 					# OR case added
      message("WARNING: \n there can be no VG since trajectory consists only of one plateau")
      #attr(solver_traj,'vertical_gaps') <- FALSE
      setattr(solver_traj, "vertical_gaps", FALSE)
      stat_flag = F
    } else {
      # "extra stats" (object specific)
      #quantiles_gap_impr = quantile(resls_df$improvement)
      # alternative:
      drops = resls_df$improvement %>% quantile(., probs = seq(0, 1, 0.05))
      #print(drops)
      # regular stats
      # identify/mark gaps and put into seperate df (VG)
      #vg_threshold = quantiles_gap_impr[4] 
      vg_threshold = drops[19L] # i.e., the XX% quantile 
      #print(vg_threshold)
      vg_df = resls_df[which(resls_df$improvement >= vg_threshold), ]
      VG_stat_ls = unlist(vg_df)


      setattr(solver_traj, "vertical_gaps", TRUE)
    }
  }
  name = "Vertical_Gaps"
  vg_stats = makeStats(name, VG_stat_ls, stat_flag)
  vg_stats = list.append(vg_stats,
                         vg_threshold = vg_threshold)
  vertical_gaps_data = as.data.frame(vg_df)

  result = list.append(result,
                       vg_stats = vg_stats,
                       vertical_gaps_data = vertical_gaps_data,
                       improvements = improvements)
  return(result)
}

# @author: Pascal, Bjoern 
# @Feature_3.2: "VERTICAL GAPS START"
# @description: 
#  Summary statistics of vertical gap(s) starts.
#  based on the idea that NETGEN has more signficant drops in the beginning (PASCAL) 
#  find related helpers in helper_monitor_feature.R

# TODO: add real starting position of vertical gap 
getVGStartStats = function(solver_traj, VG_stats, interval, increasing){
  resls = list()
  # idea_1:
  # relation of sum_impr_first_10-20% vs overall improvement(span), i.e., 
  ratio_first_perc_ls = list()
  amnt_ls = list()
  ratio_ls = list()
  
  if(attr(solver_traj,'vertical_gaps') == T) {
    improvements = ((diff(solver_traj$incumbant) %>% abs(.)) %>% data.frame(improvement = .)) %>% DataCombine::InsertRow(., c(0L), 1L)
    res_eax_traj_CACHE = cbind(solver_traj[, c("time.passed", "iter", "incumbant")], improvements)
    
    amnt_iter = length(solver_traj$iter) - 1L
    span_incumbent = sum(res_eax_traj_CACHE$improvement)
    # e.g., interval = 3 // up to first 30%, i.e., 10%,...,30%
    if(increasing == T){
      start = 1L
    } else {
      start = interval
    }
    for(i in start:interval){
      perc = tryCatch(
        {
          switch(i,
                 0.1,
                 0.2,
                 0.3,
                 0.4,
                 0.5,
                 0.6,
                 0.7,
                 0.8,
                 0.9,
                 1.0)
        },
        error = function(cond){
          message(cond)
          return(NA)
        },
        finally={
          if(i > 10){
            break
          }
        }
      )
      if(i < 10L){
        first_X_perc_traj = ((perc * amnt_iter) %>% round(., 0L)) + 1L
      } else {
        first_X_perc_traj = length(solver_traj$iter)
      }
      ratio_first_perc = sum(res_eax_traj_CACHE[1:first_X_perc_traj, "improvement"]) / span_incumbent
      ratio_first_perc_ls[i] = ratio_first_perc
      
      # idea_2
      # count drops/VGs based on 75% and 90% quantile in the first part of the run 
      improvements_perc = res_eax_traj_CACHE[1:first_X_perc_traj, "improvement"]
      amnt_VG_perc= improvements_perc[which(improvements_perc >= min(VG_stats$vertical_gaps_data))] %>% length(.)
      improvements_VG_ratio = amnt_VG_perc / length(VG_stats$vertical_gaps_data$vg_df)
      
      amnt_ls[i] = amnt_VG_perc
      ratio_ls[i] = improvements_VG_ratio 
    }

    firstVG_X_abs = res_eax_traj_CACHE[which(res_eax_traj_CACHE$improvement == 
                                       VG_stats$vertical_gaps_data$vg_df[1]), "iter"]
    lastVG_X_abs = res_eax_traj_CACHE[which(res_eax_traj_CACHE$improvement == 
                                      VG_stats$vertical_gaps_data$vg_df[length(VG_stats$vertical_gaps_data$vg_df)]), "iter"]
    firstVG_X_rel = firstVG_X_abs / length(solver_traj$iter)
    lastVG_X_rel = lastVG_X_abs / length(solver_traj$iter)

    resls = list.append(resls,
                        ratio_first_perc_ls = ratio_first_perc_ls,
                        amnt_ls = amnt_ls,
                        ratio_ls = ratio_ls,
                        firstVG_X_abs = firstVG_X_abs,
                        lastVG_X_abs = lastVG_X_abs,
                        firstVG_X_rel = firstVG_X_rel,
                        lastVG_X_rel = lastVG_X_rel
                        )
    if(increasing == F){
      resls = rmNullObs(resls)
      perc_name = paste(interval*10, "%", sep = " ")
      resls$percentage = perc_name
    } 
  } else {                                                #TODO: check is 0L or NA the more "approproate" entry?
    resls = list.append(resls,
                        ratio_first_perc_ls = 0L,
                        amnt_ls = 0L,
                        ratio_ls = 0L,
                        firstVG_X_abs = 0L,
                        lastVG_X_abs = 0L,
                        firstVG_X_rel = 0L,
                        lastVG_X_rel = 0L)
  }
  return(resls)
}

# old: calc_Gap_Power
getGapPower = function(VGstats, solver_traj){
  resls = list()
  if(attr(solver_traj,'vertical_gaps') == T){
    # helper to get the VG and its position
    helper = VGstats$improvements
    helper = as.data.frame(cbind(helper$improvement, seq(1:length(helper$improvement)))) 
    helper = helper[which(helper$V1 >= VGstats$vg_stats$vg_threshold), ]
    
    n = length(solver_traj$iter)
    m = length(helper$V2)
    
    #TODO: check if last step is VG then 1
    # fixed: now the correct slope is calculated (was wrong before because of index iteration irritation)
    slope_gap_area = (solver_traj[helper[m, "V2"]+ 1L, "incumbant"] - solver_traj[1L, "incumbant"]) / 
                     (helper[m, "V2"] - 0)
    slope_nongap_area = (solver_traj[n, "incumbant"] - solver_traj[helper[m, "V2"] + 1L, "incumbant"]) / 
                         (n - helper[m, "V2"])
    stat_ellbow_power = slope_gap_area / slope_nongap_area

    stat_ellbow_power_norm = ((helper[m, "V2"]/n) * slope_gap_area) / 
                             ((1 - (helper[m, "V2"]/n)) * slope_nongap_area) 
  } else {
    message("WARNING: \n since no vertical gaps existent there is no VG area, hence no slope.")
    stat_ellbow_power = 0L
    stat_ellbow_power_norm = 0L
  }
  resls = list.append(resls, 
                      stat_ellbow_power = stat_ellbow_power, 
                      stat_ellbow_power_norm = stat_ellbow_power_norm)
  return(resls)
}

# old: make_VG_hist
makeVG_hist = function(VGstats){
  ggplot(data = VGstats$vertical_gaps_data, aes(x = vg_df)) +
    geom_bar(color = "black", stat = "count", alpha = 0.4) +  #geom_histogram
    geom_density(color = "purple", size = 1, fill = "purple", alpha = 0.6) +
    scale_y_continuous(breaks = seq(1:(length(VGstats$vertical_gaps_data
                                              [which(VGstats$vertical_gaps_data$vg_df == 
                                                     VGstats$vg_stats$Mode_Vertical_Gaps), ]+1)))) +
    scale_x_discrete(limits= unique(VGstats$vertical_gaps_data$vg_df))
}

# old: make_VG_plot
makeVG_plot = function(VGstats, solver_traj){
  helper = VGstats$improvements
  helper = as.data.frame(cbind(helper$improvement, seq(1:length(VGstats$improvements$improvement))))
  helper = helper[which(helper$V1 >= VGstats$vg_stats$vg_threshold), ]
  
  ggplot(data=solver_traj) +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "black") +
    ggtitle("Incumbent Trajectory") +
    annotate("rect", xmin = 0 , 
                     ymin = solver_traj[length(solver_traj$iter), 'incumbant'], 
                     xmax = helper[length(helper$V2), "V2"], 
                     ymax = Inf, alpha = 0.3, fill = "green") +
    annotate("rect", xmin = helper[length(helper$V2), "V2"] , 
                     ymin = solver_traj[length(solver_traj$iter), 'incumbant'], 
                     xmax = Inf, 
                     ymax = Inf, alpha = 0.3, fill = "tomato") +
    geom_vline(xintercept = helper$V2, linetype = "solid", color = "grey", alpha = 0.3) +
    geom_vline(xintercept = helper$V2, linetype = "solid", color = "black", alpha = 0.3) +
    geom_segment(x = 0 , 
                 y = solver_traj[1, "incumbant"], 
                 xend = helper[length(helper$V2), "V2"], 
                 yend = solver_traj[helper[length(helper$V2), "V2"]+1L, "incumbant"], 
                 color = "darkgreen", linetype = "solid") +
    geom_segment(x =  helper[length(helper$V2), "V2"], 
                 y = solver_traj[helper[length(helper$V2), "V2"]+1L, "incumbant"], 
                 xend = length(solver_traj$iter)-1, 
                 yend = solver_traj[length(solver_traj$iter), "incumbant"], 
                 linetype = "solid", color = "red")    
}
