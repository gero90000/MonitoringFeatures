# @author:Bjoern
# @Feature_9: "KNEE RATIO"
# @description: 
#   Number/ratio of knees 
#   (here a knee is a point on the curve where the slope before is big and low after)

# old: get_knee_ratio
getKneeRatio = function(solver_traj, VG_stats, plat_start_stats){
  resls = list()
  knee_ls = list()
  knee_counter = 0L
  knee_ratio = 0L
  # prepare data @CALL get_VG_stats() necessary

  if(attr(solver_traj,'plateaunized') == T & 
     attr(solver_traj,'vertical_gaps') == T){
    impf_df = VG_stats$improvements %>% DataCombine::InsertRow(., c(0L), 1L)
    res_eax_traj_CACHE = cbind(solver_traj[, c("time.passed", "iter", "incumbant")], impf_df)
    res_eax_traj_CACHE$Knee_start_end = "_"
    '
    # END fist then START (since START is allowed to override END)
    # 1.1) get all starting points of Plateaus (end of knee)
    res_eax_traj_CACHE[which(res_eax_traj_CACHE$incumbant %in% 
                               plat_start_stats$plat_start$plats_positions$Group.1), "Knee_start_end"] = "End"
    # 1.2) get start of a knee by finding VG points (begin of knee)
    res_eax_traj_CACHE[which(res_eax_traj_CACHE$improvement >= 
                               VG_stats$vg_stats$vg_threshold), "Knee_start_end"] = "Start"

    '
    #### ALTERNATIVE
    drops = res_eax_traj_CACHE$improvement %>% quantile(., probs = seq(0, 1, 0.05))
    #print(drops)
    # regular stats
    # identify/mark gaps and put into seperate df (VG)
    #vg_threshold = quantiles_gap_impr[4] 
    vg_threshold = drops[18L]  # 85% quantile 
    lowSlope_threshold = drops[6L] # 25% quantile

    # END fist then START (since START is allowed to override END)
    # 1.1) get all starting points of Plateaus (end of knee)
    res_eax_traj_CACHE[which(res_eax_traj_CACHE$improvement <= lowSlope_threshold), "Knee_start_end"] = "End"
    # 1.2) get start of a knee by finding VG points (begin of knee)
    res_eax_traj_CACHE[which(res_eax_traj_CACHE$improvement >= vg_threshold), "Knee_start_end"] = "Start"

    if(!('%in%'("Start", res_eax_traj_CACHE$Knee_start_end))){
      resls = list.append(resls,
                          knee_ls = knee_ls,
                          knee_counter = knee_counter,
                          knee_ratio = knee_ratio)
    } else {
      # 2) look for "follow up" pattern 
      #    {{VG-np-np-np-P},
      #     {VG-np-np-P},
      #     {VG-np-P},
      #     {VG-P}}                       --> now updated
      knee_start_iter = res_eax_traj_CACHE[which(res_eax_traj_CACHE$Knee_start_end == "Start"), "iter"] 

      #+++ new +++ prohibit "double/tripple starts"
      knee_start_ls = c()
        for(i in 1:length(knee_start_iter)){
          if(i == length(knee_start_iter)){
            knee_start_ls = c(knee_start_ls, knee_start_iter[i])
            break
          }
          #print(knee_start_iter[i])
          if(knee_start_iter[i]+1 == knee_start_iter[i+1]){
            next
          } else {
            knee_start_ls = c(knee_start_ls, knee_start_iter[i])
          }
        }

      knee_start_iter = knee_start_ls

      knee_strictness = 10L
      for(i in 1:length(knee_start_iter)){
        stop = FALSE
        start = knee_start_iter[i]
        end = knee_start_iter[i] + knee_strictness
        
        if(end > length(solver_traj$iter)){ # catch indexing error
          end = length(solver_traj$iter) - 1 #-1 because j runs in accordance to iterations not indexes
        }

        j = start
        while(j <= end){
          if(res_eax_traj_CACHE[j + 1L, "Knee_start_end"] == "End"){ # +1 needed since now indexing
            knee_counter = knee_counter + 1L
            knee_ls[[i]] = start
            stop = TRUE
            if(stop == T){
              break
            }
          } else {
            j = j + 1
          }
        }
      }
      knee_ratio = knee_counter/length(solver_traj$iter)
      knee_ls = unlist(knee_ls)
      resls = list.append(resls,
                          knee_ls = knee_ls,
                          knee_counter = knee_counter,
                          knee_ratio = knee_ratio)
    }
  } else {
    message("WARNING: \n since there are no vertical gaps and/or Plateaus, Knees cannot be calculated.")
    resls = list.append(resls,
                        knee_ls = knee_ls,
                        knee_counter = knee_counter,
                        knee_ratio = knee_ratio)
  }
  return(resls)
}

# old: make_knee_plot
makeKnee_plot = function(solver_traj, knee_stats){
  knee_points = solver_traj[which(solver_traj$iter %in% knee_stats$knee_ls), "incumbant"]
  if(length(knee_stats$knee_ls) == 0){
    ggplot(data=solver_traj) +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "black") +
    ggtitle("AVG fitness vs. Incumbent")
  } else {
    knee_points = solver_traj[which(solver_traj$iter %in% knee_stats$knee_ls), "incumbant"]
    knee_points = data.frame(x = knee_stats$knee_ls, y = knee_points)
    ggplot(data=solver_traj) +
      geom_step(mapping=aes(x=iter, y=incumbant), color = "black") +
      geom_point(data = knee_points, mapping = aes(x = x, y = y), 
                 pch = 19, size = 10, color = "green", alpha = 0.2) +
      geom_point(data = knee_points, mapping = aes(x = x, y = y), 
                 pch = 16, size = 2, color = "darkgreen", alpha = 0.9) +
      geom_point(data = knee_points, mapping = aes(x = x, y = y), 
                 pch = 1, size = 10, color = "black", alpha = 0.9) +
      ggtitle("AVG fitness vs. Incumbent") +
      theme(
            panel.background = element_rect(fill = "white", colour = "white",
                                            size = 2, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"), 
            panel.grid.minor = element_line(size = 0.15, linetype = 'solid',
                                            colour = "grey")
    )
 }
}



