#' Monitoring Featre main getter
#'
#' @param instance_ls 
#' @param cutoff.time 
#' @param scaling 
#'
#' @return
#' @export
#'
#' @examples
getMonitoringFeatures = function(instance_ls, cutoff.time, scaling, seed){
  #========================================================================================
  # set local variables
  #========================================================================================
  base::set.seed(seed)
  big_res_ls = list() 
  cutoff.time = cutoff.time
  names_ls = names(instance_ls)

  for(i in 1:length(instance_ls)){ 
    # we need this k to have a proper grouping,
    # that is (1 eff,2 real), (3,4), etc. for every %-batch resp.
    k = 0L
    name = names_ls[i]
    for(j in 1:length(instance_ls[[i]])){  
      #========================================================================================
      # to allow a integer based group assignment on premise  
      #========================================================================================
      adder = switch(i,
                     0,
                     300,
                     2300,
                     4300,
                     6300,
                     7300,
                     8300,
                     8600,
                     8900, 
                     9200) 
      group = j + k + adder
      # needed for proper grouping, cf. above.
      k = k + 1L
      print(name)
      
      #========================================================================================
      # load instance
      #========================================================================================
      instance = importFromTSPlibFormat(instance_ls[[i]][j], round.distances = TRUE)
      
      #========================================================================================
      # run solver (for every instance run solver 6 seconds)
      #========================================================================================
      res_eax = runSolver("eax", instance, solver.pars = list(cutoff.time = cutoff.time)) 
      
      #========================================================================================
      # prepare res_eax trajectory data
      #========================================================================================
      res_eax$trajectory = insertPlateaus(res_eax$trajectory)
      res_eax$trajectory = imputeTimes(res_eax$trajectory)
      res_eax_copy = res_eax
      res_eax_copy$trajectory = imputeLastPlateau(res_eax$trajectory, cutoff.time = cutoff.time)
      
      #========================================================================================
      # scale trjactory data to min-max (as otherwise the features depent on the tour length
      #========================================================================================
      if(scaling == TRUE){
        saver = min_max_memory(res_eax$trajectory, "scale_multi")
        tmp_min_inc = saver$getMin() 
        tmp_max_inc = saver$getMax() 
        
        class(res_eax$trajectory) <- append(class(res_eax$trajectory), "scale_multi")
        class(res_eax_copy$trajectory) <- append(class(res_eax_copy$trajectory), "scale_multi")
        
        scaled_inc_avg_LS = scalerOwn(res_eax$trajectory)
        eps = scaled_inc_avg_LS[[1]][length(scaled_inc_avg_LS[[1]])-1]/2
        scaled_inc_avg_LS[[1]][length(scaled_inc_avg_LS[[1]])] <- eps
        
        scaled_inc_avg_copy_LS = scalerOwn(res_eax_copy$trajectory)
        scaled_inc_avg_copy_LS[[1]][which(scaled_inc_avg_copy_LS[[1]] == 0)] <- eps
        
        # override original value with scaled values 
        res_eax$trajectory$incumbant <- scaled_inc_avg_LS[1] %>% unlist(.)
        res_eax$trajectory$average.fitness <- scaled_inc_avg_LS[2] %>% unlist(.)
        
        res_eax_copy$trajectory$incumbant <- scaled_inc_avg_copy_LS[1] %>% unlist(.)
        res_eax_copy$trajectory$average.fitness <- scaled_inc_avg_copy_LS[2] %>% unlist(.)
      }
      
      #========================================================================================
      # now cut trajectory from cut.off time to 1
      #========================================================================================
      for(l in cutoff.time:1){
        print(paste("time batch: ", l, sep = ""))
        # cutting of original trajectory 
        # effective 
        res_eax$trajectory <- res_eax$trajectory[which(res_eax$trajectory$time.passed <= l), ]
        # real
        res_eax_copy$trajectory <- res_eax_copy$trajectory[which(res_eax_copy$trajectory$time.passed <= l), ]
        
        # check if in res_eax_copy is still a plateau or not (if not change class "plateaunized" to FALSE)
        # (needed as a quick hot fix since 0.25 for 1 sec batches mostly do not reveal a trajectory anymore)
        plat_check = checkPlat(res_eax$trajectory)
        plat_check_copy = checkPlat(res_eax_copy$trajectory)
        print(paste("plateau: ", plat_check, sep =""))
        print(paste("plateau: ", plat_check_copy, sep = ""))
        print("-------------------------------------------")
        attr(res_eax$trajectory, "plateaunized") <- plat_check
        attr(res_eax_copy$trajectory, "plateaunized") <- plat_check_copy
        
        #========================================================================================
        # effective preperation   
        #========================================================================================
        res_eax$trajectory$groupby = NA
        res_eax$trajectory$generator = NA
        res_eax$trajectory$label = NA
        res_eax$trajectory$groupby = rep(group, length(res_eax$trajectory$iter))
        res_eax$trajectory$generator = rep(name, length(res_eax$trajectory$iter)) 
        res_eax$trajectory$label = rep("effective", length(res_eax$trajectory$iter))
        # derive ID here 
        res_eax$trajectory$ID = NA 
        # e.g.: "/Users/bjornbreilmann/Desktop/local-Jacob/instances_Jakob/netgen1000/1000-5-9.tsp" 
        id_long = instance_ls[[i]][j]  
        # TODO: make this dynamic
        id = sub("/Users/bjornbreilmann/Desktop/local-Jacob/instances_Jakob/", "", id_long)
        res_eax$trajectory$ID = id
        
        #========================================================================================
        # real preperation  
        #========================================================================================
        res_eax_copy$trajectory$groupby = NA
        res_eax$trajectory$generator = NA
        res_eax_copy$trajectory$label  = NA
        res_eax_copy$trajectory$groupby = rep((group + 1L), length(res_eax_copy$trajectory$iter))
        res_eax_copy$trajectory$generator = rep(name, length(res_eax_copy$trajectory$iter)) 
        res_eax_copy$trajectory$label = rep("real", length(res_eax_copy$trajectory$iter))
        # derive ID here 
        res_eax_copy$trajectory$ID = NA 
        res_eax_copy$trajectory$ID = id
        
        #========================================================================================
        # Aggregating trajectories to have it squished as 1 appendable row 
        #========================================================================================
        # effective
        plottable_df = res_eax$trajectory
        plottable_df = plottable_df %>% dplyr::count(generator, ID, label, groupby)
        
        # real
        plottable_df_copy = res_eax_copy$trajectory
        plottable_df_copy = plottable_df_copy %>% dplyr::count(generator, ID, label, groupby)
        
        # ++ new +++ create batches where the monitoring features are created for
        # 1) create batch ID column 
        plottable_df = cbind(batch = 0.0, plottable_df)
        plottable_df_copy = cbind(batch = 0.0, plottable_df_copy)
        
        # show the aggregated information to the user
        print(plottable_df)
        
        #========================================================================================
        # drill down into %-batch 
        #========================================================================================
        for(b in c(0.25, 0.5, 0.75, 1.0)){
          print(b)
          plottable_df$batch = b
          plottable_df_copy$batch = b
          
          #========================================================================================
          # create temporary copies of res_eax$trajectory (+ copy version)
          # --> these will be passed as arguments to the functions
          # --> from now apply monitoring functions on this object
          #========================================================================================
          # effective
          threshold_eff = (res_eax$trajectory[length(res_eax$trajectory$iter), "iter"] * b) %>% ceiling(.)
          solvertraj_eff = res_eax$trajectory[which(res_eax$trajectory$iter <= threshold_eff), ]
          print(paste("eff runtime:", (solvertraj_eff %>% .[length(.$iter), "time.passed"])))
          
          # real
          threshold_real = (res_eax_copy$trajectory[length(res_eax_copy$trajectory$iter), "iter"] * b) %>% ceiling(.)
          solvertraj_real = res_eax_copy$trajectory[which(res_eax_copy$trajectory$iter <= threshold_real), ]
          print(paste("real runtime:", (solvertraj_real %>% .[length(.$iter), "time.passed"])))
          
          #========================================================================================
          # 0) default stats (passing of arguments only one time)
          #========================================================================================
          default_traj_stats = getDefaultStats(solvertraj_eff, solvertraj_real, eff_real_stat = TRUE)
          
          # Runtime and iter stats 
          plottable_df$runtime = default_traj_stats$effective_runtime
          plottable_df$runtime_cleaned = default_traj_stats$effective_runtime # default_traj_stats$real_runtime_cleaned # +++ new +++
          plottable_df$iterations = default_traj_stats$effective_iterations # was corrected
          plottable_df$iterations_cleaned = default_traj_stats$effective_iterations #default_traj_stats$real_iterations_cleaned # +++ new +++
          plottable_df$plateau_found = default_traj_stats$plateau_found_effective
          plottable_df$time_per_iter_AVG = default_traj_stats$time_per_iter_AVG_eff
          plottable_df$time_per_iter_AVG_cleaned = default_traj_stats$time_per_iter_AVG_eff # default_traj_stats$time_per_iter_AVG_real_cleaned # +++ new +++
          plottable_df$biggest_drop_span_ratio = default_traj_stats$TODO_biggest_drop_span_ratio  #same for real
          
          # Time Diff stats
          plottable_df$Num_time_diff = default_traj_stats$time_diff_stat_ls_eff$Num_time_diff_eff
          plottable_df$Min_time_diff = default_traj_stats$time_diff_stat_ls_eff$Min_time_diff_eff
          plottable_df$Max_time_diff = default_traj_stats$time_diff_stat_ls_eff$Max_time_diff_eff
          plottable_df$Mean_time_diff = default_traj_stats$time_diff_stat_ls_eff$Mean_time_diff_eff
          plottable_df$Mode_time_diff = default_traj_stats$time_diff_stat_ls_eff$Mode_time_diff_eff
          plottable_df$Median_time_diff = default_traj_stats$time_diff_stat_ls_eff$Median_time_diff_eff
          plottable_df$Quantiles_time_diff0 = default_traj_stats$time_diff_stat_ls_eff$Quantiles_time_diff_eff[1] %>% as.numeric(.)
          plottable_df$Quantiles_time_diff25 = default_traj_stats$time_diff_stat_ls_eff$Quantiles_time_diff_eff[2] %>% as.numeric(.)
          plottable_df$Quantiles_time_diff50 = default_traj_stats$time_diff_stat_ls_eff$Quantiles_time_diff_eff[3] %>% as.numeric(.)
          plottable_df$Quantiles_time_diff75 = default_traj_stats$time_diff_stat_ls_eff$Quantiles_time_diff_eff[4] %>% as.numeric(.)
          plottable_df$Quantiles_time_diff100 = default_traj_stats$time_diff_stat_ls_eff$Quantiles_time_diff_eff[5] %>% as.numeric(.)
          plottable_df$SD_time_diff = default_traj_stats$time_diff_stat_ls_eff$SD_time_diff_eff
          plottable_df$Var_time_diff = default_traj_stats$time_diff_stat_ls_eff$Var_time_diff_eff
          plottable_df$Skew_time_diff = default_traj_stats$time_diff_stat_ls_eff$Skew_time_diff_eff
          plottable_df$Span_time_diff = default_traj_stats$time_diff_stat_ls_eff$Span_time_diff_eff
          plottable_df$Varcoeff_time_diff = default_traj_stats$time_diff_stat_ls_eff$Varcoeff_time_diff_eff
          
          #Incumbent Fitness (+ diff() stats)
          plottable_df$Num_incumbent = default_traj_stats$incumbent_stat_ls_eff$Num_incumbent
          plottable_df$Min_incumbent = default_traj_stats$incumbent_stat_ls_eff$Min_incumbent
          plottable_df$Max_incumbent = default_traj_stats$incumbent_stat_ls_eff$Max_incumbent
          plottable_df$Mean_incumbent = default_traj_stats$incumbent_stat_ls_eff$Mean_incumbent
          plottable_df$Mode_incumbent = default_traj_stats$incumbent_stat_ls_eff$Mode_incumbent
          plottable_df$Median_incumbent = default_traj_stats$incumbent_stat_ls_eff$Median_incumbent
          plottable_df$Quantiles_incumbent0 = default_traj_stats$incumbent_stat_ls_eff$Quantiles_incumbent[1] %>% as.numeric(.)  # maybe normalize these later over all instances?
          plottable_df$Quantiles_incumbent25 = default_traj_stats$incumbent_stat_ls_eff$Quantiles_incumbent[2] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent50= default_traj_stats$incumbent_stat_ls_eff$Quantiles_incumbent[3] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent75 = default_traj_stats$incumbent_stat_ls_eff$Quantiles_incumbent[4] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent100 = default_traj_stats$incumbent_stat_ls_eff$Quantiles_incumbent[5] %>% as.numeric(.)
          plottable_df$SD_incumbent = default_traj_stats$incumbent_stat_ls_eff$SD_incumbent
          plottable_df$Var_incumbent = default_traj_stats$incumbent_stat_ls_eff$Var_incumbent
          plottable_df$Skew_incumbent = default_traj_stats$incumbent_stat_ls_eff$Skew_incumbent
          plottable_df$Span_incumbent = default_traj_stats$incumbent_stat_ls_eff$Span_incumbent
          plottable_df$Varcoeff_incumbent = default_traj_stats$incumbent_stat_ls_eff$Varcoeff_incumbent
          
          plottable_df$Num_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Num_incumbent_diff
          plottable_df$Min_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Min_incumbent_diff
          plottable_df$Max_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Max_incumbent_diff
          plottable_df$Mean_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Mean_incumbent_diff
          plottable_df$Mode_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Mode_incumbent_diff
          plottable_df$Median_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Median_incumbent_diff
          plottable_df$Quantiles_incumbent_diff0 = default_traj_stats$incumbent_diff_stat_ls_eff$Quantiles_incumbent_diff[1] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent_diff25 = default_traj_stats$incumbent_diff_stat_ls_eff$Quantiles_incumbent_diff[2] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent_diff50 = default_traj_stats$incumbent_diff_stat_ls_eff$Quantiles_incumbent_diff[3] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent_diff75 = default_traj_stats$incumbent_diff_stat_ls_eff$Quantiles_incumbent_diff[4] %>% as.numeric(.)
          plottable_df$Quantiles_incumbent_diff100 = default_traj_stats$incumbent_diff_stat_ls_eff$Quantiles_incumbent_diff[5] %>% as.numeric(.)
          plottable_df$SD_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$SD_incumbent_diff
          plottable_df$Var_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Var_incumbent_diff
          plottable_df$Skew_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Skew_incumbent_diff
          plottable_df$Span_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Span_incumbent_diff
          plottable_df$Varcoeff_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_eff$Varcoeff_incumbent_diff
          
          #AVG Fitness (+ diff() stats)
          plottable_df$Num_avgFit = default_traj_stats$avgFit_stat_ls_eff$Num_avgFit
          plottable_df$Min_avgFit = default_traj_stats$avgFit_stat_ls_eff$Min_avgFit
          plottable_df$Max_avgFit = default_traj_stats$avgFit_stat_ls_eff$Max_avgFit
          plottable_df$Mean_avgFit = default_traj_stats$avgFit_stat_ls_eff$Mean_avgFit
          plottable_df$Mode_avgFit = default_traj_stats$avgFit_stat_ls_eff$Mode_avgFit
          plottable_df$Median_avgFit = default_traj_stats$avgFit_stat_ls_eff$Median_avgFit
          plottable_df$Quantiles_avgFit0 = default_traj_stats$avgFit_stat_ls_eff$Quantiles_avgFit[1] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit25 = default_traj_stats$avgFit_stat_ls_eff$Quantiles_avgFit[2] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit50 = default_traj_stats$avgFit_stat_ls_eff$Quantiles_avgFit[3] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit75 = default_traj_stats$avgFit_stat_ls_eff$Quantiles_avgFit[4] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit100 = default_traj_stats$avgFit_stat_ls_eff$Quantiles_avgFit[5] %>% as.numeric(.)
          plottable_df$SD_avgFit = default_traj_stats$avgFit_stat_ls_eff$SD_avgFit
          plottable_df$Var_avgFit = default_traj_stats$avgFit_stat_ls_eff$Var_avgFit
          plottable_df$Skew_avgFit = default_traj_stats$avgFit_stat_ls_eff$Skew_avgFit
          plottable_df$Span_avgFit = default_traj_stats$avgFit_stat_ls_eff$Span_avgFit
          plottable_df$Varcoeff_avgFit = default_traj_stats$avgFit_stat_ls_eff$Varcoeff_avgFit
          
          plottable_df$Num_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Num_avgFit_diff
          plottable_df$Min_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Min_avgFit_diff
          plottable_df$Max_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Max_avgFit_diff
          plottable_df$Mean_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Mean_avgFit_diff
          plottable_df$Mode_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Mode_avgFit_diff
          plottable_df$Median_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Median_avgFit_diff
          plottable_df$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Quantiles_avgFit_diff[1] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Quantiles_avgFit_diff[2] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Quantiles_avgFit_diff[3] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Quantiles_avgFit_diff[4] %>% as.numeric(.)
          plottable_df$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Quantiles_avgFit_diff[5] %>% as.numeric(.)
          plottable_df$SD_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$SD_avgFit_diff
          plottable_df$Var_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Var_avgFit_diff
          plottable_df$Skew_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Skew_avgFit_diff
          plottable_df$Span_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Span_avgFit_diff
          plottable_df$Varcoeff_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_eff$Varcoeff_avgFit_diff
          
          # same for real
          plottable_df$real_eff_same_length_FLAG = default_traj_stats$real_effective_same_length
          plottable_df$eff_real_iter_ratio = default_traj_stats$eff_real_iter_relation# default_traj_stats$eff_real_iter_ratio
          plottable_df$eff_real_time_ratio = default_traj_stats$eff_real_time_relation #default_traj_stats$eff_real_time_ratio
          # +++ new +++
          plottable_df$effective_partion_iter = default_traj_stats$effective_partion_iter
          plottable_df$effective_partion_time = default_traj_stats$effective_partion_time
          
          plottable_df$incumbent_eff_real_ratio = default_traj_stats$incumbent_eff_real_ratio
          plottable_df$avgFit_eff_real_ratio = default_traj_stats$avgFit_eff_real_ratio
          
          #========================================================================================
          # 1) trajectory length statistic
          #========================================================================================
          # TODO:
          # integrate regular statistical output 
          # add relational statistics (e.g.: inc vs avg len ratio (same for copy), eff vs real len ratio)
          
          # eff
          traj_len_inc = solvertraj_len(solvertraj_eff, which_one = "incumbant")
          traj_len_avgfit = solvertraj_len(solvertraj_eff, which_one = "average.fitness")
          
          # INC
          plottable_df$len_solvertraj_INC = traj_len_inc$len_solvertraj
          plottable_df$actual_len_INC = traj_len_inc$actual_len
          plottable_df$Num_len_traj_INC = traj_len_inc$len_stats$Num_len_traj
          plottable_df$Min_len_traj_INC = traj_len_inc$len_stats$Min_len_traj
          plottable_df$Max_len_traj_INC = traj_len_inc$len_stats$Max_len_traj
          plottable_df$Mean_len_traj_INC = traj_len_inc$len_stats$Mean_len_traj
          plottable_df$Mode_len_traj_INC = traj_len_inc$len_stats$Mode_len_traj
          plottable_df$Median_len_traj_INC = traj_len_inc$len_stats$Median_len_traj
          plottable_df$Quantiles_len_traj0_INC = traj_len_inc$len_stats$Quantiles_len_traj[1]
          plottable_df$Quantiles_len_traj25_INC = traj_len_inc$len_stats$Quantiles_len_traj[2]
          plottable_df$Quantiles_len_traj50_INC = traj_len_inc$len_stats$Quantiles_len_traj[3]
          plottable_df$Quantiles_len_traj70_INC = traj_len_inc$len_stats$Quantiles_len_traj[4]
          plottable_df$Quantiles_len_traj100_INC = traj_len_inc$len_stats$Quantiles_len_traj[5]
          plottable_df$SD_len_traj_INC = traj_len_inc$len_stats$SD_len_traj
          plottable_df$Var_len_traj_INC = traj_len_inc$len_stats$Var_len_traj
          plottable_df$Skew_len_traj_INC = traj_len_inc$len_stats$Skew_len_traj
          plottable_df$Span_len_traj_INC = traj_len_inc$len_stats$Span_len_traj
          plottable_df$Varcoeff_len_traj_INC = traj_len_inc$len_stats$Varcoeff_len_traj
          
          # AVG FIT
          plottable_df$len_solvertraj_AVGFIT = traj_len_avgfit$len_solvertraj
          plottable_df$actual_len_AVGFIT = traj_len_avgfit$actual_len
          plottable_df$Num_len_traj_AVGFIT = traj_len_avgfit$len_stats$Num_len_traj
          plottable_df$Min_len_traj_AVGFIT = traj_len_avgfit$len_stats$Min_len_traj
          plottable_df$Max_len_traj_AVGFIT = traj_len_avgfit$len_stats$Max_len_traj
          plottable_df$Mean_len_traj_AVGFIT = traj_len_avgfit$len_stats$Mean_len_traj
          plottable_df$Mode_len_traj_AVGFIT = traj_len_avgfit$len_stats$Mode_len_traj
          plottable_df$Median_len_traj_AVGFIT = traj_len_avgfit$len_stats$Median_len_traj
          plottable_df$Quantiles_len_traj0_AVGFIT = traj_len_avgfit$len_stats$Quantiles_len_traj[1]
          plottable_df$Quantiles_len_traj25_AVGFIT = traj_len_avgfit$len_stats$Quantiles_len_traj[2]
          plottable_df$Quantiles_len_traj50_AVGFIT = traj_len_avgfit$len_stats$Quantiles_len_traj[3]
          plottable_df$Quantiles_len_traj70_AVGFIT = traj_len_avgfit$len_stats$Quantiles_len_traj[4]
          plottable_df$Quantiles_len_traj100_AVGFIT = traj_len_avgfit$len_stats$Quantiles_len_traj[5]
          plottable_df$SD_len_traj_AVGFIT = traj_len_avgfit$len_stats$SD_len_traj
          plottable_df$Var_len_traj_AVGFIT = traj_len_avgfit$len_stats$Var_len_traj
          plottable_df$Skew_len_traj_AVGFIT = traj_len_avgfit$len_stats$Skew_len_traj
          plottable_df$Span_len_traj_AVGFIT = traj_len_avgfit$len_stats$Span_len_traj
          plottable_df$Varcoeff_len_traj_AVGFIT = traj_len_avgfit$len_stats$Varcoeff_len_traj
          
          # ratios
          # adjust to copy version
          plottable_df$INC_AVGFIT_len_ratio = traj_len_inc$actual_len / traj_len_avgfit$actual_len
          
          #========================================================================================
          # 2.1) Lina Default
          #========================================================================================
          lina_def.INC = lina_default(solvertraj_eff, "incumbant")
          lina_def.AVGFIT = lina_default(solvertraj_eff, "average.fitness")
          
          # INC
          # grets INC
          plottable_df$gret10_INC = lina_def.INC$tmp_resls$gret[1]
          plottable_df$gret20_INC = lina_def.INC$tmp_resls$gret[2]
          plottable_df$gret30_INC = lina_def.INC$tmp_resls$gret[3]
          plottable_df$gret40_INC = lina_def.INC$tmp_resls$gret[4]
          plottable_df$gret50_INC = lina_def.INC$tmp_resls$gret[5]
          plottable_df$gret60_INC = lina_def.INC$tmp_resls$gret[6]
          plottable_df$gret70_INC = lina_def.INC$tmp_resls$gret[7]
          plottable_df$gret80_INC = lina_def.INC$tmp_resls$gret[8]
          plottable_df$gret90_INC = lina_def.INC$tmp_resls$gret[9]
          
          # lt INC
          plottable_df$lt10_INC = lina_def.INC$tmp_resls$lt[1]
          plottable_df$lt20_INC = lina_def.INC$tmp_resls$lt[2]
          plottable_df$lt30_INC = lina_def.INC$tmp_resls$lt[3]
          plottable_df$lt40_INC = lina_def.INC$tmp_resls$lt[4]
          plottable_df$lt50_INC = lina_def.INC$tmp_resls$lt[5]
          plottable_df$lt60_INC = lina_def.INC$tmp_resls$lt[6]
          plottable_df$lt70_INC = lina_def.INC$tmp_resls$lt[7]
          plottable_df$lt80_INC = lina_def.INC$tmp_resls$lt[8]
          plottable_df$lt90_INC = lina_def.INC$tmp_resls$lt[9]
          
          # gret lt ratio INC
          plottable_df$lt_gret_ratio_10_INC = lina_def.INC$tmp_resls$lt[1] / lina_def.INC$tmp_resls$gret[1]
          plottable_df$lt_gret_ratio_20_INC = lina_def.INC$tmp_resls$lt[2] / lina_def.INC$tmp_resls$gret[2]
          plottable_df$lt_gret_ratio_30_INC = lina_def.INC$tmp_resls$lt[3] / lina_def.INC$tmp_resls$gret[3]
          plottable_df$lt_gret_ratio_40_INC = lina_def.INC$tmp_resls$lt[4] / lina_def.INC$tmp_resls$gret[4]
          plottable_df$lt_gret_ratio_50_INC = lina_def.INC$tmp_resls$lt[5] / lina_def.INC$tmp_resls$gret[5]
          plottable_df$lt_gret_ratio_60_INC = lina_def.INC$tmp_resls$lt[6] / lina_def.INC$tmp_resls$gret[6]
          plottable_df$lt_gret_ratio_70_INC = lina_def.INC$tmp_resls$lt[7] / lina_def.INC$tmp_resls$gret[7]
          plottable_df$lt_gret_ratio_80_INC = lina_def.INC$tmp_resls$lt[8] / lina_def.INC$tmp_resls$gret[8]
          plottable_df$lt_gret_ratio_90_INC = lina_def.INC$tmp_resls$lt[9] / lina_def.INC$tmp_resls$gret[9]
          
          # slopes INC
          plottable_df$slope10_INC = lina_def.INC$tmp_resls$slope[1]
          plottable_df$slope20_INC = lina_def.INC$tmp_resls$slope[2]
          plottable_df$slope30_INC = lina_def.INC$tmp_resls$slope[3]
          plottable_df$slope40_INC = lina_def.INC$tmp_resls$slope[4]
          plottable_df$slope50_INC = lina_def.INC$tmp_resls$slope[5]
          plottable_df$slope60_INC = lina_def.INC$tmp_resls$slope[6]
          plottable_df$slope70_INC = lina_def.INC$tmp_resls$slope[7]
          plottable_df$slope80_INC = lina_def.INC$tmp_resls$slope[8]
          plottable_df$slope90_INC = lina_def.INC$tmp_resls$slope[9]
          
          # gret summ stats INC
          plottable_df$Num_gret_INC = lina_def.INC$gret_stats$Num_gret
          plottable_df$Min_gret_INC = lina_def.INC$gret_stats$Min_gret
          plottable_df$Max_gret_INC = lina_def.INC$gret_stats$Max_gret
          plottable_df$Mean_gret_INC = lina_def.INC$gret_stats$Mean_gret
          plottable_df$Mode_gret_INC = lina_def.INC$gret_stats$Mode_gret
          plottable_df$Median_gret_INC = lina_def.INC$gret_stats$Median_gret
          plottable_df$Quantiles_gret0_INC = lina_def.INC$gret_stats$Quantiles_gret[1]
          plottable_df$Quantiles_gret25_INC = lina_def.INC$gret_stats$Quantiles_gret[2]
          plottable_df$Quantiles_gret50_INC = lina_def.INC$gret_stats$Quantiles_gret[3]
          plottable_df$Quantiles_gret75_INC = lina_def.INC$gret_stats$Quantiles_gret[4]
          plottable_df$Quantiles_gret100_INC = lina_def.INC$gret_stats$Quantiles_gret[5]
          plottable_df$SD_gret_INC = lina_def.INC$gret_stats$SD_gret
          plottable_df$Var_gret_INC = lina_def.INC$gret_stats$Var_gret
          plottable_df$Skew_gret_INC = lina_def.INC$gret_stats$Skew_gret
          plottable_df$Span_gret_INC = lina_def.INC$gret_stats$Span_gret
          plottable_df$Varcoeff_gret_INC = lina_def.INC$gret_stats$Varcoeff_gret
          
          # lt summ stats INC
          plottable_df$Num_lt_INC = lina_def.INC$lt_stats$Num_lt
          plottable_df$Min_lt_INC = lina_def.INC$lt_stats$Min_lt
          plottable_df$Max_lt_INC = lina_def.INC$lt_stats$Max_lt
          plottable_df$Mean_lt_INC = lina_def.INC$lt_stats$Mean_lt
          plottable_df$Mode_lt_INC = lina_def.INC$lt_stats$Mode_lt
          plottable_df$Median_lt_INC = lina_def.INC$lt_stats$Median_lt
          plottable_df$Quantiles_lt0_INC = lina_def.INC$lt_stats$Quantiles_lt[1]
          plottable_df$Quantiles_lt25_INC = lina_def.INC$lt_stats$Quantiles_lt[2]
          plottable_df$Quantiles_lt50_INC = lina_def.INC$lt_stats$Quantiles_lt[3]
          plottable_df$Quantiles_lt75_INC = lina_def.INC$lt_stats$Quantiles_lt[4]
          plottable_df$Quantiles_lt100_INC = lina_def.INC$lt_stats$Quantiles_lt[5]
          plottable_df$SD_lt_INC = lina_def.INC$lt_stats$SD_lt
          plottable_df$Var_lt_INC = lina_def.INC$lt_stats$Var_lt
          plottable_df$Skew_lt_INC = lina_def.INC$lt_stats$Skew_lt
          plottable_df$Span_lt_INC = lina_def.INC$lt_stats$Span_lt
          plottable_df$Varcoeff_lt_INC = lina_def.INC$lt_stats$Varcoeff_lt
          
          # slope summ stats INC
          plottable_df$Num_slope_INC = lina_def.INC$slope_stats$Num_slope
          plottable_df$Min_slope_INC = lina_def.INC$slope_stats$Min_slope
          plottable_df$Max_slope_INC = lina_def.INC$slope_stats$Max_slope
          plottable_df$Mean_slope_INC = lina_def.INC$slope_stats$Mean_slope
          plottable_df$Mode_slope_INC = lina_def.INC$slope_stats$Mode_slope
          plottable_df$Median_slope_INC = lina_def.INC$slope_stats$Median_slope
          plottable_df$Quantiles_slope0_INC = lina_def.INC$slope_stats$Quantiles_slope[1]
          plottable_df$Quantiles_slope25_INC = lina_def.INC$slope_stats$Quantiles_slope[2]
          plottable_df$Quantiles_slope50_INC = lina_def.INC$slope_stats$Quantiles_slope[3]
          plottable_df$Quantiles_slope75_INC = lina_def.INC$slope_stats$Quantiles_slope[4]
          plottable_df$Quantiles_slope100_INC = lina_def.INC$slope_stats$Quantiles_slope[5]
          plottable_df$SD_slope_INC = lina_def.INC$slope_stats$SD_slope
          plottable_df$Var_slope_INC = lina_def.INC$slope_stats$Var_slope
          plottable_df$Skew_slope_INC = lina_def.INC$slope_stats$Skew_slope
          plottable_df$Span_slope_INC = lina_def.INC$slope_stats$Span_slope
          plottable_df$Varcoeff_slope_INC = lina_def.INC$slope_stats$Varcoeff_slope
          
          #  AVGFIT
          # grets AVGFIT
          plottable_df$gret10_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[1]
          plottable_df$gret20_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[2]
          plottable_df$gret30_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[3]
          plottable_df$gret40_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[4]
          plottable_df$gret50_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[5]
          plottable_df$gret60_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[6]
          plottable_df$gret70_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[7]
          plottable_df$gret80_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[8]
          plottable_df$gret90_AVGFIT = lina_def.AVGFIT$tmp_resls$gret[9]
          
          # lt AVGFIT
          plottable_df$lt10_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[1]
          plottable_df$lt20_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[2]
          plottable_df$lt30_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[3]
          plottable_df$lt40_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[4]
          plottable_df$lt50_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[5]
          plottable_df$lt60_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[6]
          plottable_df$lt70_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[7]
          plottable_df$lt80_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[8]
          plottable_df$lt90_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[9]
          
          # gret lt ratio AVGFIT
          plottable_df$lt_gret_ratio_10_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[1] / lina_def.AVGFIT$tmp_resls$gret[1]
          plottable_df$lt_gret_ratio_20_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[2] / lina_def.AVGFIT$tmp_resls$gret[2]
          plottable_df$lt_gret_ratio_30_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[3] / lina_def.AVGFIT$tmp_resls$gret[3]
          plottable_df$lt_gret_ratio_40_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[4] / lina_def.AVGFIT$tmp_resls$gret[4]
          plottable_df$lt_gret_ratio_50_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[5] / lina_def.AVGFIT$tmp_resls$gret[5]
          plottable_df$lt_gret_ratio_60_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[6] / lina_def.AVGFIT$tmp_resls$gret[6]
          plottable_df$lt_gret_ratio_70_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[7] / lina_def.AVGFIT$tmp_resls$gret[7]
          plottable_df$lt_gret_ratio_80_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[8] / lina_def.AVGFIT$tmp_resls$gret[8]
          plottable_df$lt_gret_ratio_90_AVGFIT = lina_def.AVGFIT$tmp_resls$lt[9] / lina_def.AVGFIT$tmp_resls$gret[9]
          
          # slopes AVGFIT
          plottable_df$slope10_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[1]
          plottable_df$slope20_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[2]
          plottable_df$slope30_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[3]
          plottable_df$slope40_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[4]
          plottable_df$slope50_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[5]
          plottable_df$slope60_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[6]
          plottable_df$slope70_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[7]
          plottable_df$slope80_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[8]
          plottable_df$slope90_AVGFIT = lina_def.AVGFIT$tmp_resls$slope[9]
          
          # gret summ stats AVGFIT
          plottable_df$Num_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Num_gret
          plottable_df$Min_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Min_gret
          plottable_df$Max_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Max_gret
          plottable_df$Mean_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Mean_gret
          plottable_df$Mode_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Mode_gret
          plottable_df$Median_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Median_gret
          plottable_df$Quantiles_gret0_AVGFIT = lina_def.AVGFIT$gret_stats$Quantiles_gret[1]
          plottable_df$Quantiles_gret25_AVGFIT = lina_def.AVGFIT$gret_stats$Quantiles_gret[2]
          plottable_df$Quantiles_gret50_AVGFIT = lina_def.AVGFIT$gret_stats$Quantiles_gret[3]
          plottable_df$Quantiles_gret75_AVGFIT = lina_def.AVGFIT$gret_stats$Quantiles_gret[4]
          plottable_df$Quantiles_gret100_AVGFIT = lina_def.AVGFIT$gret_stats$Quantiles_gret[5]
          plottable_df$SD_gret_AVGFIT = lina_def.AVGFIT$gret_stats$SD_gret
          plottable_df$Var_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Var_gret
          plottable_df$Skew_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Skew_gret
          plottable_df$Span_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Span_gret
          plottable_df$Varcoeff_gret_AVGFIT = lina_def.AVGFIT$gret_stats$Varcoeff_gret
          
          # lt summ stats AVGFIT
          plottable_df$Num_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Num_lt
          plottable_df$Min_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Min_lt
          plottable_df$Max_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Max_lt
          plottable_df$Mean_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Mean_lt
          plottable_df$Mode_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Mode_lt
          plottable_df$Median_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Median_lt
          plottable_df$Quantiles_lt0_AVGFIT = lina_def.AVGFIT$lt_stats$Quantiles_lt[1]
          plottable_df$Quantiles_lt25_AVGFIT = lina_def.AVGFIT$lt_stats$Quantiles_lt[2]
          plottable_df$Quantiles_lt50_AVGFIT = lina_def.AVGFIT$lt_stats$Quantiles_lt[3]
          plottable_df$Quantiles_lt75_AVGFIT = lina_def.AVGFIT$lt_stats$Quantiles_lt[4]
          plottable_df$Quantiles_lt100_AVGFIT = lina_def.AVGFIT$lt_stats$Quantiles_lt[5]
          plottable_df$SD_lt_AVGFIT = lina_def.AVGFIT$lt_stats$SD_lt
          plottable_df$Var_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Var_lt
          plottable_df$Skew_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Skew_lt
          plottable_df$Span_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Span_lt
          plottable_df$Varcoeff_lt_AVGFIT = lina_def.AVGFIT$lt_stats$Varcoeff_lt
          
          # slope summ stats AVGFIT
          plottable_df$Num_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Num_slope
          plottable_df$Min_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Min_slope
          plottable_df$Max_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Max_slope
          plottable_df$Mean_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Mean_slope
          plottable_df$Mode_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Mode_slope
          plottable_df$Median_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Median_slope
          plottable_df$Quantiles_slope0_AVGFIT = lina_def.AVGFIT$slope_stats$Quantiles_slope[1]
          plottable_df$Quantiles_slope25_AVGFIT = lina_def.AVGFIT$slope_stats$Quantiles_slope[2]
          plottable_df$Quantiles_slope50_AVGFIT = lina_def.AVGFIT$slope_stats$Quantiles_slope[3]
          plottable_df$Quantiles_slope75_AVGFIT = lina_def.AVGFIT$slope_stats$Quantiles_slope[4]
          plottable_df$Quantiles_slope100_AVGFIT = lina_def.AVGFIT$slope_stats$Quantiles_slope[5]
          plottable_df$SD_slope_AVGFIT = lina_def.AVGFIT$slope_stats$SD_slope
          plottable_df$Var_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Var_slope
          plottable_df$Skew_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Skew_slope
          plottable_df$Span_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Span_slope
          plottable_df$Varcoeff_slope_AVGFIT = lina_def.AVGFIT$slope_stats$Varcoeff_slope
          
          #========================================================================================
          # 2.2) Lina consecutive
          #========================================================================================
          lina_cons.INC = lina_consecutive(solvertraj_eff, by_what = 6, which = "incumbant")
          lina_cons.AVGFIT = lina_consecutive(solvertraj_eff, by_what = 6, which = "average.fitness")
          
          # INC
          # grets INC
          plottable_df$gret1_INC_cons = lina_cons.INC$tmp_resls$gret[1]
          plottable_df$gret2_INC_cons = lina_cons.INC$tmp_resls$gret[2]
          plottable_df$gret3_INC_cons = lina_cons.INC$tmp_resls$gret[3]
          plottable_df$gret4_INC_cons = lina_cons.INC$tmp_resls$gret[4]
          plottable_df$gret5_INC_cons = lina_cons.INC$tmp_resls$gret[5]
          plottable_df$gret6_INC_cons = lina_cons.INC$tmp_resls$gret[6]
          
          # lt INC
          plottable_df$lt1_INC_cons = lina_cons.INC$tmp_resls$lt[1]
          plottable_df$lt2_INC_cons = lina_cons.INC$tmp_resls$lt[2]
          plottable_df$lt3_INC_cons = lina_cons.INC$tmp_resls$lt[3]
          plottable_df$lt4_INC_cons = lina_cons.INC$tmp_resls$lt[4]
          plottable_df$lt5_INC_cons = lina_cons.INC$tmp_resls$lt[5]
          plottable_df$lt6_INC_cons = lina_cons.INC$tmp_resls$lt[6]
          
          # gret lt ratio INC
          plottable_df$lt_gret_ratio_1_INC_cons = lina_cons.INC$tmp_resls$lt[1] / lina_cons.INC$tmp_resls$gret[1]
          plottable_df$lt_gret_ratio_2_INC_cons = lina_cons.INC$tmp_resls$lt[2] / lina_cons.INC$tmp_resls$gret[2]
          plottable_df$lt_gret_ratio_3_INC_cons = lina_cons.INC$tmp_resls$lt[3] / lina_cons.INC$tmp_resls$gret[3]
          plottable_df$lt_gret_ratio_4_INC_cons = lina_cons.INC$tmp_resls$lt[4] / lina_cons.INC$tmp_resls$gret[4]
          plottable_df$lt_gret_ratio_5_INC_cons = lina_cons.INC$tmp_resls$lt[5] / lina_cons.INC$tmp_resls$gret[5]
          plottable_df$lt_gret_ratio_6_INC_cons = lina_cons.INC$tmp_resls$lt[6] / lina_cons.INC$tmp_resls$gret[6]
          
          # slopes INC
          plottable_df$slope1_INC = lina_cons.INC$tmp_resls$slope[1]
          plottable_df$slope2_INC = lina_cons.INC$tmp_resls$slope[2]
          plottable_df$slope3_INC = lina_cons.INC$tmp_resls$slope[3]
          plottable_df$slope4_INC = lina_cons.INC$tmp_resls$slope[4]
          plottable_df$slope5_INC = lina_cons.INC$tmp_resls$slope[5]
          plottable_df$slope6_INC = lina_cons.INC$tmp_resls$slope[6]
          
          # gret summ stats INC
          plottable_df$Num_gret_INC_cons = lina_cons.INC$gret_stats$Num_gret
          plottable_df$Min_gret_INC_cons = lina_cons.INC$gret_stats$Min_gret
          plottable_df$Max_gret_INC_cons = lina_cons.INC$gret_stats$Max_gret
          plottable_df$Mean_gret_INC_cons = lina_cons.INC$gret_stats$Mean_gret
          plottable_df$Mode_gret_INC_cons = lina_cons.INC$gret_stats$Mode_gret
          plottable_df$Median_gret_INC_cons = lina_cons.INC$gret_stats$Median_gret
          plottable_df$Quantiles_gret0_INC_cons = lina_cons.INC$gret_stats$Quantiles_gret[1]
          plottable_df$Quantiles_gret25_INC_cons = lina_cons.INC$gret_stats$Quantiles_gret[2]
          plottable_df$Quantiles_gret50_INC_cons = lina_cons.INC$gret_stats$Quantiles_gret[3]
          plottable_df$Quantiles_gret75_INC_cons = lina_cons.INC$gret_stats$Quantiles_gret[4]
          plottable_df$Quantiles_gret100_INC_cons = lina_cons.INC$gret_stats$Quantiles_gret[5]
          plottable_df$SD_gret_INC_cons = lina_cons.INC$gret_stats$SD_gret
          plottable_df$Var_gret_INC_cons = lina_cons.INC$gret_stats$Var_gret
          plottable_df$Skew_gret_INC_cons = lina_cons.INC$gret_stats$Skew_gret
          plottable_df$Span_gret_INC_cons = lina_cons.INC$gret_stats$Span_gret
          plottable_df$Varcoeff_gret_INC_cons = lina_cons.INC$gret_stats$Varcoeff_gret
          
          # lt summ stats INC
          plottable_df$Num_lt_INC_cons = lina_cons.INC$lt_stats$Num_lt
          plottable_df$Min_lt_INC_cons = lina_cons.INC$lt_stats$Min_lt
          plottable_df$Max_lt_INC_cons = lina_cons.INC$lt_stats$Max_lt
          plottable_df$Mean_lt_INC_cons = lina_cons.INC$lt_stats$Mean_lt
          plottable_df$Mode_lt_INC_cons = lina_cons.INC$lt_stats$Mode_lt
          plottable_df$Median_lt_INC_cons = lina_cons.INC$lt_stats$Median_lt
          plottable_df$Quantiles_lt0_INC_cons = lina_cons.INC$lt_stats$Quantiles_lt[1]
          plottable_df$Quantiles_lt25_INC_cons = lina_cons.INC$lt_stats$Quantiles_lt[2]
          plottable_df$Quantiles_lt50_INC_cons = lina_cons.INC$lt_stats$Quantiles_lt[3]
          plottable_df$Quantiles_lt75_INC_cons = lina_cons.INC$lt_stats$Quantiles_lt[4]
          plottable_df$Quantiles_lt100_INC_cons = lina_cons.INC$lt_stats$Quantiles_lt[5]
          plottable_df$SD_lt_INC_cons = lina_cons.INC$lt_stats$SD_lt
          plottable_df$Var_lt_INC_cons = lina_cons.INC$lt_stats$Var_lt
          plottable_df$Skew_lt_INC_cons = lina_cons.INC$lt_stats$Skew_lt
          plottable_df$Span_lt_INC_cons = lina_cons.INC$lt_stats$Span_lt
          plottable_df$Varcoeff_lt_INC_cons = lina_cons.INC$lt_stats$Varcoeff_lt
          
          # slope summ stats INC
          plottable_df$Num_slope_INC_cons = lina_cons.INC$slope_stats$Num_slope
          plottable_df$Min_slope_INC_cons = lina_cons.INC$slope_stats$Min_slope
          plottable_df$Max_slope_INC_cons = lina_cons.INC$slope_stats$Max_slope
          plottable_df$Mean_slope_INC_cons = lina_cons.INC$slope_stats$Mean_slope
          plottable_df$Mode_slope_INC_cons = lina_cons.INC$slope_stats$Mode_slope
          plottable_df$Median_slope_INC_cons = lina_cons.INC$slope_stats$Median_slope
          plottable_df$Quantiles_slope0_INC_cons = lina_cons.INC$slope_stats$Quantiles_slope[1]
          plottable_df$Quantiles_slope25_INC_cons = lina_cons.INC$slope_stats$Quantiles_slope[2]
          plottable_df$Quantiles_slope50_INC_cons = lina_cons.INC$slope_stats$Quantiles_slope[3]
          plottable_df$Quantiles_slope75_INC_cons = lina_cons.INC$slope_stats$Quantiles_slope[4]
          plottable_df$Quantiles_slope100_INC_cons = lina_cons.INC$slope_stats$Quantiles_slope[5]
          plottable_df$SD_slope_INC_cons = lina_cons.INC$slope_stats$SD_slope
          plottable_df$Var_slope_INC_cons = lina_cons.INC$slope_stats$Var_slope
          plottable_df$Skew_slope_INC_cons = lina_cons.INC$slope_stats$Skew_slope
          plottable_df$Span_slope_INC_cons = lina_cons.INC$slope_stats$Span_slope
          plottable_df$Varcoeff_slope_INC_cons = lina_cons.INC$slope_stats$Varcoeff_slope
          
          # AVGFIT
          # grets AVGFIT
          plottable_df$gret1_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$gret[1]
          plottable_df$gret2_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$gret[2]
          plottable_df$gret3_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$gret[3]
          plottable_df$gret4_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$gret[4]
          plottable_df$gret5_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$gret[5]
          plottable_df$gret6_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$gret[6]
          
          # lt AVGFIT
          plottable_df$lt1_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[1]
          plottable_df$lt2_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[2]
          plottable_df$lt3_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[3]
          plottable_df$lt4_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[4]
          plottable_df$lt5_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[5]
          plottable_df$lt6_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[6]
          
          # gret lt ratio AVGFIT
          plottable_df$lt_gret_ratio_1_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[1] / lina_cons.AVGFIT$tmp_resls$gret[1]
          plottable_df$lt_gret_ratio_2_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[2] / lina_cons.AVGFIT$tmp_resls$gret[2]
          plottable_df$lt_gret_ratio_3_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[3] / lina_cons.AVGFIT$tmp_resls$gret[3]
          plottable_df$lt_gret_ratio_4_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[4] / lina_cons.AVGFIT$tmp_resls$gret[4]
          plottable_df$lt_gret_ratio_5_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[5] / lina_cons.AVGFIT$tmp_resls$gret[5]
          plottable_df$lt_gret_ratio_6_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$lt[6] / lina_cons.AVGFIT$tmp_resls$gret[6]
          
          # slopes AVGFIT
          plottable_df$slope1_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$slope[1]
          plottable_df$slope2_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$slope[2]
          plottable_df$slope3_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$slope[3]
          plottable_df$slope4_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$slope[4]
          plottable_df$slope5_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$slope[5]
          plottable_df$slope6_AVGFIT_cons = lina_cons.AVGFIT$tmp_resls$slope[6]
          
          # gret summ stats AVGFIT
          plottable_df$Num_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Num_gret
          plottable_df$Min_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Min_gret
          plottable_df$Max_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Max_gret
          plottable_df$Mean_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Mean_gret
          plottable_df$Mode_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Mode_gret
          plottable_df$Median_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Median_gret
          plottable_df$Quantiles_gret0_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Quantiles_gret[1]
          plottable_df$Quantiles_gret25_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Quantiles_gret[2]
          plottable_df$Quantiles_gret50_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Quantiles_gret[3]
          plottable_df$Quantiles_gret75_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Quantiles_gret[4]
          plottable_df$Quantiles_gret100_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Quantiles_gret[5]
          plottable_df$SD_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$SD_gret
          plottable_df$Var_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Var_gret
          plottable_df$Skew_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Skew_gret
          plottable_df$Span_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Span_gret
          plottable_df$Varcoeff_gret_AVGFIT_cons = lina_cons.AVGFIT$gret_stats$Varcoeff_gret
          
          # lt summ stats AVGFIT
          plottable_df$Num_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Num_lt
          plottable_df$Min_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Min_lt
          plottable_df$Max_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Max_lt
          plottable_df$Mean_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Mean_lt
          plottable_df$Mode_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Mode_lt
          plottable_df$Median_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Median_lt
          plottable_df$Quantiles_lt0_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Quantiles_lt[1]
          plottable_df$Quantiles_lt25_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Quantiles_lt[2]
          plottable_df$Quantiles_lt50_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Quantiles_lt[3]
          plottable_df$Quantiles_lt75_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Quantiles_lt[4]
          plottable_df$Quantiles_lt100_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Quantiles_lt[5]
          plottable_df$SD_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$SD_lt
          plottable_df$Var_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Var_lt
          plottable_df$Skew_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Skew_lt
          plottable_df$Span_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Span_lt
          plottable_df$Varcoeff_lt_AVGFIT_cons = lina_cons.AVGFIT$lt_stats$Varcoeff_lt
          
          # slope summ stats AVGFIT
          plottable_df$Num_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Num_slope
          plottable_df$Min_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Min_slope
          plottable_df$Max_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Max_slope
          plottable_df$Mean_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Mean_slope
          plottable_df$Mode_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Mode_slope
          plottable_df$Median_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Median_slope
          plottable_df$Quantiles_slope0_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Quantiles_slope[1]
          plottable_df$Quantiles_slope25_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Quantiles_slope[2]
          plottable_df$Quantiles_slope50_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Quantiles_slope[3]
          plottable_df$Quantiles_slope75_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Quantiles_slope[4]
          plottable_df$Quantiles_slope100_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Quantiles_slope[5]
          plottable_df$SD_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$SD_slope
          plottable_df$Var_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Var_slope
          plottable_df$Skew_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Skew_slope
          plottable_df$Span_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Span_slope
          plottable_df$Varcoeff_slope_AVGFIT_cons = lina_cons.AVGFIT$slope_stats$Varcoeff_slope
          
          #========================================================================================
          # 3) time related features
          #========================================================================================
          time_after_iteration_X_stats = time_X_stats(solvertraj_eff)
          
          # time_passed_iter_X
          plottable_df$time_passed_after_10perc = time_after_iteration_X_stats$time_passed_iter_X[1]
          plottable_df$time_passed_after_20perc = time_after_iteration_X_stats$time_passed_iter_X[2]
          plottable_df$time_passed_after_30perc = time_after_iteration_X_stats$time_passed_iter_X[3]
          plottable_df$time_passed_after_40perc = time_after_iteration_X_stats$time_passed_iter_X[4]
          plottable_df$time_passed_after_50perc = time_after_iteration_X_stats$time_passed_iter_X[5]
          plottable_df$time_passed_after_60perc = time_after_iteration_X_stats$time_passed_iter_X[1]
          plottable_df$time_passed_after_70perc = time_after_iteration_X_stats$time_passed_iter_X[7]
          plottable_df$time_passed_after_80perc = time_after_iteration_X_stats$time_passed_iter_X[8]
          plottable_df$time_passed_after_90perc = time_after_iteration_X_stats$time_passed_iter_X[9]
          
          # time_passed_iter_diff
          plottable_df$time_passed_iter_diff_10_20 = time_after_iteration_X_stats$time_passed_iter_diff[1]
          plottable_df$time_passed_iter_diff_20_30 = time_after_iteration_X_stats$time_passed_iter_diff[2]
          plottable_df$time_passed_iter_diff_30_40 = time_after_iteration_X_stats$time_passed_iter_diff[3]
          plottable_df$time_passed_iter_diff_40_50 = time_after_iteration_X_stats$time_passed_iter_diff[4]
          plottable_df$time_passed_iter_diff_50_60 = time_after_iteration_X_stats$time_passed_iter_diff[5]
          plottable_df$time_passed_iter_diff_60_70 = time_after_iteration_X_stats$time_passed_iter_diff[6]
          plottable_df$time_passed_iter_diff_70_80 = time_after_iteration_X_stats$time_passed_iter_diff[7]
          plottable_df$time_passed_iter_diff_80_90 = time_after_iteration_X_stats$time_passed_iter_diff[8]
          
          # time_passed_iter_X_diff_stats
          plottable_df$Num_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Num_time_X
          plottable_df$Min_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Min_time_X
          plottable_df$Max_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Max_time_X
          plottable_df$Mean_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Mean_time_X
          plottable_df$Mode_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Mode_time_X
          plottable_df$Median_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Median_time_X
          plottable_df$Quantiles_time_X0 = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Quantiles_time_X[1]
          plottable_df$Quantiles_time_X25 = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Quantiles_time_X[2]
          plottable_df$Quantiles_time_X50 = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Quantiles_time_X[3]
          plottable_df$Quantiles_time_X75 = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Quantiles_time_X[4]
          plottable_df$Quantiles_time_X100 = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Quantiles_time_X[5]
          plottable_df$SD_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$SD_time_X
          plottable_df$Var_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Var_time_X
          plottable_df$Skew_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Skew_time_X
          plottable_df$Span_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Span_time_X
          plottable_df$Varcoeff_time_X = time_after_iteration_X_stats$time_passed_iter_X_diff_stats$Varcoeff_time_X
          
          #========================================================================================
          # 4) Covergence Speed 1, 2, and 3
          #========================================================================================
          solvertraj_eff = getAreaAVGInc(solvertraj_eff, FALSE)
          ConvSpeed_1 = getConvSpeed_1(solvertraj_eff, TRUE)
          
          plottable_df$convPerIter_AVG = ConvSpeed_1$convIter # ((lastAreaVal / firstAreaVal) / n) 
          plottable_df$convPerIter_AVG_2 = ConvSpeed_1$convIter_2 # ((firstAreaVal - lastAreaVal) / n) 
          plottable_df$convPerTime_AVG = ConvSpeed_1$convTime
          plottable_df$convPerTime_AVG_2 = ConvSpeed_1$convTime_2
          
          plottable_df$convOverIter = ConvSpeed_1$area_improvement_iter
          #plottable_df$convPerIter = ConvSpeed_1$area_improvement_time  # TODO: fix
          plottable_df$Num_rectangle_iter = ConvSpeed_1$rect_stats_iter$Num_rectangle_iter
          plottable_df$Min_rectangle_iter = ConvSpeed_1$rect_stats_iter$Min_rectangle_iter
          plottable_df$Max_rectangle_iter = ConvSpeed_1$rect_stats_iter$Max_rectangle_iter
          plottable_df$Mean_rectangle_iter = ConvSpeed_1$rect_stats_iter$Mean_rectangle_iter
          plottable_df$Mode_rectangle_iter = ConvSpeed_1$rect_stats_iter$Mode_rectangle_iter
          plottable_df$Median_rectangle_iter = ConvSpeed_1$rect_stats_iter$Median_rectangle_iter
          plottable_df$Quantiles_rectangle_iter0 = ConvSpeed_1$rect_stats_iter$Quantiles_rectangle_iter[1] %>% as.numeric(.)
          plottable_df$Quantiles_rectangle_iter25 = ConvSpeed_1$rect_stats_iter$Quantiles_rectangle_iter[2] %>% as.numeric(.)
          plottable_df$Quantiles_rectangle_iter50 = ConvSpeed_1$rect_stats_iter$Quantiles_rectangle_iter[3] %>% as.numeric(.)
          plottable_df$Quantiles_rectangle_iter75 = ConvSpeed_1$rect_stats_iter$Quantiles_rectangle_iter[4] %>% as.numeric(.)
          plottable_df$Quantiles_rectangle_iter100 = ConvSpeed_1$rect_stats_iter$Quantiles_rectangle_iter[5] %>% as.numeric(.)
          plottable_df$SD_rectangle_iter = ConvSpeed_1$rect_stats_iter$SD_rectangle_iter
          plottable_df$Var_rectangle_iter = ConvSpeed_1$rect_stats_iter$Var_rectangle_iter
          plottable_df$Skew_rectangle_iter = ConvSpeed_1$rect_stats_iter$Skew_rectangle_iter
          plottable_df$Span_rectangle_iter = ConvSpeed_1$rect_stats_iter$Span_rectangle_iter
          plottable_df$Varcoeff_rectangle_iter = ConvSpeed_1$rect_stats_iter$Varcoeff_rectangle_iter
          
          areasls = calcTrigonometricAreas(solvertraj_eff, TRUE, TRUE) 
          plottable_df$triangle = areasls$triangle
          plottable_df$tri_peak= areasls$tri_peak
          plottable_df$tri_sideA = areasls$tri_sideA
          plottable_df$tri_sideB = areasls$tri_sideB
          plottable_df$tri_sideC = areasls$tri_sideC
          plottable_df$trapezoid = areasls$trapezoid
          
          ConvSpeed_2 = getConvSpeed_2(solvertraj_eff, areasls$triangle, areasls$trapezoid)
          plottable_df$convergence_idealized = ConvSpeed_2$convergence_idealized
          plottable_df$convergence_idealized_norm = ConvSpeed_2$convergence_idealized_norm
          plottable_df$convergence_step_tri = ConvSpeed_2$convergence_step_tri
          quality_indication = getConvQuality(solvertraj_eff) 
          plottable_df$dist_begin = quality_indication$dist_begin
          plottable_df$dist_end = quality_indication$dist_end
          plottable_df$quality_drift = quality_indication$quality_drift
          
          ConvSpeed_3 = getConvSpeed_3(solvertraj_eff)
          plottable_df$convSpeed_3 = ConvSpeed_3$conv_speed
          plottable_df$incumbant_slope = ConvSpeed_3$slopes$incumbant
          plottable_df$average.fitness_slope = ConvSpeed_3$slopes$average.fitness
          
          #========================================================================================
          # 5) IncLB AREA
          #========================================================================================
          black.list = c("VRP")
          instance = instance
          sum_of_lowest_edge_values = try.getFeatureSet(black.list, instance)
          
          # important: only add the LB_scaled to mon_feat of TRUE
          if(scaling == TRUE){
            LB_scaled = scalerOwn.scale_Scalar(LB = sum_of_lowest_edge_values, min_inc = tmp_min_inc, max_inc = tmp_max_inc)
            plottable_df$sum_of_lowest_edge_values_SCALED = LB_scaled
          }
          # +++ new +++: 
          inc_lb_stat.INC = getIncLB_area(solvertraj_eff, lb = 0, ub = "incumbant")
          inc_lb_stat.AVGFIT = getIncLB_area(solvertraj_eff, lb = 0, ub = "average.fitness")
          
          inc_lb_stat_copy.INC = getIncLB_area(solvertraj_real, lb = 0, ub = "incumbant")
          inc_lb_stat_copy.AVGGIT = getIncLB_area(solvertraj_real, lb = 0, ub = "average.fitness")
          # +++ new +++
          plottable_df$sum_of_lowest_edge_values_ORIG = sum_of_lowest_edge_values
          # updated:
          plottable_df$IncLB_area_INC = inc_lb_stat.INC$area_sum
          plottable_df$IncLB_area_AVGFIT = inc_lb_stat.AVGFIT$area_sum
          # +++ new +++
          plottable_df$IncLB_INC_AVGFFIT_diff = inc_lb_stat.AVGFIT$area_sum - inc_lb_stat.INC$area_sum
          
          plottable_df$Num_areas_INC = inc_lb_stat.INC$area_stats$Num_areas
          plottable_df$Min_areas_INC = inc_lb_stat.INC$area_stats$Min_areas
          plottable_df$Max_areas_INC = inc_lb_stat.INC$area_stats$Max_areas
          plottable_df$Mean_areas_INC = inc_lb_stat.INC$area_stats$Mean_areas
          plottable_df$Mode_areas_INC = inc_lb_stat.INC$area_stats$Mode_areas
          plottable_df$Median_areas_INC = inc_lb_stat.INC$area_stats$Median_areas
          plottable_df$Quantiles_areas0_INC = inc_lb_stat.INC$area_stats$Quantiles_areas[1]
          plottable_df$Quantiles_areas25_INC = inc_lb_stat.INC$area_stats$Quantiles_areas[2]
          plottable_df$Quantiles_areas50_INC = inc_lb_stat.INC$area_stats$Quantiles_areas[3]
          plottable_df$Quantiles_areas75_INC = inc_lb_stat.INC$area_stats$Quantiles_areas[4]
          plottable_df$Quantiles_areas100_INC = inc_lb_stat.INC$area_stats$Quantiles_areas[5]
          plottable_df$SD_areas_INC = inc_lb_stat.INC$area_stats$SD_areas
          plottable_df$Var_areas_INC = inc_lb_stat.INC$area_stats$Var_areas
          plottable_df$Skew_areas_INC = inc_lb_stat.INC$area_stats$Skew_areas
          plottable_df$Span_areas_INC = inc_lb_stat.INC$area_stats$Span_areas
          plottable_df$Varcoeff_areas_INC = inc_lb_stat.INC$area_stats$Varcoeff_areas
          
          # +++ new +++:
          plottable_df$Num_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Num_areas
          plottable_df$Min_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Min_areas
          plottable_df$Max_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Max_areas
          plottable_df$Mean_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Mean_areas
          plottable_df$Mode_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Mode_areas
          plottable_df$Median_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Median_areas
          plottable_df$Quantiles_areas0_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Quantiles_areas[1]
          plottable_df$Quantiles_areas25_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Quantiles_areas[2]
          plottable_df$Quantiles_areas50_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Quantiles_areas[3]
          plottable_df$Quantiles_areas75_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Quantiles_areas[4]
          plottable_df$Quantiles_areas100_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Quantiles_areas[5]
          plottable_df$SD_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$SD_areas
          plottable_df$Var_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Var_areas
          plottable_df$Skew_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Skew_areas
          plottable_df$Span_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Span_areas
          plottable_df$Varcoeff_areas_AVGFIT = inc_lb_stat.AVGFIT$area_stats$Varcoeff_areas
          
          # +++ new +++
          plottable_df$Inc_Avg_diff = inc_lb_stat.AVGFIT$area_sum - inc_lb_stat.INC$area_sum
          area_inc_lb_ratio.INC = inc_lb_stat.INC$area_sum / inc_lb_stat_copy.INC$area_sum
          plottable_df$area_inc_lb_ratio.INC = area_inc_lb_ratio.INC
          
          # +++ new +++:
          area_inc_lb_ratio.AVGFIT = inc_lb_stat.AVGFIT$area_sum / inc_lb_stat_copy.AVGGIT$area_sum
          plottable_df$area_inc_lb_ratio.AVGFIT = area_inc_lb_ratio.AVGFIT
          
          # +++ new +++:
          area_inc_lb_ratio.INC_AVGFIT_diff = area_inc_lb_ratio.INC - area_inc_lb_ratio.AVGFIT
          plottable_df$area_inc_lb_ratio.INC_AVGFIT_diff = area_inc_lb_ratio.INC_AVGFIT_diff
          
          #========================================================================================
          # 6) success ratio
          #========================================================================================
          success_ratio = getSuccessRatio(solvertraj_eff)
          plottable_df$success_ratio = success_ratio$successRatio
          plottable_df$not_successful = success_ratio$num_NoSuccess
          #plottable_df$num_total = success_ratio$num_total
          
          #========================================================================================
          # 7) Plateau Statistics 
          #========================================================================================
          if(attr(solvertraj_eff,"plateaunized") == TRUE) {
            Plat_stats = getPlateauStats(solvertraj_eff, success_ratio$successRatio)
            
            plottable_df$Num_Plateau_Length = Plat_stats$plat_stats$Num_Plateau_Length
            plottable_df$Min_Plateau_Length = Plat_stats$plat_stats$Min_Plateau_Length
            plottable_df$Max_Plateau_Length = Plat_stats$plat_stats$Max_Plateau_Length
            plottable_df$Mean_Plateau_Length = Plat_stats$plat_stats$Mean_Plateau_Length
            plottable_df$Mode_Plateau_Length = Plat_stats$plat_stats$Mode_Plateau_Length
            plottable_df$Median_Plateau_Length = Plat_stats$plat_stats$Median_Plateau_Length
            plottable_df$Quantiles_Plateau_Length0 = Plat_stats$plat_stats$Quantiles_Plateau_Length[1]
            plottable_df$Quantiles_Plateau_Length25 = Plat_stats$plat_stats$Quantiles_Plateau_Length[2]
            plottable_df$Quantiles_Plateau_Length50 = Plat_stats$plat_stats$Quantiles_Plateau_Length[3]
            plottable_df$Quantiles_Plateau_Length75 = Plat_stats$plat_stats$Quantiles_Plateau_Length[4]
            plottable_df$Quantiles_Plateau_Length100 = Plat_stats$plat_stats$Quantiles_Plateau_Length[5]
            plottable_df$SD_Plateau_Length = Plat_stats$plat_stats$SD_Plateau_Length
            plottable_df$Var_Plateau_Length = Plat_stats$plat_stats$Var_Plateau_Length
            plottable_df$Skew_Plateau_Length = Plat_stats$plat_stats$Skew_Plateau_Length
            plottable_df$Span_Plateau_Length = Plat_stats$plat_stats$Span_Plateau_Length
            plottable_df$Varcoeff_Plateau_Length = Plat_stats$plat_stats$Varcoeff_Plateau_Length
          } else {
            plottable_df$Num_Plateau_Length = 0L   # or NA? 
            plottable_df$Min_Plateau_Length = 0L
            plottable_df$Max_Plateau_Length = 0L
            plottable_df$Mean_Plateau_Length = 0L
            plottable_df$Mode_Plateau_Length = 0L
            plottable_df$Median_Plateau_Length = 0L
            plottable_df$Quantiles_Plateau_Length0 = 0L
            plottable_df$Quantiles_Plateau_Length25 = 0L
            plottable_df$Quantiles_Plateau_Length50 = 0L
            plottable_df$Quantiles_Plateau_Length75 = 0L
            plottable_df$Quantiles_Plateau_Length100 = 0L
            plottable_df$SD_Plateau_Length = 0L
            plottable_df$Var_Plateau_Length = 0L
            plottable_df$Skew_Plateau_Length = 0L
            plottable_df$Span_Plateau_Length = 0L
            plottable_df$Varcoeff_Plateau_Length = 0L
          }
          
          #========================================================================================
          # 8) Plateau Start Statistics EFF
          #========================================================================================
          if(attr(solvertraj_eff, "plateaunized") == TRUE) {
            Plat_start_stats = getPlateauStartStats(solvertraj_eff, 4, success_ratio$successRatio)
          }
          
          if(exists("Plat_start_stats") & !is.na(Plat_start_stats$plat_start$Plats_area_begin_X)){
            plottable_df$plat_start_X = Plat_start_stats$plat_start$Plats_area_begin_X
            plottable_df$Plats_area_begin_X_abs = Plat_start_stats$plat_start$Plats_area_begin_X_abs
            plottable_df$plat_start_Y = Plat_start_stats$plat_start$Plats_area_begin_Y
            plottable_df$Plats_area_begin_Y_abs = Plat_start_stats$plat_start$Plats_area_begin_Y_abs
            
            plottable_df$Num_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Num_plat_start_X_abs
            plottable_df$Min_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Min_plat_start_X_abs
            plottable_df$Max_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Max_plat_start_X_abs
            plottable_df$Mean_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Mean_plat_start_X_abs
            plottable_df$Mode_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Mode_plat_start_X_abs
            plottable_df$Median_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Median_plat_start_X_abs
            plottable_df$Quantiles_plat_start_X_abs0 = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[1]
            plottable_df$Quantiles_plat_start_X_abs25 = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[2]
            plottable_df$Quantiles_plat_start_X_abs50 = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[3]
            plottable_df$Quantiles_plat_start_X_abs75 = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[4]
            plottable_df$Quantiles_plat_start_X_abs100 = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[5]
            plottable_df$SD_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$SD_plat_start_X_abs
            plottable_df$Var_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Var_plat_start_X_abs
            plottable_df$Skew_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Skew_plat_start_X_abs
            plottable_df$Span_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Span_plat_start_X_abs
            plottable_df$Varcoeff_plat_start_X_abs = Plat_start_stats$plat_start$plat_start_X_abs_Stats$Varcoeff_plat_start_X_abs
            
            # +++ new +++ : X_rel, Y_abs
            plottable_df$Num_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Num_plat_start_X_rel
            plottable_df$Min_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Min_plat_start_X_rel
            plottable_df$Max_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Max_plat_start_X_rel
            plottable_df$Mean_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Mean_plat_start_X_rel
            plottable_df$Mode_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Mode_plat_start_X_rel
            plottable_df$Median_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Median_plat_start_X_rel
            plottable_df$Quantiles_plat_start_X_rel0 = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[1]
            plottable_df$Quantiles_plat_start_X_rel25 = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[2]
            plottable_df$Quantiles_plat_start_X_rel50 = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[3]
            plottable_df$Quantiles_plat_start_X_rel75 = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[4]
            plottable_df$Quantiles_plat_start_X_rel100 = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[5]
            plottable_df$SD_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$SD_plat_start_X_rel
            plottable_df$Var_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Var_plat_start_X_rel
            plottable_df$Skew_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Skew_plat_start_X_rel
            plottable_df$Span_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Span_plat_start_X_rel
            plottable_df$Varcoeff_plat_start_X_rel = Plat_start_stats$plat_start$plat_start_X_rel_Stats$Varcoeff_plat_start_X_rel
            
            plottable_df$Num_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Num_plat_start_Y_abs
            plottable_df$Min_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Min_plat_start_Y_abs
            plottable_df$Max_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Max_plat_start_Y_abs
            plottable_df$Mean_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Mean_plat_start_Y_abs
            plottable_df$Mode_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Mode_plat_start_Y_abs
            plottable_df$Median_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Median_plat_start_Y_abs
            plottable_df$Quantiles_plat_start_Y_abs0 = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[1]
            plottable_df$Quantiles_plat_start_Y_abs25 = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[2]
            plottable_df$Quantiles_plat_start_Y_abs50 = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[3]
            plottable_df$Quantiles_plat_start_Y_abs75 = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[4]
            plottable_df$Quantiles_plat_start_Y_abs100 = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[5]
            plottable_df$SD_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$SD_plat_start_Y_abs
            plottable_df$Var_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Var_plat_start_Y_abs
            plottable_df$Skew_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Skew_plat_start_Y_abs
            plottable_df$Span_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Span_plat_start_Y_abs
            plottable_df$Varcoeff_plat_start_Y_abs = Plat_start_stats$plat_start$plat_start_Y_abs_Stats$Varcoeff_plat_start_Y_abs
            
            plottable_df$Num_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Num_plat_start_Y_rel
            plottable_df$Min_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Min_plat_start_Y_rel
            plottable_df$Max_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Max_plat_start_Y_rel
            plottable_df$Mean_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Mean_plat_start_Y_rel
            plottable_df$Mode_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Mode_plat_start_Y_rel
            plottable_df$Median_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Median_plat_start_Y_rel
            plottable_df$Quantiles_plat_start_Y_rel0 = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[1]
            plottable_df$Quantiles_plat_start_Y_rel25 = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[2]
            plottable_df$Quantiles_plat_start_Y_rel50 = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[3]
            plottable_df$Quantiles_plat_start_Y_rel75 = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[4]
            plottable_df$Quantiles_plat_start_Y_rel100 = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[5]
            plottable_df$SD_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$SD_plat_start_Y_rel
            plottable_df$Var_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Var_plat_start_Y_rel
            plottable_df$Skew_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Skew_plat_start_Y_rel
            plottable_df$Span_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Span_plat_start_Y_rel
            plottable_df$Varcoeff_plat_start_Y_rel = Plat_start_stats$plat_start$plat_start_Y_rel_Stats$Varcoeff_plat_start_Y_rel
            
            plottable_df$characteristic_volume_X = Plat_start_stats$plat_start$characteristic_volume_X
            plottable_df$characteristic_volume_Y =Plat_start_stats$plat_start$characteristic_volume_Y
            plottable_df$characteristic_volume = Plat_start_stats$plat_start$characteristic_volume
          } else {
            plottable_df$plat_start_X = 0L                  
            plottable_df$Plats_area_begin_X_abs = 0L
            plottable_df$plat_start_Y = 0L
            plottable_df$Plats_area_begin_Y_abs = 0L
            
            plottable_df$Num_plat_start_X_abs = 0L
            plottable_df$Min_plat_start_X_abs = 0L
            plottable_df$Max_plat_start_X_abs = 0L
            plottable_df$Mean_plat_start_X_abs = 0L
            plottable_df$Mode_plat_start_X_abs = 0L
            plottable_df$Median_plat_start_X_abs = 0L
            plottable_df$Quantiles_plat_start_X_abs0 = 0L
            plottable_df$Quantiles_plat_start_X_abs25 = 0L
            plottable_df$Quantiles_plat_start_X_abs50 = 0L
            plottable_df$Quantiles_plat_start_X_abs75 = 0L
            plottable_df$Quantiles_plat_start_X_abs100 = 0L
            plottable_df$SD_plat_start_X_abs = 0L
            plottable_df$Var_plat_start_X_abs = 0L
            plottable_df$Skew_plat_start_X_abs = 0L
            plottable_df$Span_plat_start_X_abs = 0L
            plottable_df$Varcoeff_plat_start_X_abs = 0L
            
            # +++ new +++ : X_rel, Y_abs
            plottable_df$Num_plat_start_X_rel = 0L
            plottable_df$Min_plat_start_X_rel = 0L
            plottable_df$Max_plat_start_X_rel = 0L
            plottable_df$Mean_plat_start_X_rel = 0L
            plottable_df$Mode_plat_start_X_rel = 0L
            plottable_df$Median_plat_start_X_rel = 0L
            plottable_df$Quantiles_plat_start_X_rel0 = 0L
            plottable_df$Quantiles_plat_start_X_rel25 = 0L
            plottable_df$Quantiles_plat_start_X_rel50 = 0L
            plottable_df$Quantiles_plat_start_X_rel75 = 0L
            plottable_df$Quantiles_plat_start_X_rel100 = 0L
            plottable_df$SD_plat_start_X_rel = 0L
            plottable_df$Var_plat_start_X_rel = 0L
            plottable_df$Skew_plat_start_X_rel = 0L
            plottable_df$Span_plat_start_X_rel = 0L
            plottable_df$Varcoeff_plat_start_X_rel = 0L
            
            plottable_df$Num_plat_start_Y_abs = 0L
            plottable_df$Min_plat_start_Y_abs = 0L
            plottable_df$Max_plat_start_Y_abs = 0L
            plottable_df$Mean_plat_start_Y_abs = 0L
            plottable_df$Mode_plat_start_Y_abs = 0L
            plottable_df$Median_plat_start_Y_abs = 0L
            plottable_df$Quantiles_plat_start_Y_abs0 = 0L
            plottable_df$Quantiles_plat_start_Y_abs25 = 0L
            plottable_df$Quantiles_plat_start_Y_abs50 = 0L
            plottable_df$Quantiles_plat_start_Y_abs75 = 0L
            plottable_df$Quantiles_plat_start_Y_abs100 = 0L
            plottable_df$SD_plat_start_Y_abs = 0L
            plottable_df$Var_plat_start_Y_abs = 0L
            plottable_df$Skew_plat_start_Y_abs = 0L
            plottable_df$Span_plat_start_Y_abs = 0L
            plottable_df$Varcoeff_plat_start_Y_abs = 0L
            
            plottable_df$Num_plat_start_Y_rel = 0L
            plottable_df$Min_plat_start_Y_rel = 0L
            plottable_df$Max_plat_start_Y_rel = 0L
            plottable_df$Mean_plat_start_Y_rel = 0L
            plottable_df$Mode_plat_start_Y_rel = 0L
            plottable_df$Median_plat_start_Y_rel = 0L
            plottable_df$Quantiles_plat_start_Y_rel0 = 0L
            plottable_df$Quantiles_plat_start_Y_rel25 = 0L
            plottable_df$Quantiles_plat_start_Y_rel50 = 0L
            plottable_df$Quantiles_plat_start_Y_rel75 = 0L
            plottable_df$Quantiles_plat_start_Y_rel100 = 0L
            plottable_df$SD_plat_start_Y_rel = 0L
            plottable_df$Var_plat_start_Y_rel = 0L
            plottable_df$Skew_plat_start_Y_rel = 0L
            plottable_df$Span_plat_start_Y_rel = 0L
            plottable_df$Varcoeff_plat_start_Y_rel = 0L
            
            plottable_df$characteristic_volume_X = 0L
            plottable_df$characteristic_volume_Y = 0L
            plottable_df$characteristic_volume = 0L
          }
          
          #========================================================================================
          # 9) vertical gaps
          #========================================================================================
          VG_stats = getVG_stats(solvertraj_eff)
          plottable_df$vg_threshold = VG_stats$vg_stats$vg_threshold
          
          plottable_df$Num_Vertical_Gaps = VG_stats$vg_stats$Num_Vertical_Gaps
          plottable_df$Min_Vertical_Gaps = VG_stats$vg_stats$Min_Vertical_Gaps
          plottable_df$Max_Vertical_Gaps = VG_stats$vg_stats$Max_Vertical_Gaps
          plottable_df$Mean_Vertical_Gaps = VG_stats$vg_stats$Mean_Vertical_Gaps
          plottable_df$Mode_Vertical_Gaps = VG_stats$vg_stats$Mode_Vertical_Gaps
          plottable_df$Median_Vertical_Gaps = VG_stats$vg_stats$Median_Vertical_Gaps
          plottable_df$Quantiles_Vertical_Gaps0 = VG_stats$vg_stats$Quantiles_Vertical_Gaps[1]
          plottable_df$Quantiles_Vertical_Gaps25 = VG_stats$vg_stats$Quantiles_Vertical_Gaps[2]
          plottable_df$Quantiles_Vertical_Gaps50= VG_stats$vg_stats$Quantiles_Vertical_Gaps[3]
          plottable_df$Quantiles_Vertical_Gaps75 = VG_stats$vg_stats$Quantiles_Vertical_Gaps[4]
          plottable_df$Quantiles_Vertical_Gaps100 = VG_stats$vg_stats$Quantiles_Vertical_Gaps[5]
          plottable_df$SD_Vertical_Gaps = VG_stats$vg_stats$SD_Vertical_Gaps
          plottable_df$Var_Vertical_Gaps = VG_stats$vg_stats$Var_Vertical_Gaps
          plottable_df$Skew_Vertical_Gaps = VG_stats$vg_stats$Skew_Vertical_Gaps
          plottable_df$Span_Vertical_Gaps = VG_stats$vg_stats$Span_Vertical_Gaps
          plottable_df$Varcoeff_Vertical_Gaps = VG_stats$vg_stats$Varcoeff_Vertical_Gaps
          
          # new: requires default stats: span incumbent
          plottable_df$meanVG_Span_ratio = VG_stats$vg_stats$Mean_Vertical_Gaps / 
            default_traj_stats$incumbent_stat_ls_eff$Span_incumbent
          
          #========================================================================================
          # 10) vertical gaps Start
          #========================================================================================
          VG_start_stats = getVGStartStats(solvertraj_eff, VG_stats, 5, TRUE) 
          
          plottable_df$impro_ratio_10 = (VG_start_stats$ratio_first_perc_ls[1] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$impro_ratio_20 = (VG_start_stats$ratio_first_perc_ls[2] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$impro_ratio_30 = (VG_start_stats$ratio_first_perc_ls[3] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$impro_ratio_40 = (VG_start_stats$ratio_first_perc_ls[4] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$impro_ratio_50 = (VG_start_stats$ratio_first_perc_ls[5] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_amnt_10 = (VG_start_stats$amnt_ls[1] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_amnt_20 = (VG_start_stats$amnt_ls[2] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_amnt_30 = (VG_start_stats$amnt_ls[3] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_amnt_40 = (VG_start_stats$amnt_ls[4] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_amnt_50 = (VG_start_stats$amnt_ls[5] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_ratio_10 = (VG_start_stats$ratio_ls[1] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_ratio_20 = (VG_start_stats$ratio_ls[2] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_ratio_30 = (VG_start_stats$ratio_ls[3] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_ratio_40 = (VG_start_stats$ratio_ls[4] %>% unlist(.)) %>% as.numeric(.)
          plottable_df$VG_ratio_50 = (VG_start_stats$ratio_ls[5] %>% unlist(.)) %>% as.numeric(.)
          
          plottable_df$firstVG_X_abs = VG_start_stats$firstVG_X_abs
          plottable_df$lastVG_X_abs = VG_start_stats$lastVG_X_abs
          plottable_df$firstVG_X_rel = VG_start_stats$firstVG_X_rel
          plottable_df$lastVG_X_rel = VG_start_stats$lastVG_X_rel
          
          #========================================================================================
          # 11) Gap Power 
          #========================================================================================
          gap_power = getGapPower(VG_stats, solvertraj_eff)
          plottable_df$gap_power = gap_power$stat_ellbow_power
          plottable_df$gap_power_norm = gap_power$stat_ellbow_power_norm
          
          #========================================================================================
          # 12) Improvements statistics
          #========================================================================================
          # backwards <- T
          plottable_df$impr_last_1 = (getLastXimprovement(solvertraj_eff, 0.1)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_last_2 = (getLastXimprovement(solvertraj_eff, 0.2)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_last_3 = (getLastXimprovement(solvertraj_eff, 0.3)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_last_4 = (getLastXimprovement(solvertraj_eff, 0.4)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_last_5 = (getLastXimprovement(solvertraj_eff, 0.5)[3] %>% unlist(.)) %>% as.double(.)
          # backwards <- F
          plottable_df$impr_first_1 = (getLastXimprovement(solvertraj_eff, 0.1, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_first_2 = (getLastXimprovement(solvertraj_eff, 0.2, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_first_3 = (getLastXimprovement(solvertraj_eff, 0.3, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_first_4 = (getLastXimprovement(solvertraj_eff, 0.4, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_first_5 = (getLastXimprovement(solvertraj_eff, 0.5, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          # from to
          plottable_df$impr_0_to_01 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.0, to = 0.1)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_01_to_02 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.1, to = 0.2)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_02_to_03 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.2, to = 0.3)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_03_to_04 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.3, to = 0.4)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_04_to_05 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.4, to = 0.5)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_05_to_06 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.5, to = 0.6)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_06_to_07 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.6, to = 0.7)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_07_to_08 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.7, to = 0.8)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_08_to_09 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.8, to = 0.9)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_09_to_1 = (getLastXimprovement(solvertraj_eff, from_to = T, from = 0.9, to = 1.0)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df$impr_025_to_075 =(getLastXimprovement(solvertraj_eff, from_to = T, from = 0.25, to = 0.75)[3] %>% unlist(.)) %>% as.double(.)
          
          #========================================================================================
          # 13.1) Slopes Stats 
          #========================================================================================
          slope_stats = getSlopeStats(solvertraj_eff)
          
          plottable_df$Num_slopes = slope_stats$resls_EXCL_ZERO_impro$Num_slopes
          plottable_df$Min_slopes = slope_stats$resls_EXCL_ZERO_impro$Min_slopes
          plottable_df$Max_slopes = slope_stats$resls_EXCL_ZERO_impro$Max_slopes
          plottable_df$Mean_slopes = slope_stats$resls_EXCL_ZERO_impro$Mean_slopes
          plottable_df$Mode_slopes = slope_stats$resls_EXCL_ZERO_impro$Mode_slopes
          plottable_df$Median_slopes = slope_stats$resls_EXCL_ZERO_impro$Median_slopes
          plottable_df$Quantiles_slopes0 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[1]
          plottable_df$Quantiles_slopes25 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[2]
          plottable_df$Quantiles_slopes50 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[3]
          plottable_df$Quantiles_slopes75 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[4]
          plottable_df$Quantiles_slopes100 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[5]
          plottable_df$SD_slopes = slope_stats$resls_EXCL_ZERO_impro$SD_slopes
          plottable_df$Var_slopes = slope_stats$resls_EXCL_ZERO_impro$Var_slopes
          plottable_df$Skew_slopes = slope_stats$resls_EXCL_ZERO_impro$Skew_slopes
          plottable_df$Span_slopes = slope_stats$resls_EXCL_ZERO_impro$Span_slopes
          plottable_df$Varcoeff_slopes = slope_stats$resls_EXCL_ZERO_impro$Varcoeff_slopes
          
          #========================================================================================
          # 13.2) Slopes Direction Change Stats  
          #========================================================================================
          slope_dir_stat = getSlopeDirectionStats(solvertraj_eff)
          plottable_df$slope_dir_changes = slope_dir_stat$Num_direction_change
          
          plottable_df$Min_slopes_i_n = slope_dir_stat$slope_2_stats$Min_slopes_i_n
          plottable_df$Max_slopes_i_n = slope_dir_stat$slope_2_stats$Max_slopes_i_n
          plottable_df$Mean_slopes_i_n = slope_dir_stat$slope_2_stats$Mean_slopes_i_n
          plottable_df$Mode_slopes_i_n = slope_dir_stat$slope_2_stats$Mode_slopes_i_n
          plottable_df$Median_slopes_i_n = slope_dir_stat$slope_2_stats$Median_slopes_i_n
          plottable_df$Quantiles_slopes_i_n0 = slope_dir_stat$slope_2_stats$Quantiles_slopes_i_n[1]
          plottable_df$Quantiles_slopes_i_n25 = slope_dir_stat$slope_2_stats$Quantiles_slopes_i_n[2]
          plottable_df$Quantiles_slopes_i_n50 = slope_dir_stat$slope_2_stats$Quantiles_slopes_i_n[3]
          plottable_df$Quantiles_slopes_i_n75 = slope_dir_stat$slope_2_stats$Quantiles_slopes_i_n[4]
          plottable_df$Quantiles_slopes_i_n100 = slope_dir_stat$slope_2_stats$Quantiles_slopes_i_n[5]
          plottable_df$SD_slopes_i_n = slope_dir_stat$slope_2_stats$SD_slopes_i_n
          plottable_df$Span_slopes_i_n = slope_dir_stat$slope_2_stats$Span_slopes_i_n
          plottable_df$Skew_slopes_i_n = slope_dir_stat$slope_2_stats$Skew_slopes_i_n
          plottable_df$Span_slopes_i_n = slope_dir_stat$slope_2_stats$Span_slopes_i_n
          plottable_df$Varcoeff_slopes_i_n = slope_dir_stat$slope_2_stats$Varcoeff_slopes_i_n
          
          #========================================================================================
          # 14) violation stats EFF 
          #========================================================================================
          if(length(Plat_start_stats$data) != 0){
            violation_stats = getVioPlat(solvertraj_eff, VG_stats, Plat_start_stats, Plat_stats)
            plottable_df$VG_boarder_X = violation_stats$VG_boarder_X
            plottable_df$vio_ratio_PLAT = violation_stats$vio_ratio_PLAT
            plottable_df$vio_ratio_VG = violation_stats$vio_ratio_VG
            plottable_df$vio_ratio_PLAT_VG = violation_stats$vio_ratio_PLAT_VG
          } else {
            plottable_df$VG_boarder_X = 0
            plottable_df$vio_ratio_PLAT = 0
            plottable_df$vio_ratio_VG = 0
            plottable_df$vio_ratio_PLAT_VG = 0
          }
          
          #========================================================================================
          # 15) CHULL stats
          #========================================================================================
          chull_stats = getCHULLratio(solvertraj_eff)
          plottable_df$chull_spanning_points = chull_stats$chull_spanning_points
          plottable_df$points_on_chull = chull_stats$points_on_chull
          plottable_df$ratio_chull_pointsOnchull = chull_stats$ratio_chull_pointsOnchull
          plottable_df$ratio_points_on_chull_allPoints = chull_stats$ratio_points_on_chull_allPoints
          plottable_df$ratio_chull_allPoints = chull_stats$ratio_chull_allPoints
          
          #========================================================================================
          # 16) quadratic model fit 
          #========================================================================================
          qmcoeff = getQuadraticModelCoeff(solvertraj_eff, TRUE)
          
          plottable_df$rsquared= qmcoeff$summ_coeff_poly$r.squared
          plottable_df$rsquared_adj = qmcoeff$summ_coeff_poly$adj.r.squared
          plottable_df$fstatistic_value = qmcoeff$summ_coeff_poly$fstatistic[1] %>% as.double(.)
          plottable_df$fstatistic_numdf = qmcoeff$summ_coeff_poly$fstatistic[2] %>% as.double(.)
          plottable_df$fstatistic_dendf = qmcoeff$summ_coeff_poly$fstatistic[3] %>% as.double(.)
          plottable_df$sigma = qmcoeff$summ_coeff_poly$sigma
          plottable_df$residuals_mean = qmcoeff$summ_coeff_poly$residuals %>% mean(.)
          
          #========================================================================================
          # 17) Knee Stats 
          #========================================================================================
          if(exists("Plat_start_stats") & !is.na(Plat_start_stats$plat_start$Plats_area_begin_X)){
            knee_stats = getKneeRatio(solvertraj_eff, VG_stats, Plat_start_stats)
            plottable_df$KneeAmnt = knee_stats$knee_counter
            plottable_df$KneeRatio = knee_stats$knee_ratio
          } else {
            plottable_df$KneeAmnt = 0L
            plottable_df$KneeRatio = 0L
          }
          print(paste("could derive features in EFF: ", l))
          
          #############################################################################################################################
          #############################################################################################################################
          # eff real switch
          #############################################################################################################################
          #############################################################################################################################
          
          #========================================================================================
          # 0) default Stats REAL
          #========================================================================================
          # Runtime and iter stats
          plottable_df_copy$runtime = default_traj_stats$real_runtime
          plottable_df_copy$runtime_cleaned = default_traj_stats$real_runtime_cleaned # +++ new +++
          plottable_df_copy$iterations = default_traj_stats$real_iterations
          plottable_df_copy$iterations_cleaned = default_traj_stats$real_iterations_cleaned # +++ new +++
          plottable_df_copy$plateau_found =  1L # as it is always the case to have (at least one) plateau for the copy/real version
          plottable_df_copy$time_per_iter_AVG = default_traj_stats$time_per_iter_AVG_real
          plottable_df_copy$time_per_iter_AVG_cleaned = default_traj_stats$time_per_iter_AVG_real_cleaned # +++ new +++
          plottable_df_copy$biggest_drop_span_ratio = default_traj_stats$TODO_biggest_drop_span_ratio  #same for real
          
          # Time Diff stats
          plottable_df_copy$Num_time_diff = default_traj_stats$time_diff_stat_ls_real$Num_time_diff_real
          plottable_df_copy$Min_time_diff = default_traj_stats$time_diff_stat_ls_real$Min_time_diff_real
          plottable_df_copy$Max_time_diff = default_traj_stats$time_diff_stat_ls_real$Max_time_diff_real
          plottable_df_copy$Mean_time_diff = default_traj_stats$time_diff_stat_ls_real$Mean_time_diff_real
          plottable_df_copy$Mode_time_diff = default_traj_stats$time_diff_stat_ls_real$Mode_time_diff_real
          plottable_df_copy$Median_time_diff = default_traj_stats$time_diff_stat_ls_real$Median_time_diff_real
          plottable_df_copy$Quantiles_time_diff0 = default_traj_stats$time_diff_stat_ls_real$Quantiles_time_diff_real[1] %>% as.numeric(.)
          plottable_df_copy$Quantiles_time_diff25 = default_traj_stats$time_diff_stat_ls_real$Quantiles_time_diff_real[2] %>% as.numeric(.)
          plottable_df_copy$Quantiles_time_diff50 = default_traj_stats$time_diff_stat_ls_real$Quantiles_time_diff_real[3] %>% as.numeric(.)
          plottable_df_copy$Quantiles_time_diff75 = default_traj_stats$time_diff_stat_ls_real$Quantiles_time_diff_real[4] %>% as.numeric(.)
          plottable_df_copy$Quantiles_time_diff100 = default_traj_stats$time_diff_stat_ls_real$Quantiles_time_diff_real[5] %>% as.numeric(.)
          plottable_df_copy$SD_time_diff = default_traj_stats$time_diff_stat_ls_real$SD_time_diff_real
          plottable_df_copy$Var_time_diff = default_traj_stats$time_diff_stat_ls_real$Var_time_diff_real
          plottable_df_copy$Skew_time_diff = default_traj_stats$time_diff_stat_ls_real$Skew_time_diff_real
          plottable_df_copy$Span_time_diff = default_traj_stats$time_diff_stat_ls_real$Span_time_diff_real
          plottable_df_copy$Varcoeff_time_diff = default_traj_stats$time_diff_stat_ls_real$Varcoeff_time_diff_real
          
          #Incumbent Fitness (+ diff() stats)
          plottable_df_copy$Num_incumbent = default_traj_stats$incumbent_stat_ls_real$Num_incumbent_copy
          plottable_df_copy$Min_incumbent = default_traj_stats$incumbent_stat_ls_real$Min_incumbent_copy
          plottable_df_copy$Max_incumbent = default_traj_stats$incumbent_stat_ls_real$Max_incumbent_copy
          plottable_df_copy$Mean_incumbent = default_traj_stats$incumbent_stat_ls_real$Mean_incumbent_copy
          plottable_df_copy$Mode_incumbent = default_traj_stats$incumbent_stat_ls_real$Mode_incumbent_copy
          plottable_df_copy$Median_incumbent = default_traj_stats$incumbent_stat_ls_real$Median_incumbent_copy
          plottable_df_copy$Quantiles_incumbent0 = default_traj_stats$incumbent_stat_ls_real$Quantiles_incumbent_copy[1] %>% as.numeric(.)  # maybe normalize these later over all instances?
          plottable_df_copy$Quantiles_incumbent25 = default_traj_stats$incumbent_stat_ls_real$Quantiles_incumbent_copy[2] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent50= default_traj_stats$incumbent_stat_ls_real$Quantiles_incumbent_copy[3] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent75 = default_traj_stats$incumbent_stat_ls_real$Quantiles_incumbent_copy[4] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent100 = default_traj_stats$incumbent_stat_ls_real$Quantiles_incumbent_copy[5] %>% as.numeric(.)
          plottable_df_copy$SD_incumbent = default_traj_stats$incumbent_stat_ls_real$SD_incumbent_copy
          plottable_df_copy$Var_incumbent = default_traj_stats$incumbent_stat_ls_real$Var_incumbent_copy
          plottable_df_copy$Skew_incumbent = default_traj_stats$incumbent_stat_ls_real$Skew_incumbent_copy
          plottable_df_copy$Span_incumbent = default_traj_stats$incumbent_stat_ls_real$Span_incumbent_copy
          plottable_df_copy$Varcoeff_incumbent = default_traj_stats$incumbent_stat_ls_real$Varcoeff_incumbent_copy
          
          plottable_df_copy$Num_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Num_incumbent_diff
          plottable_df_copy$Min_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Min_incumbent_diff
          plottable_df_copy$Max_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Max_incumbent_diff
          plottable_df_copy$Mean_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Mean_incumbent_diff
          plottable_df_copy$Mode_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Mode_incumbent_diff
          plottable_df_copy$Median_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Median_incumbent_diff
          plottable_df_copy$Quantiles_incumbent_diff0 = default_traj_stats$incumbent_diff_stat_ls_real$Quantiles_incumbent_diff[1] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent_diff25 = default_traj_stats$incumbent_diff_stat_ls_real$Quantiles_incumbent_diff[2] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent_diff50 = default_traj_stats$incumbent_diff_stat_ls_real$Quantiles_incumbent_diff[3] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent_diff75 = default_traj_stats$incumbent_diff_stat_ls_real$Quantiles_incumbent_diff[4] %>% as.numeric(.)
          plottable_df_copy$Quantiles_incumbent_diff100 = default_traj_stats$incumbent_diff_stat_ls_real$Quantiles_incumbent_diff[5] %>% as.numeric(.)
          plottable_df_copy$SD_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$SD_incumbent_diff
          plottable_df_copy$Var_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Var_incumbent_diff
          plottable_df_copy$Skew_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Skew_incumbent_diff
          plottable_df_copy$Span_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Span_incumbent_diff
          plottable_df_copy$Varcoeff_incumbent_diff = default_traj_stats$incumbent_diff_stat_ls_real$Varcoeff_incumbent_diff
          
          #AVG Fitness (+ diff() stats)
          plottable_df_copy$Num_avgFit = default_traj_stats$avgFit_stat_ls_real$Num_avgFit_copy
          plottable_df_copy$Min_avgFit = default_traj_stats$avgFit_stat_ls_real$Min_avgFit_copy
          plottable_df_copy$Max_avgFit = default_traj_stats$avgFit_stat_ls_real$Max_avgFit_copy
          plottable_df_copy$Mean_avgFit = default_traj_stats$avgFit_stat_ls_real$Mean_avgFit_copy
          plottable_df_copy$Mode_avgFit = default_traj_stats$avgFit_stat_ls_real$Mode_avgFit_copy
          plottable_df_copy$Median_avgFit = default_traj_stats$avgFit_stat_ls_real$Median_avgFit_copy
          plottable_df_copy$Quantiles_avgFit0 = default_traj_stats$avgFit_stat_ls_real$Quantiles_avgFit_copy[1] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit25 = default_traj_stats$avgFit_stat_ls_real$Quantiles_avgFit_copy[2] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit50 = default_traj_stats$avgFit_stat_ls_real$Quantiles_avgFit_copy[3] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit75 = default_traj_stats$avgFit_stat_ls_real$Quantiles_avgFit_copy[4] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit100 = default_traj_stats$avgFit_stat_ls_real$Quantiles_avgFit_copy[5] %>% as.numeric(.)
          plottable_df_copy$SD_avgFit = default_traj_stats$avgFit_stat_ls_real$SD_avgFit_copy
          plottable_df_copy$Var_avgFit = default_traj_stats$avgFit_stat_ls_real$Var_avgFit_copy
          plottable_df_copy$Skew_avgFit = default_traj_stats$avgFit_stat_ls_real$Skew_avgFit_copy
          plottable_df_copy$Span_avgFit = default_traj_stats$avgFit_stat_ls_real$Span_avgFit_copy
          plottable_df_copy$Varcoeff_avgFit = default_traj_stats$avgFit_stat_ls_real$Varcoeff_avgFit_copy
          
          plottable_df_copy$Num_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Num_avgFit_diff
          plottable_df_copy$Min_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Min_avgFit_diff
          plottable_df_copy$Max_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Max_avgFit_diff
          plottable_df_copy$Mean_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Mean_avgFit_diff
          plottable_df_copy$Mode_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Mode_avgFit_diff
          plottable_df_copy$Median_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Median_avgFit_diff
          plottable_df_copy$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Quantiles_avgFit_diff[1] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Quantiles_avgFit_diff[2] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Quantiles_avgFit_diff[3] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Quantiles_avgFit_diff[4] %>% as.numeric(.)
          plottable_df_copy$Quantiles_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Quantiles_avgFit_diff[5] %>% as.numeric(.)
          plottable_df_copy$SD_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$SD_avgFit_diff
          plottable_df_copy$Var_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Var_avgFit_diff
          plottable_df_copy$Skew_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Skew_avgFit_diff
          plottable_df_copy$Span_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Span_avgFit_diff
          plottable_df_copy$Varcoeff_avgFit_diff = default_traj_stats$avgFit_diff_stat_ls_real$Varcoeff_avgFit_diff
          
          # same for real
          plottable_df_copy$real_eff_same_length_FLAG = default_traj_stats$real_effective_same_length
          plottable_df_copy$eff_real_iter_ratio = default_traj_stats$eff_real_iter_relation # default_traj_stats$eff_real_iter_ratio
          plottable_df_copy$eff_real_time_ratio = default_traj_stats$eff_real_time_relation # default_traj_stats$eff_real_time_ratio
          # +++ new +++
          plottable_df_copy$effective_partion_iter = default_traj_stats$effective_partion_iter
          plottable_df_copy$effective_partion_time = default_traj_stats$effective_partion_time
          
          plottable_df_copy$incumbent_eff_real_ratio = default_traj_stats$incumbent_eff_real_ratio
          plottable_df_copy$avgFit_eff_real_ratio = default_traj_stats$avgFit_eff_real_ratio
          
          #========================================================================================
          # 1) traj length stat REAL
          #========================================================================================
          # real
          traj_len_inc_copy = solvertraj_len(solvertraj_real, which_one = "incumbant")
          traj_len_avgfit_copy = solvertraj_len(solvertraj_real, which_one = "average.fitness")
          
          # INC
          plottable_df_copy$len_solvertraj_INC = traj_len_inc_copy$len_solvertraj
          plottable_df_copy$actual_len_INC = traj_len_inc_copy$actual_len
          plottable_df_copy$Num_len_traj_INC = traj_len_inc_copy$len_stats$Num_len_traj
          plottable_df_copy$Min_len_traj_INC = traj_len_inc_copy$len_stats$Min_len_traj
          plottable_df_copy$Max_len_traj_INC = traj_len_inc_copy$len_stats$Max_len_traj
          plottable_df_copy$Mean_len_traj_INC = traj_len_inc_copy$len_stats$Mean_len_traj
          plottable_df_copy$Mode_len_traj_INC = traj_len_inc_copy$len_stats$Mode_len_traj
          plottable_df_copy$Median_len_traj_INC = traj_len_inc_copy$len_stats$Median_len_traj
          plottable_df_copy$Quantiles_len_traj0_INC = traj_len_inc_copy$len_stats$Quantiles_len_traj[1]
          plottable_df_copy$Quantiles_len_traj25_INC = traj_len_inc_copy$len_stats$Quantiles_len_traj[2]
          plottable_df_copy$Quantiles_len_traj50_INC = traj_len_inc_copy$len_stats$Quantiles_len_traj[3]
          plottable_df_copy$Quantiles_len_traj70_INC = traj_len_inc_copy$len_stats$Quantiles_len_traj[4]
          plottable_df_copy$Quantiles_len_traj100_INC = traj_len_inc_copy$len_stats$Quantiles_len_traj[5]
          plottable_df_copy$SD_len_traj_INC = traj_len_inc_copy$len_stats$SD_len_traj
          plottable_df_copy$Var_len_traj_INC = traj_len_inc_copy$len_stats$Var_len_traj
          plottable_df_copy$Skew_len_traj_INC = traj_len_inc_copy$len_stats$Skew_len_traj
          plottable_df_copy$Span_len_traj_INC = traj_len_inc_copy$len_stats$Span_len_traj
          plottable_df_copy$Varcoeff_len_traj_INC = traj_len_inc_copy$len_stats$Varcoeff_len_traj
          
          # AVG FIT
          plottable_df_copy$len_solvertraj_AVGFIT = traj_len_avgfit_copy$len_solvertraj
          plottable_df_copy$actual_len_AVGFIT = traj_len_avgfit_copy$actual_len
          plottable_df_copy$Num_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Num_len_traj
          plottable_df_copy$Min_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Min_len_traj
          plottable_df_copy$Max_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Max_len_traj
          plottable_df_copy$Mean_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Mean_len_traj
          plottable_df_copy$Mode_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Mode_len_traj
          plottable_df_copy$Median_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Median_len_traj
          plottable_df_copy$Quantiles_len_traj0_AVGFIT = traj_len_avgfit_copy$len_stats$Quantiles_len_traj[1]
          plottable_df_copy$Quantiles_len_traj25_AVGFIT = traj_len_avgfit_copy$len_stats$Quantiles_len_traj[2]
          plottable_df_copy$Quantiles_len_traj50_AVGFIT = traj_len_avgfit_copy$len_stats$Quantiles_len_traj[3]
          plottable_df_copy$Quantiles_len_traj70_AVGFIT = traj_len_avgfit_copy$len_stats$Quantiles_len_traj[4]
          plottable_df_copy$Quantiles_len_traj100_AVGFIT = traj_len_avgfit_copy$len_stats$Quantiles_len_traj[5]
          plottable_df_copy$SD_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$SD_len_traj
          plottable_df_copy$Var_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Var_len_traj
          plottable_df_copy$Skew_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Skew_len_traj
          plottable_df_copy$Span_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Span_len_traj
          plottable_df_copy$Varcoeff_len_traj_AVGFIT = traj_len_avgfit_copy$len_stats$Varcoeff_len_traj
          
          # ratios
          # adjust to copy version
          plottable_df_copy$INC_AVGFIT_len_ratio = traj_len_inc_copy$actual_len / traj_len_avgfit_copy$actual_len
          
          #========================================================================================
          # 2.1) Lina Default
          #========================================================================================
          lina_def_copy.INC = lina_default(solvertraj_real, "incumbant")
          lina_def_copy.AVGFIT = lina_default(solvertraj_real, "average.fitness")
          
          # INC
          # grets INC
          plottable_df_copy$gret10_INC = lina_def_copy.INC$tmp_resls$gret[1]
          plottable_df_copy$gret20_INC = lina_def_copy.INC$tmp_resls$gret[2]
          plottable_df_copy$gret30_INC = lina_def_copy.INC$tmp_resls$gret[3]
          plottable_df_copy$gret40_INC = lina_def_copy.INC$tmp_resls$gret[4]
          plottable_df_copy$gret50_INC = lina_def_copy.INC$tmp_resls$gret[5]
          plottable_df_copy$gret60_INC = lina_def_copy.INC$tmp_resls$gret[6]
          plottable_df_copy$gret70_INC = lina_def_copy.INC$tmp_resls$gret[7]
          plottable_df_copy$gret80_INC = lina_def_copy.INC$tmp_resls$gret[8]
          plottable_df_copy$gret90_INC = lina_def_copy.INC$tmp_resls$gret[9]
          
          # lt INC
          plottable_df_copy$lt10_INC = lina_def_copy.INC$tmp_resls$lt[1]
          plottable_df_copy$lt20_INC = lina_def_copy.INC$tmp_resls$lt[2]
          plottable_df_copy$lt30_INC = lina_def_copy.INC$tmp_resls$lt[3]
          plottable_df_copy$lt40_INC = lina_def_copy.INC$tmp_resls$lt[4]
          plottable_df_copy$lt50_INC = lina_def_copy.INC$tmp_resls$lt[5]
          plottable_df_copy$lt60_INC = lina_def_copy.INC$tmp_resls$lt[6]
          plottable_df_copy$lt70_INC = lina_def_copy.INC$tmp_resls$lt[7]
          plottable_df_copy$lt80_INC = lina_def_copy.INC$tmp_resls$lt[8]
          plottable_df_copy$lt90_INC = lina_def_copy.INC$tmp_resls$lt[9]
          
          # gret lt ratio INC
          plottable_df_copy$lt_gret_ratio_10_INC = lina_def_copy.INC$tmp_resls$lt[1] / lina_def_copy.INC$tmp_resls$gret[1]
          plottable_df_copy$lt_gret_ratio_20_INC = lina_def_copy.INC$tmp_resls$lt[2] / lina_def_copy.INC$tmp_resls$gret[2]
          plottable_df_copy$lt_gret_ratio_30_INC = lina_def_copy.INC$tmp_resls$lt[3] / lina_def_copy.INC$tmp_resls$gret[3]
          plottable_df_copy$lt_gret_ratio_40_INC = lina_def_copy.INC$tmp_resls$lt[4] / lina_def_copy.INC$tmp_resls$gret[4]
          plottable_df_copy$lt_gret_ratio_50_INC = lina_def_copy.INC$tmp_resls$lt[5] / lina_def_copy.INC$tmp_resls$gret[5]
          plottable_df_copy$lt_gret_ratio_60_INC = lina_def_copy.INC$tmp_resls$lt[6] / lina_def_copy.INC$tmp_resls$gret[6]
          plottable_df_copy$lt_gret_ratio_70_INC = lina_def_copy.INC$tmp_resls$lt[7] / lina_def_copy.INC$tmp_resls$gret[7]
          plottable_df_copy$lt_gret_ratio_80_INC = lina_def_copy.INC$tmp_resls$lt[8] / lina_def_copy.INC$tmp_resls$gret[8]
          plottable_df_copy$lt_gret_ratio_90_INC = lina_def_copy.INC$tmp_resls$lt[9] / lina_def_copy.INC$tmp_resls$gret[9]
          
          # slopes INC
          plottable_df_copy$slope10_INC = lina_def_copy.INC$tmp_resls$slope[1]
          plottable_df_copy$slope20_INC = lina_def_copy.INC$tmp_resls$slope[2]
          plottable_df_copy$slope30_INC = lina_def_copy.INC$tmp_resls$slope[3]
          plottable_df_copy$slope40_INC = lina_def_copy.INC$tmp_resls$slope[4]
          plottable_df_copy$slope50_INC = lina_def_copy.INC$tmp_resls$slope[5]
          plottable_df_copy$slope60_INC = lina_def_copy.INC$tmp_resls$slope[6]
          plottable_df_copy$slope70_INC = lina_def_copy.INC$tmp_resls$slope[7]
          plottable_df_copy$slope80_INC = lina_def_copy.INC$tmp_resls$slope[8]
          plottable_df_copy$slope90_INC = lina_def_copy.INC$tmp_resls$slope[9]
          
          # gret summ stats INC
          plottable_df_copy$Num_gret_INC = lina_def_copy.INC$gret_stats$Num_gret
          plottable_df_copy$Min_gret_INC = lina_def_copy.INC$gret_stats$Min_gret
          plottable_df_copy$Max_gret_INC = lina_def_copy.INC$gret_stats$Max_gret
          plottable_df_copy$Mean_gret_INC = lina_def_copy.INC$gret_stats$Mean_gret
          plottable_df_copy$Mode_gret_INC = lina_def_copy.INC$gret_stats$Mode_gret
          plottable_df_copy$Median_gret_INC = lina_def_copy.INC$gret_stats$Median_gret
          plottable_df_copy$Quantiles_gret0_INC = lina_def_copy.INC$gret_stats$Quantiles_gret[1]
          plottable_df_copy$Quantiles_gret25_INC = lina_def_copy.INC$gret_stats$Quantiles_gret[2]
          plottable_df_copy$Quantiles_gret50_INC = lina_def_copy.INC$gret_stats$Quantiles_gret[3]
          plottable_df_copy$Quantiles_gret75_INC = lina_def_copy.INC$gret_stats$Quantiles_gret[4]
          plottable_df_copy$Quantiles_gret100_INC = lina_def_copy.INC$gret_stats$Quantiles_gret[5]
          plottable_df_copy$SD_gret_INC = lina_def_copy.INC$gret_stats$SD_gret
          plottable_df_copy$Var_gret_INC = lina_def_copy.INC$gret_stats$Var_gret
          plottable_df_copy$Skew_gret_INC = lina_def_copy.INC$gret_stats$Skew_gret
          plottable_df_copy$Span_gret_INC = lina_def_copy.INC$gret_stats$Span_gret
          plottable_df_copy$Varcoeff_gret_INC = lina_def_copy.INC$gret_stats$Varcoeff_gret
          
          # lt summ stats INC
          plottable_df_copy$Num_lt_INC = lina_def_copy.INC$lt_stats$Num_lt
          plottable_df_copy$Min_lt_INC = lina_def_copy.INC$lt_stats$Min_lt
          plottable_df_copy$Max_lt_INC = lina_def_copy.INC$lt_stats$Max_lt
          plottable_df_copy$Mean_lt_INC = lina_def_copy.INC$lt_stats$Mean_lt
          plottable_df_copy$Mode_lt_INC = lina_def_copy.INC$lt_stats$Mode_lt
          plottable_df_copy$Median_lt_INC = lina_def_copy.INC$lt_stats$Median_lt
          plottable_df_copy$Quantiles_lt0_INC = lina_def_copy.INC$lt_stats$Quantiles_lt[1]
          plottable_df_copy$Quantiles_lt25_INC = lina_def_copy.INC$lt_stats$Quantiles_lt[2]
          plottable_df_copy$Quantiles_lt50_INC = lina_def_copy.INC$lt_stats$Quantiles_lt[3]
          plottable_df_copy$Quantiles_lt75_INC = lina_def_copy.INC$lt_stats$Quantiles_lt[4]
          plottable_df_copy$Quantiles_lt100_INC = lina_def_copy.INC$lt_stats$Quantiles_lt[5]
          plottable_df_copy$SD_lt_INC = lina_def_copy.INC$lt_stats$SD_lt
          plottable_df_copy$Var_lt_INC = lina_def_copy.INC$lt_stats$Var_lt
          plottable_df_copy$Skew_lt_INC = lina_def_copy.INC$lt_stats$Skew_lt
          plottable_df_copy$Span_lt_INC = lina_def_copy.INC$lt_stats$Span_lt
          plottable_df_copy$Varcoeff_lt_INC = lina_def_copy.INC$lt_stats$Varcoeff_lt
          
          # slope summ stats INC
          plottable_df_copy$Num_slope_INC = lina_def_copy.INC$slope_stats$Num_slope
          plottable_df_copy$Min_slope_INC = lina_def_copy.INC$slope_stats$Min_slope
          plottable_df_copy$Max_slope_INC = lina_def_copy.INC$slope_stats$Max_slope
          plottable_df_copy$Mean_slope_INC = lina_def_copy.INC$slope_stats$Mean_slope
          plottable_df_copy$Mode_slope_INC = lina_def_copy.INC$slope_stats$Mode_slope
          plottable_df_copy$Median_slope_INC = lina_def_copy.INC$slope_stats$Median_slope
          plottable_df_copy$Quantiles_slope0_INC = lina_def_copy.INC$slope_stats$Quantiles_slope[1]
          plottable_df_copy$Quantiles_slope25_INC = lina_def_copy.INC$slope_stats$Quantiles_slope[2]
          plottable_df_copy$Quantiles_slope50_INC = lina_def_copy.INC$slope_stats$Quantiles_slope[3]
          plottable_df_copy$Quantiles_slope75_INC = lina_def_copy.INC$slope_stats$Quantiles_slope[4]
          plottable_df_copy$Quantiles_slope100_INC = lina_def_copy.INC$slope_stats$Quantiles_slope[5]
          plottable_df_copy$SD_slope_INC = lina_def_copy.INC$slope_stats$SD_slope
          plottable_df_copy$Var_slope_INC = lina_def_copy.INC$slope_stats$Var_slope
          plottable_df_copy$Skew_slope_INC = lina_def_copy.INC$slope_stats$Skew_slope
          plottable_df_copy$Span_slope_INC = lina_def_copy.INC$slope_stats$Span_slope
          plottable_df_copy$Varcoeff_slope_INC = lina_def_copy.INC$slope_stats$Varcoeff_slope
          
          # AVGFIT
          # grets AVGFIT
          plottable_df_copy$gret10_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[1]
          plottable_df_copy$gret20_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[2]
          plottable_df_copy$gret30_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[3]
          plottable_df_copy$gret40_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[4]
          plottable_df_copy$gret50_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[5]
          plottable_df_copy$gret60_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[6]
          plottable_df_copy$gret70_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[7]
          plottable_df_copy$gret80_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[8]
          plottable_df_copy$gret90_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$gret[9]
          
          # lt AVGFIT
          plottable_df_copy$lt10_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[1]
          plottable_df_copy$lt20_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[2]
          plottable_df_copy$lt30_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[3]
          plottable_df_copy$lt40_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[4]
          plottable_df_copy$lt50_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[5]
          plottable_df_copy$lt60_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[6]
          plottable_df_copy$lt70_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[7]
          plottable_df_copy$lt80_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[8]
          plottable_df_copy$lt90_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[9]
          
          # gret lt ratio AVGFIT
          plottable_df_copy$lt_gret_ratio_10_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[1] / lina_def_copy.AVGFIT$tmp_resls$gret[1]
          plottable_df_copy$lt_gret_ratio_20_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[2] / lina_def_copy.AVGFIT$tmp_resls$gret[2]
          plottable_df_copy$lt_gret_ratio_30_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[3] / lina_def_copy.AVGFIT$tmp_resls$gret[3]
          plottable_df_copy$lt_gret_ratio_40_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[4] / lina_def_copy.AVGFIT$tmp_resls$gret[4]
          plottable_df_copy$lt_gret_ratio_50_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[5] / lina_def_copy.AVGFIT$tmp_resls$gret[5]
          plottable_df_copy$lt_gret_ratio_60_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[6] / lina_def_copy.AVGFIT$tmp_resls$gret[6]
          plottable_df_copy$lt_gret_ratio_70_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[7] / lina_def_copy.AVGFIT$tmp_resls$gret[7]
          plottable_df_copy$lt_gret_ratio_80_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[8] / lina_def_copy.AVGFIT$tmp_resls$gret[8]
          plottable_df_copy$lt_gret_ratio_90_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$lt[9] / lina_def_copy.AVGFIT$tmp_resls$gret[9]
          
          # slopes AVGFIT
          plottable_df_copy$slope10_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[1]
          plottable_df_copy$slope20_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[2]
          plottable_df_copy$slope30_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[3]
          plottable_df_copy$slope40_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[4]
          plottable_df_copy$slope50_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[5]
          plottable_df_copy$slope60_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[6]
          plottable_df_copy$slope70_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[7]
          plottable_df_copy$slope80_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[8]
          plottable_df_copy$slope90_AVGFIT = lina_def_copy.AVGFIT$tmp_resls$slope[9]
          
          # gret summ stats AVGFIT
          plottable_df_copy$Num_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Num_gret
          plottable_df_copy$Min_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Min_gret
          plottable_df_copy$Max_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Max_gret
          plottable_df_copy$Mean_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Mean_gret
          plottable_df_copy$Mode_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Mode_gret
          plottable_df_copy$Median_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Median_gret
          plottable_df_copy$Quantiles_gret0_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Quantiles_gret[1]
          plottable_df_copy$Quantiles_gret25_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Quantiles_gret[2]
          plottable_df_copy$Quantiles_gret50_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Quantiles_gret[3]
          plottable_df_copy$Quantiles_gret75_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Quantiles_gret[4]
          plottable_df_copy$Quantiles_gret100_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Quantiles_gret[5]
          plottable_df_copy$SD_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$SD_gret
          plottable_df_copy$Var_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Var_gret
          plottable_df_copy$Skew_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Skew_gret
          plottable_df_copy$Span_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Span_gret
          plottable_df_copy$Varcoeff_gret_AVGFIT = lina_def_copy.AVGFIT$gret_stats$Varcoeff_gret
          
          # lt summ stats AVGFIT
          plottable_df_copy$Num_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Num_lt
          plottable_df_copy$Min_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Min_lt
          plottable_df_copy$Max_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Max_lt
          plottable_df_copy$Mean_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Mean_lt
          plottable_df_copy$Mode_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Mode_lt
          plottable_df_copy$Median_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Median_lt
          plottable_df_copy$Quantiles_lt0_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Quantiles_lt[1]
          plottable_df_copy$Quantiles_lt25_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Quantiles_lt[2]
          plottable_df_copy$Quantiles_lt50_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Quantiles_lt[3]
          plottable_df_copy$Quantiles_lt75_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Quantiles_lt[4]
          plottable_df_copy$Quantiles_lt100_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Quantiles_lt[5]
          plottable_df_copy$SD_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$SD_lt
          plottable_df_copy$Var_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Var_lt
          plottable_df_copy$Skew_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Skew_lt
          plottable_df_copy$Span_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Span_lt
          plottable_df_copy$Varcoeff_lt_AVGFIT = lina_def_copy.AVGFIT$lt_stats$Varcoeff_lt
          
          # slope summ stats AVGFIT
          plottable_df_copy$Num_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Num_slope
          plottable_df_copy$Min_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Min_slope
          plottable_df_copy$Max_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Max_slope
          plottable_df_copy$Mean_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Mean_slope
          plottable_df_copy$Mode_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Mode_slope
          plottable_df_copy$Median_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Median_slope
          plottable_df_copy$Quantiles_slope0_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Quantiles_slope[1]
          plottable_df_copy$Quantiles_slope25_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Quantiles_slope[2]
          plottable_df_copy$Quantiles_slope50_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Quantiles_slope[3]
          plottable_df_copy$Quantiles_slope75_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Quantiles_slope[4]
          plottable_df_copy$Quantiles_slope100_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Quantiles_slope[5]
          plottable_df_copy$SD_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$SD_slope
          plottable_df_copy$Var_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Var_slope
          plottable_df_copy$Skew_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Skew_slope
          plottable_df_copy$Span_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Span_slope
          plottable_df_copy$Varcoeff_slope_AVGFIT = lina_def_copy.AVGFIT$slope_stats$Varcoeff_slope
          
          #========================================================================================
          # 2.2) Lina consecutive
          #========================================================================================
          lina_cons_copy.INC = lina_consecutive(solvertraj_eff, by_what = 6, which = "incumbant")
          lina_cons_copy.AVGFIT = lina_consecutive(solvertraj_eff, by_what = 6, which = "average.fitness")
          
          # INC
          # grets INC
          plottable_df_copy$gret1_INC_cons = lina_cons_copy.INC$tmp_resls$gret[1]
          plottable_df_copy$gret2_INC_cons = lina_cons_copy.INC$tmp_resls$gret[2]
          plottable_df_copy$gret3_INC_cons = lina_cons_copy.INC$tmp_resls$gret[3]
          plottable_df_copy$gret4_INC_cons = lina_cons_copy.INC$tmp_resls$gret[4]
          plottable_df_copy$gret5_INC_cons = lina_cons_copy.INC$tmp_resls$gret[5]
          plottable_df_copy$gret6_INC_cons = lina_cons_copy.INC$tmp_resls$gret[6]
          
          # lt INC
          plottable_df_copy$lt1_INC_cons = lina_cons_copy.INC$tmp_resls$lt[1]
          plottable_df_copy$lt2_INC_cons = lina_cons_copy.INC$tmp_resls$lt[2]
          plottable_df_copy$lt3_INC_cons = lina_cons_copy.INC$tmp_resls$lt[3]
          plottable_df_copy$lt4_INC_cons = lina_cons_copy.INC$tmp_resls$lt[4]
          plottable_df_copy$lt5_INC_cons = lina_cons_copy.INC$tmp_resls$lt[5]
          plottable_df_copy$lt6_INC_cons = lina_cons_copy.INC$tmp_resls$lt[6]
          
          # gret lt ratio INC
          plottable_df_copy$lt_gret_ratio_1_INC_cons = lina_cons_copy.INC$tmp_resls$lt[1] / lina_cons_copy.INC$tmp_resls$gret[1]
          plottable_df_copy$lt_gret_ratio_2_INC_cons = lina_cons_copy.INC$tmp_resls$lt[2] / lina_cons_copy.INC$tmp_resls$gret[2]
          plottable_df_copy$lt_gret_ratio_3_INC_cons = lina_cons_copy.INC$tmp_resls$lt[3] / lina_cons_copy.INC$tmp_resls$gret[3]
          plottable_df_copy$lt_gret_ratio_4_INC_cons = lina_cons_copy.INC$tmp_resls$lt[4] / lina_cons_copy.INC$tmp_resls$gret[4]
          plottable_df_copy$lt_gret_ratio_5_INC_cons = lina_cons_copy.INC$tmp_resls$lt[5] / lina_cons_copy.INC$tmp_resls$gret[5]
          plottable_df_copy$lt_gret_ratio_6_INC_cons = lina_cons_copy.INC$tmp_resls$lt[6] / lina_cons_copy.INC$tmp_resls$gret[6]
          
          # slopes INC
          plottable_df_copy$slope1_INC = lina_cons_copy.INC$tmp_resls$slope[1]
          plottable_df_copy$slope2_INC = lina_cons_copy.INC$tmp_resls$slope[2]
          plottable_df_copy$slope3_INC = lina_cons_copy.INC$tmp_resls$slope[3]
          plottable_df_copy$slope4_INC = lina_cons_copy.INC$tmp_resls$slope[4]
          plottable_df_copy$slope5_INC = lina_cons_copy.INC$tmp_resls$slope[5]
          plottable_df_copy$slope6_INC = lina_cons_copy.INC$tmp_resls$slope[6]
          
          # gret summ stats INC
          plottable_df_copy$Num_gret_INC_cons = lina_cons_copy.INC$gret_stats$Num_gret
          plottable_df_copy$Min_gret_INC_cons = lina_cons_copy.INC$gret_stats$Min_gret
          plottable_df_copy$Max_gret_INC_cons = lina_cons_copy.INC$gret_stats$Max_gret
          plottable_df_copy$Mean_gret_INC_cons = lina_cons_copy.INC$gret_stats$Mean_gret
          plottable_df_copy$Mode_gret_INC_cons = lina_cons_copy.INC$gret_stats$Mode_gret
          plottable_df_copy$Median_gret_INC_cons = lina_cons_copy.INC$gret_stats$Median_gret
          plottable_df_copy$Quantiles_gret0_INC_cons = lina_cons_copy.INC$gret_stats$Quantiles_gret[1]
          plottable_df_copy$Quantiles_gret25_INC_cons = lina_cons_copy.INC$gret_stats$Quantiles_gret[2]
          plottable_df_copy$Quantiles_gret50_INC_cons = lina_cons_copy.INC$gret_stats$Quantiles_gret[3]
          plottable_df_copy$Quantiles_gret75_INC_cons = lina_cons_copy.INC$gret_stats$Quantiles_gret[4]
          plottable_df_copy$Quantiles_gret100_INC_cons = lina_cons_copy.INC$gret_stats$Quantiles_gret[5]
          plottable_df_copy$SD_gret_INC_cons = lina_cons_copy.INC$gret_stats$SD_gret
          plottable_df_copy$Var_gret_INC_cons = lina_cons_copy.INC$gret_stats$Var_gret
          plottable_df_copy$Skew_gret_INC_cons = lina_cons_copy.INC$gret_stats$Skew_gret
          plottable_df_copy$Span_gret_INC_cons = lina_cons_copy.INC$gret_stats$Span_gret
          plottable_df_copy$Varcoeff_gret_INC_cons = lina_cons_copy.INC$gret_stats$Varcoeff_gret
          
          # lt summ stats INC
          plottable_df_copy$Num_lt_INC_cons = lina_cons_copy.INC$lt_stats$Num_lt
          plottable_df_copy$Min_lt_INC_cons = lina_cons_copy.INC$lt_stats$Min_lt
          plottable_df_copy$Max_lt_INC_cons = lina_cons_copy.INC$lt_stats$Max_lt
          plottable_df_copy$Mean_lt_INC_cons = lina_cons_copy.INC$lt_stats$Mean_lt
          plottable_df_copy$Mode_lt_INC_cons = lina_cons_copy.INC$lt_stats$Mode_lt
          plottable_df_copy$Median_lt_INC_cons = lina_cons_copy.INC$lt_stats$Median_lt
          plottable_df_copy$Quantiles_lt0_INC_cons = lina_cons_copy.INC$lt_stats$Quantiles_lt[1]
          plottable_df_copy$Quantiles_lt25_INC_cons = lina_cons_copy.INC$lt_stats$Quantiles_lt[2]
          plottable_df_copy$Quantiles_lt50_INC_cons = lina_cons_copy.INC$lt_stats$Quantiles_lt[3]
          plottable_df_copy$Quantiles_lt75_INC_cons = lina_cons_copy.INC$lt_stats$Quantiles_lt[4]
          plottable_df_copy$Quantiles_lt100_INC_cons = lina_cons_copy.INC$lt_stats$Quantiles_lt[5]
          plottable_df_copy$SD_lt_INC_cons = lina_cons_copy.INC$lt_stats$SD_lt
          plottable_df_copy$Var_lt_INC_cons = lina_cons_copy.INC$lt_stats$Var_lt
          plottable_df_copy$Skew_lt_INC_cons = lina_cons_copy.INC$lt_stats$Skew_lt
          plottable_df_copy$Span_lt_INC_cons = lina_cons_copy.INC$lt_stats$Span_lt
          plottable_df_copy$Varcoeff_lt_INC_cons = lina_cons_copy.INC$lt_stats$Varcoeff_lt
          
          # slope summ stats INC
          plottable_df_copy$Num_slope_INC_cons = lina_cons_copy.INC$slope_stats$Num_slope
          plottable_df_copy$Min_slope_INC_cons = lina_cons_copy.INC$slope_stats$Min_slope
          plottable_df_copy$Max_slope_INC_cons = lina_cons_copy.INC$slope_stats$Max_slope
          plottable_df_copy$Mean_slope_INC_cons = lina_cons_copy.INC$slope_stats$Mean_slope
          plottable_df_copy$Mode_slope_INC_cons = lina_cons_copy.INC$slope_stats$Mode_slope
          plottable_df_copy$Median_slope_INC_cons = lina_cons_copy.INC$slope_stats$Median_slope
          plottable_df_copy$Quantiles_slope0_INC_cons = lina_cons_copy.INC$slope_stats$Quantiles_slope[1]
          plottable_df_copy$Quantiles_slope25_INC_cons = lina_cons_copy.INC$slope_stats$Quantiles_slope[2]
          plottable_df_copy$Quantiles_slope50_INC_cons = lina_cons_copy.INC$slope_stats$Quantiles_slope[3]
          plottable_df_copy$Quantiles_slope75_INC_cons = lina_cons_copy.INC$slope_stats$Quantiles_slope[4]
          plottable_df_copy$Quantiles_slope100_INC_cons = lina_cons_copy.INC$slope_stats$Quantiles_slope[5]
          plottable_df_copy$SD_slope_INC_cons = lina_cons_copy.INC$slope_stats$SD_slope
          plottable_df_copy$Var_slope_INC_cons = lina_cons_copy.INC$slope_stats$Var_slope
          plottable_df_copy$Skew_slope_INC_cons = lina_cons_copy.INC$slope_stats$Skew_slope
          plottable_df_copy$Span_slope_INC_cons = lina_cons_copy.INC$slope_stats$Span_slope
          plottable_df_copy$Varcoeff_slope_INC_cons = lina_cons_copy.INC$slope_stats$Varcoeff_slope
          
          # AVGFIT
          # grets AVGFIT
          plottable_df_copy$gret1_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$gret[1]
          plottable_df_copy$gret2_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$gret[2]
          plottable_df_copy$gret3_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$gret[3]
          plottable_df_copy$gret4_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$gret[4]
          plottable_df_copy$gret5_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$gret[5]
          plottable_df_copy$gret6_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$gret[6]
          
          # lt AVGFIT
          plottable_df_copy$lt1_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[1]
          plottable_df_copy$lt2_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[2]
          plottable_df_copy$lt3_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[3]
          plottable_df_copy$lt4_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[4]
          plottable_df_copy$lt5_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[5]
          plottable_df_copy$lt6_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[6]
          
          # gret lt ratio AVGFIT
          plottable_df_copy$lt_gret_ratio_1_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[1] / lina_cons_copy.AVGFIT$tmp_resls$gret[1]
          plottable_df_copy$lt_gret_ratio_2_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[2] / lina_cons_copy.AVGFIT$tmp_resls$gret[2]
          plottable_df_copy$lt_gret_ratio_3_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[3] / lina_cons_copy.AVGFIT$tmp_resls$gret[3]
          plottable_df_copy$lt_gret_ratio_4_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[4] / lina_cons_copy.AVGFIT$tmp_resls$gret[4]
          plottable_df_copy$lt_gret_ratio_5_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[5] / lina_cons_copy.AVGFIT$tmp_resls$gret[5]
          plottable_df_copy$lt_gret_ratio_6_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$lt[6] / lina_cons_copy.AVGFIT$tmp_resls$gret[6]
          
          # slopes AVGFIT
          plottable_df_copy$slope1_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$slope[1]
          plottable_df_copy$slope2_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$slope[2]
          plottable_df_copy$slope3_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$slope[3]
          plottable_df_copy$slope4_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$slope[4]
          plottable_df_copy$slope5_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$slope[5]
          plottable_df_copy$slope6_AVGFIT_cons = lina_cons_copy.AVGFIT$tmp_resls$slope[6]
          
          # gret summ stats AVGFIT
          plottable_df_copy$Num_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Num_gret
          plottable_df_copy$Min_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Min_gret
          plottable_df_copy$Max_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Max_gret
          plottable_df_copy$Mean_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Mean_gret
          plottable_df_copy$Mode_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Mode_gret
          plottable_df_copy$Median_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Median_gret
          plottable_df_copy$Quantiles_gret0_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Quantiles_gret[1]
          plottable_df_copy$Quantiles_gret25_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Quantiles_gret[2]
          plottable_df_copy$Quantiles_gret50_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Quantiles_gret[3]
          plottable_df_copy$Quantiles_gret75_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Quantiles_gret[4]
          plottable_df_copy$Quantiles_gret100_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Quantiles_gret[5]
          plottable_df_copy$SD_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$SD_gret
          plottable_df_copy$Var_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Var_gret
          plottable_df_copy$Skew_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Skew_gret
          plottable_df_copy$Span_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Span_gret
          plottable_df_copy$Varcoeff_gret_AVGFIT_cons = lina_cons_copy.AVGFIT$gret_stats$Varcoeff_gret
          
          # lt summ stats AVGFIT
          plottable_df_copy$Num_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Num_lt
          plottable_df_copy$Min_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Min_lt
          plottable_df_copy$Max_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Max_lt
          plottable_df_copy$Mean_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Mean_lt
          plottable_df_copy$Mode_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Mode_lt
          plottable_df_copy$Median_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Median_lt
          plottable_df_copy$Quantiles_lt0_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Quantiles_lt[1]
          plottable_df_copy$Quantiles_lt25_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Quantiles_lt[2]
          plottable_df_copy$Quantiles_lt50_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Quantiles_lt[3]
          plottable_df_copy$Quantiles_lt75_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Quantiles_lt[4]
          plottable_df_copy$Quantiles_lt100_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Quantiles_lt[5]
          plottable_df_copy$SD_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$SD_lt
          plottable_df_copy$Var_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Var_lt
          plottable_df_copy$Skew_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Skew_lt
          plottable_df_copy$Span_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Span_lt
          plottable_df_copy$Varcoeff_lt_AVGFIT_cons = lina_cons_copy.AVGFIT$lt_stats$Varcoeff_lt
          
          # slope summ stats AVGFIT
          plottable_df_copy$Num_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Num_slope
          plottable_df_copy$Min_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Min_slope
          plottable_df_copy$Max_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Max_slope
          plottable_df_copy$Mean_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Mean_slope
          plottable_df_copy$Mode_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Mode_slope
          plottable_df_copy$Median_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Median_slope
          plottable_df_copy$Quantiles_slope0_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Quantiles_slope[1]
          plottable_df_copy$Quantiles_slope25_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Quantiles_slope[2]
          plottable_df_copy$Quantiles_slope50_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Quantiles_slope[3]
          plottable_df_copy$Quantiles_slope75_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Quantiles_slope[4]
          plottable_df_copy$Quantiles_slope100_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Quantiles_slope[5]
          plottable_df_copy$SD_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$SD_slope
          plottable_df_copy$Var_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Var_slope
          plottable_df_copy$Skew_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Skew_slope
          plottable_df_copy$Span_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Span_slope
          plottable_df_copy$Varcoeff_slope_AVGFIT_cons = lina_cons_copy.AVGFIT$slope_stats$Varcoeff_slope
          
          #========================================================================================
          # 3) time related features
          #========================================================================================
          time_after_iteration_X_stats_copy = time_X_stats(solvertraj_real)
          
          plottable_df_copy$time_passed_after_10perc = time_after_iteration_X_stats_copy$time_passed_iter_X[1]
          plottable_df_copy$time_passed_after_20perc = time_after_iteration_X_stats_copy$time_passed_iter_X[2]
          plottable_df_copy$time_passed_after_30perc = time_after_iteration_X_stats_copy$time_passed_iter_X[3]
          plottable_df_copy$time_passed_after_40perc = time_after_iteration_X_stats_copy$time_passed_iter_X[4]
          plottable_df_copy$time_passed_after_50perc = time_after_iteration_X_stats_copy$time_passed_iter_X[5]
          plottable_df_copy$time_passed_after_60perc = time_after_iteration_X_stats_copy$time_passed_iter_X[1]
          plottable_df_copy$time_passed_after_70perc = time_after_iteration_X_stats_copy$time_passed_iter_X[7]
          plottable_df_copy$time_passed_after_80perc = time_after_iteration_X_stats_copy$time_passed_iter_X[8]
          plottable_df_copy$time_passed_after_90perc = time_after_iteration_X_stats_copy$time_passed_iter_X[9]
          
          plottable_df_copy$time_passed_iter_diff_10_20 = time_after_iteration_X_stats_copy$time_passed_iter_diff[1]
          plottable_df_copy$time_passed_iter_diff_20_30 = time_after_iteration_X_stats_copy$time_passed_iter_diff[2]
          plottable_df_copy$time_passed_iter_diff_30_40 = time_after_iteration_X_stats_copy$time_passed_iter_diff[3]
          plottable_df_copy$time_passed_iter_diff_40_50 = time_after_iteration_X_stats_copy$time_passed_iter_diff[4]
          plottable_df_copy$time_passed_iter_diff_50_60 = time_after_iteration_X_stats_copy$time_passed_iter_diff[5]
          plottable_df_copy$time_passed_iter_diff_60_70 = time_after_iteration_X_stats_copy$time_passed_iter_diff[6]
          plottable_df_copy$time_passed_iter_diff_70_80 = time_after_iteration_X_stats_copy$time_passed_iter_diff[7]
          plottable_df_copy$time_passed_iter_diff_80_90 = time_after_iteration_X_stats_copy$time_passed_iter_diff[8]
          
          plottable_df_copy$Num_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Num_time_X
          plottable_df_copy$Min_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Min_time_X
          plottable_df_copy$Max_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Max_time_X
          plottable_df_copy$Mean_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Mean_time_X
          plottable_df_copy$Mode_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Mode_time_X
          plottable_df_copy$Median_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Median_time_X
          plottable_df_copy$Quantiles_time_X0 = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Quantiles_time_X[1]
          plottable_df_copy$Quantiles_time_X25 = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Quantiles_time_X[2]
          plottable_df_copy$Quantiles_time_X50 = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Quantiles_time_X[3]
          plottable_df_copy$Quantiles_time_X75 = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Quantiles_time_X[4]
          plottable_df_copy$Quantiles_time_X100 = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Quantiles_time_X[5]
          plottable_df_copy$SD_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$SD_time_X
          plottable_df_copy$Var_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Var_time_X
          plottable_df_copy$Skew_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Skew_time_X
          plottable_df_copy$Span_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Span_time_X
          plottable_df_copy$Varcoeff_time_X = time_after_iteration_X_stats_copy$time_passed_iter_X_diff_stats$Varcoeff_time_X
          
          #========================================================================================
          # 4) Covergence Speed 1, 2, and 3 REAL
          #========================================================================================
          solvertraj_real = getAreaAVGInc(solvertraj_real, FALSE)
          ConvSpeed_1_copy = getConvSpeed_1(solvertraj_real, TRUE)
          
          plottable_df_copy$convPerIter_AVG = ConvSpeed_1_copy$convIter # ((lastAreaVal / firstAreaVal) / n) 
          plottable_df_copy$convPerIter_AVG_2 = ConvSpeed_1_copy$convIter_2 # ((firstAreaVal - lastAreaVal) / n) 
          plottable_df_copy$convPerTime_AVG = ConvSpeed_1_copy$convTime
          plottable_df_copy$convPerTime_AVG_2 = ConvSpeed_1_copy$convTime_2
          
          plottable_df_copy$convOverIter = ConvSpeed_1_copy$area_improvement_iter # 1L - (lastAreaVal / firstAreaVal)
          #plottable_df_copy$convPerIter = ConvSpeed_1_copy$area_improvement_time  # TODO: da passt noch was nicht
          plottable_df_copy$Num_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Num_rectangle_iter
          plottable_df_copy$Min_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Min_rectangle_iter
          plottable_df_copy$Max_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Max_rectangle_iter
          plottable_df_copy$Mean_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Mean_rectangle_iter
          plottable_df_copy$Mode_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Mode_rectangle_iter
          plottable_df_copy$Median_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Median_rectangle_iter
          plottable_df_copy$Quantiles_rectangle_iter0 = ConvSpeed_1_copy$rect_stats_iter$Quantiles_rectangle_iter[1] %>% as.numeric(.)
          plottable_df_copy$Quantiles_rectangle_iter25 = ConvSpeed_1_copy$rect_stats_iter$Quantiles_rectangle_iter[2] %>% as.numeric(.)
          plottable_df_copy$Quantiles_rectangle_iter50 = ConvSpeed_1_copy$rect_stats_iter$Quantiles_rectangle_iter[3] %>% as.numeric(.)
          plottable_df_copy$Quantiles_rectangle_iter75 = ConvSpeed_1_copy$rect_stats_iter$Quantiles_rectangle_iter[4] %>% as.numeric(.)
          plottable_df_copy$Quantiles_rectangle_iter100 = ConvSpeed_1_copy$rect_stats_iter$Quantiles_rectangle_iter[5] %>% as.numeric(.)
          plottable_df_copy$SD_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$SD_rectangle_iter
          plottable_df_copy$Var_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Var_rectangle_iter
          plottable_df_copy$Skew_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Skew_rectangle_iter
          plottable_df_copy$Span_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Span_rectangle_iter
          plottable_df_copy$Varcoeff_rectangle_iter = ConvSpeed_1_copy$rect_stats_iter$Varcoeff_rectangle_iter
          
          areasls_copy = calcTrigonometricAreas(solvertraj_real, TRUE, TRUE) 
          plottable_df_copy$triangle = areasls_copy$triangle
          plottable_df_copy$tri_peak= areasls_copy$tri_peak
          plottable_df_copy$tri_sideA = areasls_copy$tri_sideA
          plottable_df_copy$tri_sideB = areasls_copy$tri_sideB
          plottable_df_copy$tri_sideC = areasls_copy$tri_sideC
          plottable_df_copy$trapezoid = areasls_copy$trapezoid
          
          ConvSpeed_2_copy = getConvSpeed_2(solvertraj_real, areasls$triangle, areasls$trapezoid)
          plottable_df_copy$convergence_idealized = ConvSpeed_2_copy$convergence_idealized
          plottable_df_copy$convergence_idealized_norm = ConvSpeed_2_copy$convergence_idealized_norm
          plottable_df_copy$convergence_step_tri = ConvSpeed_2_copy$convergence_step_tri
          quality_indication_copy = getConvQuality(solvertraj_real)
          plottable_df_copy$dist_begin = quality_indication_copy$dist_begin
          plottable_df_copy$dist_end = quality_indication_copy$dist_end
          plottable_df_copy$quality_drift = quality_indication_copy$quality_drift
          
          ConvSpeed_3_copy = getConvSpeed_3(solvertraj_real)
          plottable_df_copy$convSpeed_3 = ConvSpeed_3_copy$conv_speed
          plottable_df_copy$incumbant_slope = ConvSpeed_3_copy$slopes$incumbant
          plottable_df_copy$average.fitness_slope = ConvSpeed_3_copy$slopes$average.fitness
          
          #========================================================================================
          # 5) inc_LB area REAL
          #========================================================================================
          
          # +++ new +++
          plottable_df_copy$sum_of_lowest_edge_values_ORIG = sum_of_lowest_edge_values
          if(scaling == TRUE){
            plottable_df_copy$sum_of_lowest_edge_values_SCALED = LB_scaled
          }
          #updated
          plottable_df_copy$IncLB_area_INC = inc_lb_stat_copy.INC$area_sum
          plottable_df_copy$IncLB_area_AVGFIT = inc_lb_stat_copy.AVGGIT$area_sum
          # +++ new +++
          plottable_df_copy$IncLB_INC_AVGFFIT_diff = inc_lb_stat_copy.AVGGIT$area_sum - inc_lb_stat_copy.INC$area_sum
          
          plottable_df_copy$Num_areas_INC = inc_lb_stat_copy.INC$area_stats$Num_areas
          plottable_df_copy$Min_areas_INC = inc_lb_stat_copy.INC$area_stats$Min_areas
          plottable_df_copy$Max_areas_INC = inc_lb_stat_copy.INC$area_stats$Max_areas
          plottable_df_copy$Mean_areas_INC = inc_lb_stat_copy.INC$area_stats$Mean_areas
          plottable_df_copy$Mode_areas_INC = inc_lb_stat_copy.INC$area_stats$Mode_areas
          plottable_df_copy$Median_areas_INC = inc_lb_stat_copy.INC$area_stats$Median_areas
          plottable_df_copy$Quantiles_areas0_INC = inc_lb_stat_copy.INC$area_stats$Quantiles_areas[1]
          plottable_df_copy$Quantiles_areas25_INC = inc_lb_stat_copy.INC$area_stats$Quantiles_areas[2]
          plottable_df_copy$Quantiles_areas50_INC = inc_lb_stat_copy.INC$area_stats$Quantiles_areas[3]
          plottable_df_copy$Quantiles_areas75_INC = inc_lb_stat_copy.INC$area_stats$Quantiles_areas[4]
          plottable_df_copy$Quantiles_areas100_INC = inc_lb_stat_copy.INC$area_stats$Quantiles_areas[5]
          plottable_df_copy$SD_areas_INC = inc_lb_stat_copy.INC$area_stats$SD_areas
          plottable_df_copy$Var_areas_INC = inc_lb_stat_copy.INC$area_stats$Var_areas
          plottable_df_copy$Skew_areas_INC = inc_lb_stat_copy.INC$area_stats$Skew_areas
          plottable_df_copy$Span_areas_INC = inc_lb_stat_copy.INC$area_stats$Span_areas
          plottable_df_copy$Varcoeff_areas_INC = inc_lb_stat_copy.INC$area_stats$Varcoeff_areas
          
          plottable_df_copy$Num_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Num_areas
          plottable_df_copy$Min_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Min_areas
          plottable_df_copy$Max_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Max_areas
          plottable_df_copy$Mean_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Mean_areas
          plottable_df_copy$Mode_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Mode_areas
          plottable_df_copy$Median_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Median_areas
          plottable_df_copy$Quantiles_areas0_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Quantiles_areas[1]
          plottable_df_copy$Quantiles_areas25_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Quantiles_areas[2]
          plottable_df_copy$Quantiles_areas50_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Quantiles_areas[3]
          plottable_df_copy$Quantiles_areas75_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Quantiles_areas[4]
          plottable_df_copy$Quantiles_areas100_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Quantiles_areas[5]
          plottable_df_copy$SD_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$SD_areas
          plottable_df_copy$Var_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Var_areas
          plottable_df_copy$Skew_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Skew_areas
          plottable_df_copy$Span_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Span_areas
          plottable_df_copy$Varcoeff_areas_AVGFIT = inc_lb_stat_copy.AVGGIT$area_stats$Varcoeff_areas
          
          # +++ new +++
          plottable_df_copy$Inc_Avg_diff = inc_lb_stat_copy.AVGGIT$area_sum - inc_lb_stat_copy.INC$area_sum
          #area_inc_lb_ratio.INC = inc_lb_stat.INC$area_sum / inc_lb_stat_copy.INC$area_sum
          plottable_df_copy$area_inc_lb_ratio.INC = area_inc_lb_ratio.INC
          # +++new+++:
          #area_inc_lb_ratio.AVGFIT = inc_lb_stat.AVGFIT$area_sum / inc_lb_stat_copy.AVGGIT$area_sum
          plottable_df_copy$area_inc_lb_ratio.AVGFIT = area_inc_lb_ratio.AVGFIT
          # +++new+++:
          area_inc_lb_ratio.INC_AVGFIT_diff = area_inc_lb_ratio.INC - area_inc_lb_ratio.AVGFIT
          plottable_df_copy$area_inc_lb_ratio.INC_AVGFIT_diff = area_inc_lb_ratio.INC_AVGFIT_diff
          
          #========================================================================================
          # 6) success ratio REAL
          #========================================================================================
          success_ratio_copy = getSuccessRatio(solvertraj_real)
          plottable_df_copy$success_ratio = success_ratio_copy$successRatio
          plottable_df_copy$not_successful = success_ratio_copy$num_NoSuccess
          #plottable_df_copy$num_total = success_ratio_copy$num_total
          
          #========================================================================================
          # 7) Plateau Statistics REAL
          #========================================================================================
          if(attr(solvertraj_real, "plateaunized") == TRUE) {
            Plat_stats_copy = getPlateauStats(solvertraj_real, success_ratio_copy$successRatio)
            
            plottable_df_copy$Num_Plateau_Length = Plat_stats_copy$plat_stats$Num_Plateau_Length
            plottable_df_copy$Min_Plateau_Length = Plat_stats_copy$plat_stats$Min_Plateau_Length
            plottable_df_copy$Max_Plateau_Length = Plat_stats_copy$plat_stats$Max_Plateau_Length
            plottable_df_copy$Mean_Plateau_Length = Plat_stats_copy$plat_stats$Mean_Plateau_Length
            plottable_df_copy$Mode_Plateau_Length = Plat_stats_copy$plat_stats$Mode_Plateau_Length
            plottable_df_copy$Median_Plateau_Length = Plat_stats_copy$plat_stats$Median_Plateau_Length
            plottable_df_copy$Quantiles_Plateau_Length0 = Plat_stats_copy$plat_stats$Quantiles_Plateau_Length[1]
            plottable_df_copy$Quantiles_Plateau_Length25 = Plat_stats_copy$plat_stats$Quantiles_Plateau_Length[2]
            plottable_df_copy$Quantiles_Plateau_Length50 = Plat_stats_copy$plat_stats$Quantiles_Plateau_Length[3]
            plottable_df_copy$Quantiles_Plateau_Length75 = Plat_stats_copy$plat_stats$Quantiles_Plateau_Length[4]
            plottable_df_copy$Quantiles_Plateau_Length100 = Plat_stats_copy$plat_stats$Quantiles_Plateau_Length[5]
            plottable_df_copy$SD_Plateau_Length = Plat_stats_copy$plat_stats$SD_Plateau_Length
            plottable_df_copy$Var_Plateau_Length = Plat_stats_copy$plat_stats$Var_Plateau_Length
            plottable_df_copy$Skew_Plateau_Length = Plat_stats_copy$plat_stats$Skew_Plateau_Length
            plottable_df_copy$Span_Plateau_Length = Plat_stats_copy$plat_stats$Span_Plateau_Length
            plottable_df_copy$Varcoeff_Plateau_Length = Plat_stats_copy$plat_stats$Varcoeff_Plateau_Length
          } else {
            plottable_df_copy$Num_Plateau_Length = 0L   # or NA? 
            plottable_df_copy$Min_Plateau_Length = 0L
            plottable_df_copy$Max_Plateau_Length = 0L
            plottable_df_copy$Mean_Plateau_Length = 0L
            plottable_df_copy$Mode_Plateau_Length = 0L
            plottable_df_copy$Median_Plateau_Length = 0L
            plottable_df_copy$Quantiles_Plateau_Length0 = 0L
            plottable_df_copy$Quantiles_Plateau_Length25 = 0L
            plottable_df_copy$Quantiles_Plateau_Length50 = 0L
            plottable_df_copy$Quantiles_Plateau_Length75 = 0L
            plottable_df_copy$Quantiles_Plateau_Length100 = 0L
            plottable_df_copy$SD_Plateau_Length = 0L
            plottable_df_copy$Var_Plateau_Length = 0L
            plottable_df_copy$Skew_Plateau_Length = 0L
            plottable_df_copy$Span_Plateau_Length = 0L
            plottable_df_copy$Varcoeff_Plateau_Length = 0L
          }
          
          #========================================================================================
          # 8) Plateau Start Statistics REAL
          #========================================================================================
          if(attr(solvertraj_real,"plateaunized") == TRUE) {
            Plat_start_stats_copy = getPlateauStartStats(solvertraj_real, 4, success_ratio_copy$successRatio)
          }
          if(exists("Plat_start_stats_copy") & !is.na(Plat_start_stats_copy$plat_start$Plats_area_begin_X)){
            plottable_df_copy$plat_start_X = Plat_start_stats_copy$plat_start$Plats_area_begin_X
            plottable_df_copy$Plats_area_begin_X_abs = Plat_start_stats_copy$plat_start$Plats_area_begin_X_abs
            plottable_df_copy$plat_start_Y = Plat_start_stats_copy$plat_start$Plats_area_begin_Y
            plottable_df_copy$Plats_area_begin_Y_abs = Plat_start_stats_copy$plat_start$Plats_area_begin_Y_abs
            
            plottable_df_copy$Num_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Num_plat_start_X_abs
            plottable_df_copy$Min_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Min_plat_start_X_abs
            plottable_df_copy$Max_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Max_plat_start_X_abs
            plottable_df_copy$Mean_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Mean_plat_start_X_abs
            plottable_df_copy$Mode_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Mode_plat_start_X_abs
            plottable_df_copy$Median_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Median_plat_start_X_abs
            plottable_df_copy$Quantiles_plat_start_X_abs0 = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[1]
            plottable_df_copy$Quantiles_plat_start_X_abs25 = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[2]
            plottable_df_copy$Quantiles_plat_start_X_abs50 = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[3]
            plottable_df_copy$Quantiles_plat_start_X_abs75 = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[4]
            plottable_df_copy$Quantiles_plat_start_X_abs100 = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Quantiles_plat_start_X_abs[5]
            plottable_df_copy$SD_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$SD_plat_start_X_abs
            plottable_df_copy$Var_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Var_plat_start_X_abs
            plottable_df_copy$Skew_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Skew_plat_start_X_abs
            plottable_df_copy$Span_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Span_plat_start_X_abs
            plottable_df_copy$Varcoeff_plat_start_X_abs = Plat_start_stats_copy$plat_start$plat_start_X_abs_Stats$Varcoeff_plat_start_X_abs
            
            # +++ new +++ : X_rel, Y_abs
            plottable_df_copy$Num_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Num_plat_start_X_rel
            plottable_df_copy$Min_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Min_plat_start_X_rel
            plottable_df_copy$Max_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Max_plat_start_X_rel
            plottable_df_copy$Mean_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Mean_plat_start_X_rel
            plottable_df_copy$Mode_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Mode_plat_start_X_rel
            plottable_df_copy$Median_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Median_plat_start_X_rel
            plottable_df_copy$Quantiles_plat_start_X_rel0 = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[1]
            plottable_df_copy$Quantiles_plat_start_X_rel25 = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[2]
            plottable_df_copy$Quantiles_plat_start_X_rel50 = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[3]
            plottable_df_copy$Quantiles_plat_start_X_rel75 = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[4]
            plottable_df_copy$Quantiles_plat_start_X_rel100 = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Quantiles_plat_start_X_rel[5]
            plottable_df_copy$SD_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$SD_plat_start_X_rel
            plottable_df_copy$Var_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Var_plat_start_X_rel
            plottable_df_copy$Skew_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Skew_plat_start_X_rel
            plottable_df_copy$Span_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Span_plat_start_X_rel
            plottable_df_copy$Varcoeff_plat_start_X_rel = Plat_start_stats_copy$plat_start$plat_start_X_rel_Stats$Varcoeff_plat_start_X_rel
            
            plottable_df_copy$Num_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Num_plat_start_Y_abs
            plottable_df_copy$Min_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Min_plat_start_Y_abs
            plottable_df_copy$Max_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Max_plat_start_Y_abs
            plottable_df_copy$Mean_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Mean_plat_start_Y_abs
            plottable_df_copy$Mode_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Mode_plat_start_Y_abs
            plottable_df_copy$Median_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Median_plat_start_Y_abs
            plottable_df_copy$Quantiles_plat_start_Y_abs0 = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[1]
            plottable_df_copy$Quantiles_plat_start_Y_abs25 = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[2]
            plottable_df_copy$Quantiles_plat_start_Y_abs50 = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[3]
            plottable_df_copy$Quantiles_plat_start_Y_abs75 = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[4]
            plottable_df_copy$Quantiles_plat_start_Y_abs100 = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Quantiles_plat_start_Y_abs[5]
            plottable_df_copy$SD_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$SD_plat_start_Y_abs
            plottable_df_copy$Var_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Var_plat_start_Y_abs
            plottable_df_copy$Skew_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Skew_plat_start_Y_abs
            plottable_df_copy$Span_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Span_plat_start_Y_abs
            plottable_df_copy$Varcoeff_plat_start_Y_abs = Plat_start_stats_copy$plat_start$plat_start_Y_abs_Stats$Varcoeff_plat_start_Y_abs
            
            plottable_df_copy$Num_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Num_plat_start_Y_rel
            plottable_df_copy$Min_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Min_plat_start_Y_rel
            plottable_df_copy$Max_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Max_plat_start_Y_rel
            plottable_df_copy$Mean_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Mean_plat_start_Y_rel
            plottable_df_copy$Mode_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Mode_plat_start_Y_rel
            plottable_df_copy$Median_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Median_plat_start_Y_rel
            plottable_df_copy$Quantiles_plat_start_Y_rel0 = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[1]
            plottable_df_copy$Quantiles_plat_start_Y_rel25 = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[2]
            plottable_df_copy$Quantiles_plat_start_Y_rel50 = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[3]
            plottable_df_copy$Quantiles_plat_start_Y_rel75 = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[4]
            plottable_df_copy$Quantiles_plat_start_Y_rel100 = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Quantiles_plat_start_Y_rel[5]
            plottable_df_copy$SD_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$SD_plat_start_Y_rel
            plottable_df_copy$Var_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Var_plat_start_Y_rel
            plottable_df_copy$Skew_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Skew_plat_start_Y_rel
            plottable_df_copy$Span_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Span_plat_start_Y_rel
            plottable_df_copy$Varcoeff_plat_start_Y_rel = Plat_start_stats_copy$plat_start$plat_start_Y_rel_Stats$Varcoeff_plat_start_Y_rel
            
            plottable_df_copy$characteristic_volume_X = Plat_start_stats_copy$plat_start$characteristic_volume_X
            plottable_df_copy$characteristic_volume_Y = Plat_start_stats_copy$plat_start$characteristic_volume_Y
            plottable_df_copy$characteristic_volume = Plat_start_stats_copy$plat_start$characteristic_volume
          } else {
            plottable_df_copy$plat_start_X = 0L               
            plottable_df_copy$Plats_area_begin_X_abs = 0L
            plottable_df_copy$plat_start_Y = 0L
            plottable_df_copy$Plats_area_begin_Y_abs = 0L
            
            plottable_df_copy$Num_plat_start_X_abs = 0L
            plottable_df_copy$Min_plat_start_X_abs = 0L
            plottable_df_copy$Max_plat_start_X_abs = 0L
            plottable_df_copy$Mean_plat_start_X_abs = 0L
            plottable_df_copy$Mode_plat_start_X_abs = 0L
            plottable_df_copy$Median_plat_start_X_abs = 0L
            plottable_df_copy$Quantiles_plat_start_X_abs0 = 0L
            plottable_df_copy$Quantiles_plat_start_X_abs25 = 0L
            plottable_df_copy$Quantiles_plat_start_X_abs50 = 0L
            plottable_df_copy$Quantiles_plat_start_X_abs75 = 0L
            plottable_df_copy$Quantiles_plat_start_X_abs100 = 0L
            plottable_df_copy$SD_plat_start_X_abs = 0L
            plottable_df_copy$Var_plat_start_X_abs = 0L
            plottable_df_copy$Skew_plat_start_X_abs = 0L
            plottable_df_copy$Span_plat_start_X_abs = 0L
            plottable_df_copy$Varcoeff_plat_start_X_abs = 0L
            
            # +++ new +++ : X_rel, Y_abs
            plottable_df_copy$Num_plat_start_X_rel = 0L
            plottable_df_copy$Min_plat_start_X_rel = 0L
            plottable_df_copy$Max_plat_start_X_rel = 0L
            plottable_df_copy$Mean_plat_start_X_rel = 0L
            plottable_df_copy$Mode_plat_start_X_rel = 0L
            plottable_df_copy$Median_plat_start_X_rel = 0L
            plottable_df_copy$Quantiles_plat_start_X_rel0 = 0L
            plottable_df_copy$Quantiles_plat_start_X_rel25 = 0L
            plottable_df_copy$Quantiles_plat_start_X_rel50 = 0L
            plottable_df_copy$Quantiles_plat_start_X_rel75 = 0L
            plottable_df_copy$Quantiles_plat_start_X_rel100 = 0L
            plottable_df_copy$SD_plat_start_X_rel = 0L
            plottable_df_copy$Var_plat_start_X_rel = 0L
            plottable_df_copy$Skew_plat_start_X_rel = 0L
            plottable_df_copy$Span_plat_start_X_rel = 0L
            plottable_df_copy$Varcoeff_plat_start_X_rel = 0L
            
            plottable_df_copy$Num_plat_start_Y_abs = 0L
            plottable_df_copy$Min_plat_start_Y_abs = 0L
            plottable_df_copy$Max_plat_start_Y_abs = 0L
            plottable_df_copy$Mean_plat_start_Y_abs = 0L
            plottable_df_copy$Mode_plat_start_Y_abs = 0L
            plottable_df_copy$Median_plat_start_Y_abs = 0L
            plottable_df_copy$Quantiles_plat_start_Y_abs0 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_abs25 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_abs50 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_abs75 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_abs100 = 0L
            plottable_df_copy$SD_plat_start_Y_abs = 0L
            plottable_df_copy$Var_plat_start_Y_abs = 0L
            plottable_df_copy$Skew_plat_start_Y_abs = 0L
            plottable_df_copy$Span_plat_start_Y_abs = 0L
            plottable_df_copy$Varcoeff_plat_start_Y_abs = 0L
            
            plottable_df_copy$Num_plat_start_Y_rel = 0L
            plottable_df_copy$Min_plat_start_Y_rel = 0L
            plottable_df_copy$Max_plat_start_Y_rel = 0L
            plottable_df_copy$Mean_plat_start_Y_rel = 0L
            plottable_df_copy$Mode_plat_start_Y_rel = 0L
            plottable_df_copy$Median_plat_start_Y_rel = 0L
            plottable_df_copy$Quantiles_plat_start_Y_rel0 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_rel25 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_rel50 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_rel75 = 0L
            plottable_df_copy$Quantiles_plat_start_Y_rel100 = 0L
            plottable_df_copy$SD_plat_start_Y_rel = 0L
            plottable_df_copy$Var_plat_start_Y_rel = 0L
            plottable_df_copy$Skew_plat_start_Y_rel = 0L
            plottable_df_copy$Span_plat_start_Y_rel = 0L
            plottable_df_copy$Varcoeff_plat_start_Y_rel = 0L
            
            plottable_df_copy$characteristic_volume_X = 0L
            plottable_df_copy$characteristic_volume_Y = 0L
            plottable_df_copy$characteristic_volume = 0L
          }
          
          #========================================================================================
          # 9.1) VG REAL
          #========================================================================================
          VG_stats_copy = getVG_stats(solvertraj_real)
          plottable_df_copy$vg_threshold = VG_stats_copy$vg_stats$vg_threshold
          
          plottable_df_copy$Num_Vertical_Gaps = VG_stats_copy$vg_stats$Num_Vertical_Gaps
          plottable_df_copy$Min_Vertical_Gaps = VG_stats_copy$vg_stats$Min_Vertical_Gaps
          plottable_df_copy$Max_Vertical_Gaps = VG_stats_copy$vg_stats$Max_Vertical_Gaps
          plottable_df_copy$Mean_Vertical_Gaps = VG_stats_copy$vg_stats$Mean_Vertical_Gaps
          plottable_df_copy$Mode_Vertical_Gaps = VG_stats_copy$vg_stats$Mode_Vertical_Gaps
          plottable_df_copy$Median_Vertical_Gaps = VG_stats_copy$vg_stats$Median_Vertical_Gaps
          plottable_df_copy$Quantiles_Vertical_Gaps0 = VG_stats_copy$vg_stats$Quantiles_Vertical_Gaps[1]
          plottable_df_copy$Quantiles_Vertical_Gaps25 = VG_stats_copy$vg_stats$Quantiles_Vertical_Gaps[2]
          plottable_df_copy$Quantiles_Vertical_Gaps50= VG_stats_copy$vg_stats$Quantiles_Vertical_Gaps[3]
          plottable_df_copy$Quantiles_Vertical_Gaps75 = VG_stats_copy$vg_stats$Quantiles_Vertical_Gaps[4]
          plottable_df_copy$Quantiles_Vertical_Gaps100 = VG_stats_copy$vg_stats$Quantiles_Vertical_Gaps[5]
          plottable_df_copy$SD_Vertical_Gaps = VG_stats_copy$vg_stats$SD_Vertical_Gaps
          plottable_df_copy$Var_Vertical_Gaps = VG_stats_copy$vg_stats$Var_Vertical_Gaps
          plottable_df_copy$Skew_Vertical_Gaps = VG_stats_copy$vg_stats$Skew_Vertical_Gaps
          plottable_df_copy$Span_Vertical_Gaps = VG_stats_copy$vg_stats$Span_Vertical_Gaps
          plottable_df_copy$Varcoeff_Vertical_Gaps = VG_stats_copy$vg_stats$Varcoeff_Vertical_Gaps
          
          # new: requires default stats: span incumbent
          plottable_df_copy$meanVG_Span_ratio = VG_stats_copy$vg_stats$Mean_Vertical_Gaps / 
            default_traj_stats$incumbent_stat_ls_real$Span_incumbent_copy
          
          #========================================================================================
          # 9.2) VG Start REAL
          #========================================================================================
          VG_start_stats_copy = getVGStartStats(solvertraj_real, VG_stats, 5, TRUE) 
          
          plottable_df_copy$impro_ratio_10 = (VG_start_stats_copy$ratio_first_perc_ls[1] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$impro_ratio_20 = (VG_start_stats_copy$ratio_first_perc_ls[2] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$impro_ratio_30 = (VG_start_stats_copy$ratio_first_perc_ls[3] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$impro_ratio_40 = (VG_start_stats_copy$ratio_first_perc_ls[4] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$impro_ratio_50 = (VG_start_stats_copy$ratio_first_perc_ls[5] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_amnt_10 = (VG_start_stats_copy$amnt_ls[1] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_amnt_20 = (VG_start_stats_copy$amnt_ls[2] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_amnt_30 = (VG_start_stats_copy$amnt_ls[3] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_amnt_40 = (VG_start_stats_copy$amnt_ls[4] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_amnt_50 = (VG_start_stats_copy$amnt_ls[5] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_ratio_10 = (VG_start_stats_copy$ratio_ls[1] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_ratio_20 = (VG_start_stats_copy$ratio_ls[2] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_ratio_30 = (VG_start_stats_copy$ratio_ls[3] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_ratio_40 = (VG_start_stats_copy$ratio_ls[4] %>% unlist(.)) %>% as.numeric(.)
          plottable_df_copy$VG_ratio_50 = (VG_start_stats_copy$ratio_ls[5] %>% unlist(.)) %>% as.numeric(.)
          
          plottable_df_copy$firstVG_X_abs = VG_start_stats_copy$firstVG_X_abs
          plottable_df_copy$lastVG_X_abs = VG_start_stats_copy$lastVG_X_abs
          
          plottable_df_copy$firstVG_X_rel = VG_start_stats_copy$firstVG_X_rel
          plottable_df_copy$lastVG_X_rel = VG_start_stats_copy$lastVG_X_rel
          
          #========================================================================================
          # 10) Gap Power REAL 
          #========================================================================================
          gap_power_copy = getGapPower(VG_stats_copy, solvertraj_real) 
          plottable_df_copy$gap_power = gap_power_copy$stat_ellbow_power
          plottable_df_copy$gap_power_norm = gap_power_copy$stat_ellbow_power_norm
          
          #========================================================================================
          # 11) improvements REAL
          #========================================================================================
          # backwards <- T
          plottable_df_copy$impr_last_1 =  (getLastXimprovement(solvertraj_real, 0.1)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_last_2 =  (getLastXimprovement(solvertraj_real, 0.2)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_last_3 =  (getLastXimprovement(solvertraj_real, 0.3)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_last_4 =  (getLastXimprovement(solvertraj_real, 0.4)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_last_5 =  (getLastXimprovement(solvertraj_real, 0.5)[3] %>% unlist(.)) %>% as.double(.)
          # backwards <- F
          plottable_df_copy$impr_first_1 =  (getLastXimprovement(solvertraj_real, 0.1, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_first_2 =  (getLastXimprovement(solvertraj_real, 0.2, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_first_3 =  (getLastXimprovement(solvertraj_real, 0.3, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_first_4 =  (getLastXimprovement(solvertraj_real, 0.4, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_first_5 =  (getLastXimprovement(solvertraj_real, 0.5, backwards = F)[3] %>% unlist(.)) %>% as.double(.)
          # from to
          plottable_df_copy$impr_0_to_01 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.0, to = 0.1)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_01_to_02 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.1, to = 0.2)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_02_to_03 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.2, to = 0.3)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_03_to_04 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.3, to = 0.4)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_04_to_05 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.4, to = 0.5)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_05_to_06 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.5, to = 0.6)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_06_to_07 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.6, to = 0.7)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_07_to_08 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.7, to = 0.8)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_08_to_09 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.8, to = 0.9)[3] %>% unlist(.)) %>% as.double(.)
          plottable_df_copy$impr_09_to_1 =  (getLastXimprovement(solvertraj_real, from_to = T, from = 0.9, to = 1.0)[3] %>% unlist(.)) %>% as.double(.)
          
          plottable_df_copy$impr_025_to_075 =(getLastXimprovement(solvertraj_real, from_to = T, from = 0.25, to = 0.75)[3] %>% unlist(.)) %>% as.double(.)
          
          #========================================================================================
          # 12.1) Slopes Stats REAL (# takes too long with current R implementation )
          #========================================================================================
          #slope_stats_copy = getSlopeStats(solvertraj_real)
          # take the effective slopes stats, since those bear information --> redundancy over loss of information
          plottable_df_copy$Num_slopes = slope_stats$resls_EXCL_ZERO_impro$Num_slopes # 0 before for all
          plottable_df_copy$Min_slopes = slope_stats$resls_EXCL_ZERO_impro$Min_slopes
          plottable_df_copy$Max_slopes = slope_stats$resls_EXCL_ZERO_impro$Max_slopes
          plottable_df_copy$Mean_slopes = slope_stats$resls_EXCL_ZERO_impro$Mean_slopes
          plottable_df_copy$Mode_slopes = slope_stats$resls_EXCL_ZERO_impro$Mode_slopes
          plottable_df_copy$Median_slopes = slope_stats$resls_EXCL_ZERO_impro$Median_slopes
          plottable_df_copy$Quantiles_slopes0 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[1]
          plottable_df_copy$Quantiles_slopes25 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[2]
          plottable_df_copy$Quantiles_slopes50 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[3]
          plottable_df_copy$Quantiles_slopes75 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[4]
          plottable_df_copy$Quantiles_slopes100 = slope_stats$resls_EXCL_ZERO_impro$Quantiles_slopes[5]
          plottable_df_copy$SD_slopes = slope_stats$resls_EXCL_ZERO_impro$SD_slopes
          plottable_df_copy$Var_slopes = slope_stats$resls_EXCL_ZERO_impro$Var_slopes
          plottable_df_copy$Skew_slopes = slope_stats$resls_EXCL_ZERO_impro$Skew_slopes
          plottable_df_copy$Span_slopes = slope_stats$resls_EXCL_ZERO_impro$Span_slopes
          plottable_df_copy$Varcoeff_slopes = slope_stats$resls_EXCL_ZERO_impro$Varcoeff_slopes
          
          #========================================================================================
          # 12.2) Slopes Direction Change Stats REAL
          #========================================================================================
          slope_dir_stat_copy = getSlopeDirectionStats(solvertraj_real)
          
          plottable_df_copy$slope_dir_changes = slope_dir_stat_copy$Num_direction_change
          
          plottable_df_copy$Min_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Min_slopes_i_n
          plottable_df_copy$Max_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Max_slopes_i_n
          plottable_df_copy$Mean_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Mean_slopes_i_n
          plottable_df_copy$Mode_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Mode_slopes_i_n
          plottable_df_copy$Median_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Median_slopes_i_n
          plottable_df_copy$Quantiles_slopes_i_n0 = slope_dir_stat_copy$slope_2_stats$Quantiles_slopes_i_n[1]
          plottable_df_copy$Quantiles_slopes_i_n25 = slope_dir_stat_copy$slope_2_stats$Quantiles_slopes_i_n[2]
          plottable_df_copy$Quantiles_slopes_i_n50 = slope_dir_stat_copy$slope_2_stats$Quantiles_slopes_i_n[3]
          plottable_df_copy$Quantiles_slopes_i_n75 = slope_dir_stat_copy$slope_2_stats$Quantiles_slopes_i_n[4]
          plottable_df_copy$Quantiles_slopes_i_n100 = slope_dir_stat_copy$slope_2_stats$Quantiles_slopes_i_n[5]
          plottable_df_copy$SD_slopes_i_n = slope_dir_stat_copy$slope_2_stats$SD_slopes_i_n
          plottable_df_copy$Span_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Span_slopes_i_n
          plottable_df_copy$Skew_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Skew_slopes_i_n
          plottable_df_copy$Span_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Span_slopes_i_n
          plottable_df_copy$Varcoeff_slopes_i_n = slope_dir_stat_copy$slope_2_stats$Varcoeff_slopes_i_n
          
          #========================================================================================
          # 13) violation stats REAL
          #========================================================================================
          if(length(Plat_start_stats_copy$data) != 0){
            violation_stats_copy = getVioPlat(solvertraj_real, VG_stats_copy, Plat_start_stats_copy, Plat_stats_copy)
            
            plottable_df_copy$VG_boarder_X = violation_stats_copy$VG_boarder_X
            plottable_df_copy$vio_ratio_PLAT = violation_stats_copy$vio_ratio_PLAT
            plottable_df_copy$vio_ratio_VG = violation_stats_copy$vio_ratio_VG
            plottable_df_copy$vio_ratio_PLAT_VG = violation_stats_copy$vio_ratio_PLAT_VG
          } else {
            plottable_df_copy$VG_boarder_X = 0
            plottable_df_copy$vio_ratio_PLAT = 0
            plottable_df_copy$vio_ratio_VG = 0
            plottable_df_copy$vio_ratio_PLAT_VG = 0
          }
          
          #========================================================================================
          # 14) CHULL REAL
          #========================================================================================
          chull_stats_copy = getCHULLratio(solvertraj_real)
          plottable_df_copy$chull_spanning_points = chull_stats_copy$chull_spanning_points
          plottable_df_copy$points_on_chull = chull_stats_copy$points_on_chull
          plottable_df_copy$ratio_chull_pointsOnchull = chull_stats_copy$ratio_chull_pointsOnchull
          plottable_df_copy$ratio_points_on_chull_allPoints = chull_stats_copy$ratio_points_on_chull_allPoints
          plottable_df_copy$ratio_chull_allPoints = chull_stats_copy$ratio_chull_allPoints
          
          #========================================================================================
          # 15) quadratic model fit REAL
          #========================================================================================
          qmcoeff_copy = getQuadraticModelCoeff(solvertraj_real, TRUE)
          
          plottable_df_copy$rsquared = qmcoeff_copy$summ_coeff_poly$r.squared
          plottable_df_copy$rsquared_adj = qmcoeff_copy$summ_coeff_poly$adj.r.squared
          plottable_df_copy$fstatistic_value = qmcoeff_copy$summ_coeff_poly$fstatistic[1] %>% as.double(.)
          plottable_df_copy$fstatistic_numdf = qmcoeff_copy$summ_coeff_poly$fstatistic[2] %>% as.double(.)
          plottable_df_copy$fstatistic_dendf = qmcoeff_copy$summ_coeff_poly$fstatistic[3] %>% as.double(.)
          plottable_df_copy$sigma = qmcoeff_copy$summ_coeff_poly$sigma
          plottable_df_copy$residuals_mean = qmcoeff_copy$summ_coeff_poly$residuals %>% mean(.)
          
          #========================================================================================
          # 16) Knee Stats REAL
          #========================================================================================
          if(exists("Plat_start_stats_copy") & !is.na(Plat_start_stats_copy$plat_start$Plats_area_begin_X)){
            knee_stats_copy = getKneeRatio(solvertraj_real, VG_stats_copy, Plat_start_stats_copy)
            plottable_df_copy$KneeAmnt = knee_stats_copy$knee_counter
            plottable_df_copy$KneeRatio = knee_stats_copy$knee_ratio
          } else {
            plottable_df_copy$KneeAmnt = 0L
            plottable_df_copy$KneeRatio = 0L
          }
          print(paste("could derive features in: ", l))
          
          #========================================================================================
          # capture results 
          #======================================================================================== 
          
          plottable_df_all_TMP = rbind(plottable_df, plottable_df_copy)
          # +++ new +++: if b = 0.1
          if(b == 0.25){
            plottable_df_all = plottable_df_all_TMP
          } else {
            plottable_df_all = rbind(plottable_df_all, plottable_df_all_TMP)
          }
        }

        #========================================================================================
        # append results
        #======================================================================================== 
        if(i == 1 & j == 1){
          big_res_ls[[paste("feats_", l, sep = "")]] <- plottable_df_all
        } else {
          big_res_ls[[paste("feats_", l, sep = "")]] <- rbind(big_res_ls[[paste("feats_", l, sep = "")]], plottable_df_all)
        }
        print(paste("could append: ", l))
      }
    }
  }
  return(big_res_ls)
}