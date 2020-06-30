# TODO
# put roxygen comment here
# @author: Jacob, Pascal, Bjoern 
# @IDEA_ID_0.1: "GENERAL OVERVIEW STATISTICS OF EAX PRE SOLVING RUN"
# @description: 
#  returns general and feault staistics about 
#  cutoff-time-defined pre-solving run of EAX (+restart).
#  ideally used and featured with ideas in the future. 
#  (cf. commented ideas in code below, for example)

# can be used both for default stats for orig and copy (i.e., effective and real)
# but also for specialized effective stats when called like 
# getDefaultStats(res_eax$trajectory, res_eax_copy$trajectory, eff_real_stat = TRUE)
getDefaultStats = function(solvertraj, solvertraj_copy = "", eff_real_stat = FALSE) {
  resls = list()

  time_diff_stat_ls = makeStats("time_diff", diff(solvertraj$time.passed))
  effective_iter = length(solvertraj$iter) # -1 ????
  
  effective_runtime = res_eax$trajectory[effective_iter, "time.passed"]
  avg_iter_duration = effective_runtime / effective_iter

  #### incumbent and avg fitness related 
  # INCUMBANT
  incumbent_stat_ls = makeStats("incumbent", solvertraj$incumbant)
  incumbent_diff_stat_ls = makeStats("incumbent_diff", diff(solvertraj$incumbant))

  ##### CF IDEA ON BOARD (15.6.20)
  # idea: emprically derive a threshold here to declare a "drop" as "VG"
  biggest_drop_span_ratio = (incumbent_diff_stat_ls$Min_incumbent_diff / 
                               incumbent_stat_ls$Span_incumbent) %>% abs(.)
  # AVG FITNESS
  avgFit_stat_ls = makeStats("avgFit", solvertraj$average.fitness)
  avgFit_diff_stat_ls = makeStats("avgFit_diff", diff(solvertraj$average.fitness))

  #(incumbent_diff_stat_ls$Min_incumbent_diff - avgFit_diff_stat_ls$Min_avgFit_diff) %>% abs(.)
  #(incumbent_diff_stat_ls$Mean_incumbent_diff- avgFit_diff_stat_ls$Mean_avgFit_diff) %>% abs(.)
  resls = list.append(resls,
                      effective_runtime = effective_runtime,
                      effective_iterations = effective_iter,
                      time_per_iter_AVG= avg_iter_duration,
                      TODO_biggest_drop_span_ratio = biggest_drop_span_ratio, #TODO: potentially being the VG theshold stat

                      time_diff_stat_ls = time_diff_stat_ls,
                      incumbent_stat_ls = incumbent_stat_ls,
                      incumbent_diff_stat_ls = incumbent_diff_stat_ls,
                      avgFit_stat_ls = avgFit_stat_ls,
                      avgFit_diff_stat_ls = avgFit_diff_stat_ls
                      )
  
  ### Effective vs. Real
  # can only be called in case the last_plateau imputed version is passed as well 
  if(eff_real_stat & !missing(solvertraj_copy)){

    message("deriving default stats considering real and effective solver run relation")
    
    real_iter = length(solvertraj_copy$iter)
    effective_partion_iter = effective_iter / real_iter

    real_runtime = solvertraj_copy[real_iter, "time.passed"] %>% round(., 0)

    # partion of effective vs real solver runtime
    eff_real_time_ratio = effective_runtime / real_runtime #5L   or just take the defined cutoff time

    # partion of incumbent fitness between effective and real run
    incumbent_stat_ls_copy = makeStats("incumbent_copy", solvertraj_copy$incumbant)
    incumbent_diff_stat_ls_copy = makeStats("incumbent_diff", diff(solvertraj_copy$incumbant))
    Incumbent_Eff_real_ratio = (incumbent_stat_ls$Num_incumbent * incumbent_stat_ls$Mean_incumbent) / 
                               (incumbent_stat_ls_copy$Num_incumbent_copy * incumbent_stat_ls_copy$Mean_incumbent_copy)

    #print((incumbent_stat_ls$Num_incumbent * incumbent_stat_ls$Mean_incumbent))
    #print((incumbent_stat_ls_copy$Num_incumbent_copy * incumbent_stat_ls_copy$Mean_incumbent_copy))

    # portion of avg fitness between effective and real run
    avgFit_stat_ls_copy = makeStats("avgFit_copy", solvertraj_copy$average.fitness)
    avgFit_diff_stat_ls_copy = makeStats("avgFit_diff", diff(solvertraj_copy$average.fitness))
    AVGfit_Eff_real_ratio = (avgFit_stat_ls$Num_avgFit * avgFit_stat_ls$Mean_avgFit) / 
                            (avgFit_stat_ls_copy$Num_avgFit_copy * avgFit_stat_ls_copy$Mean_avgFit_copy)
    
    resls = list.append(resls,
                        real_iterations = real_iter,
                        real_runtime = real_runtime, 
                        eff_real_iter_ratio = effective_partion_iter,
                        eff_real_time_ratio = eff_real_time_ratio,

                        incumbent_eff_real_ratio = Incumbent_Eff_real_ratio,
                        avgFit_eff_real_ratio = AVGfit_Eff_real_ratio,

                        incumbent_stat_ls_real = incumbent_stat_ls_copy,
                        incumbent_diff_stat_ls_real = incumbent_diff_stat_ls_copy, 
                        avgFit_stat_ls_real = avgFit_stat_ls_copy,
                        avgFit_diff_stat_ls_real = avgFit_diff_stat_ls_copy
                        )
                        
                        
  } else {
    #message("Either eff_real_stat is not set to TRUE or solver_trajectory_copy object has not been passed.")
  }
  return(resls)
}

