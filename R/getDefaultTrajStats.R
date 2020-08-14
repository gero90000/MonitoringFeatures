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

  # ++++ new +++
  if(attr(solvertraj,'plateaunized') == T){
    plateau_found_effective = 1L
  } else {
    plateau_found_effective = 0L
  }

  effective_iter = solvertraj %>% .[length(.$iter), "iter"] #length(solvertraj$iter) - 1L # indexing != iteration
  effective_runtime = solvertraj %>% .[length(.$iter), "time.passed"] #solvertraj[effective_iter, "time.passed"]
  avg_iter_duration = effective_runtime / effective_iter

  # time per step related
  time_diff_stat_ls = makeStats("time_diff_eff", diff(solvertraj$time.passed))





  # ++++++++++++ TOOD: decouple it from absolute fitness value and make it a relative number +++++++++++
  #### incumbent and avg fitness related 
  # INCUMBANT
  # abs
  incumbent_stat_ls = makeStats("incumbent", solvertraj$incumbant)
  incumbent_diff_stat_ls = makeStats("incumbent_diff", diff(solvertraj$incumbant))
  # rel
  # TODO ...  there comes a new column




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
                      plateau_found_effective = plateau_found_effective,
                      time_per_iter_AVG_eff = avg_iter_duration,
                      TODO_biggest_drop_span_ratio = biggest_drop_span_ratio, #TODO: potentially being the VG theshold stat

                      time_diff_stat_ls_eff = time_diff_stat_ls,
                      incumbent_stat_ls_eff = incumbent_stat_ls,
                      incumbent_diff_stat_ls_eff = incumbent_diff_stat_ls,
                      avgFit_stat_ls_eff = avgFit_stat_ls,
                      avgFit_diff_stat_ls_eff = avgFit_diff_stat_ls
                      )
  
  ### Effective vs. Real
  # can only be called in case the last_plateau imputed version is passed as well 
  if(eff_real_stat & !missing(solvertraj_copy)){

    message("deriving default stats considering real and effective solver run relation")
    
    time_diff_stat_ls_real = makeStats("time_diff_real", diff(solvertraj_copy$time.passed))


    real_iter = solvertraj_copy %>% .[length(.$iter), "iter"] 
    real_iter_cleaned = real_iter - effective_iter
    real_runtime = (solvertraj_copy %>% .[length(.$iter), "time.passed"]) %>% round(., 0L)
    real_runtime_cleaned = real_runtime - effective_runtime


    #eff-real relation
    eff_real_iter_relation = (effective_iter / real_iter_cleaned)
    eff_real_time_relation = (effective_runtime / real_runtime_cleaned)

    effective_partion_iter = effective_iter / real_iter
    effective_partion_time = effective_runtime / real_runtime #5L   or just take the defined cutoff time

    avg_iter_duration_real = real_runtime / real_iter
    avg_iter_duration_real_cleaned = real_runtime_cleaned / real_iter_cleaned


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
                        real_runtime = real_runtime,
                        real_runtime_cleaned = real_runtime_cleaned, 
                        real_iterations = real_iter,
                        real_iterations_cleaned = real_iter_cleaned,
                        time_per_iter_AVG_real = avg_iter_duration_real,
                        time_per_iter_AVG_real_cleaned = avg_iter_duration_real_cleaned,
                        #biggestDrop_span = same

                        time_diff_stat_ls_real = time_diff_stat_ls_real,
                        incumbent_stat_ls_real = incumbent_stat_ls_copy,
                        incumbent_diff_stat_ls_real = incumbent_diff_stat_ls_copy, 
                        avgFit_stat_ls_real = avgFit_stat_ls_copy,
                        avgFit_diff_stat_ls_real = avgFit_diff_stat_ls_copy,

                        # proportional KPIs
                        # +++ new +++
                        eff_real_iter_relation = eff_real_iter_relation,
                        eff_real_time_relation = eff_real_time_relation,

                        effective_partion_iter = effective_partion_iter,
                        effective_partion_time = effective_partion_time,

                        incumbent_eff_real_ratio = Incumbent_Eff_real_ratio,
                        avgFit_eff_real_ratio = AVGfit_Eff_real_ratio
                        )
                                            
  } else {
    message("Either eff_real_stat is not set to TRUE or solver_trajectory_copy object has not been passed.")
  }
  return(resls)
}


# for convenience put as a new function
time_X_stats = function(solvertraj){
  resl = list()
  resls_time = c()
  for(i in 1:9){
    perc = switch(i,
                  0.1,
                  0.2,
                  0.3,
                  0.4,
                  0.5,
                  0.6,
                  0.7,
                  0.8,
                  0.9
    ) 
    iter_X = (perc *solvertraj[length(solvertraj$iter), "iter"]) %>% base::round(., 0L)
    time_passed_X = solvertraj[iter_X, "time.passed"]
    resls_time = c(resls_time, time_passed_X)
  }
  resls_time_diff = diff(resls_time)
  resls_time_diff_stats = MonitoringFeature::makeStats("time_X", resls_time_diff)
  
  resl = list.append(resl,
                     time_passed_iter_X = resls_time,
                     time_passed_iter__diff = resls_time_diff,
                     time_passed_iter_X_diff_stats = resls_time_diff_stats
                     )

}






