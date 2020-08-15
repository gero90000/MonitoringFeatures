#TODO
# put roxygen2 comment here

# scales the avg.fitness and incumbent with min-max scaling,
# whereby min and max do relate on an per instance basis on the incumbent trajectory
scaler_orig = function(solvertraj){
  resls = list()
  ls = c("incumbant", "average.fitness")
  min_inc = min(solvertraj$incumbant)
  max_inc = max(solvertraj$incumbant)
  
  for(i in 1:length(ls)){
    tobe_scaled = ls[i]
    tmp = sapply(solvertraj[tobe_scaled], function(x){
            (x-min_inc) / (max_inc -min_inc)
    })
    resls[[i]] = tmp
  }
  # attr(solver_traj,'MIN_MAX_scaled') <- TRUE
  return(resls)
}

# shrink and log scale of data on an per-instance basis
scaler_other = function(solvertraj){
  res = list()
  plls = list()
  shapirols = list()
  resls = list()
  ls = c("incumbant", "average.fitness")
  
  for(i in 1:length(ls)){
    col = ls[i]
    # 1) shrink range
    tmp = sapply(solvertraj[col], function(x){
      x - min(solvertraj["incumbant"])
    })
    # 2) use natural logarithm
    tmp_2 = sapply(tmp, log)
    tmp_2[length(tmp_2)] <- 0L
    res[[i]] = tmp_2
    
    # TODO: fix
    plot_trans = plot(density(tmp_2))
    plls[[i]] = plot_trans
    
    tmp_test = shapiro.test(tmp_2)
    shapirols[[i]] <- tmp_test
  }
  
  resls = list.append(resls,
                      scales = res,
                      plots = plls,
                      shapirols = shapirols)
}

# FIXME: a little off since order of names and function list 
#        must be the "same" semantically
getStatSetNames = function() {
  return(c("Num", "Min", "Max", "Mean", "Mode",
    "Median", "Quantiles", "SD", "Var", "Skew", "Span", "Varcoeff"))
}

# helper for @makeStats
getStatFuncs = function(){
  stat.func = c("length(x)", "min(x)", "max(x)", "mean(x)", 
                "getMode(x)", "median(x)", "quantile(x)", 
                "sd(x)", "var(x)", "skew(x)",  "max(x) - min(x)", "mean(x)/sd(x)")
  func.container = list()
  for(i in 1:length(stat.func)){
    func.container[[i]] = function(x){} #step1 make list of functions
    body(func.container[[i]]) = base::parse(text=stat.func[i]) # step2 parse expressions into function body
  }
  return(func.container)
}

# old: get_mode
# helper for stat functions
getMode = function(list) {
  uniqElems = unique(list)
  list(x = uniqElems, y = list) %>% {
    .$x[which.max(tabulate(match(.$y, .$x)))]
  }
}

# old: make_stats
makeStats = function(name, stat_ls, stat_flag = T){
  stat_ls = unlist(stat_ls)
  
  stats = getStatSetNames()
  n = name
  names = list()
  for(i in 1:length(stats)){
    name = paste(stats[i], "_", n, sep = "")
    names[[i]] = name
    name = ""
  }

  funcs = getStatFuncs()
  stat_resls = list()
  for(i in 1:length(stats)){
    entry = names[[i]]
    if(stat_flag){
      fun_val = funcs[[i]](stat_ls)
    } else {
      fun_val = 0L
    }
    stat_resls[[entry]] = fun_val
  }
  return(stat_resls)
}

# TODO: read wickham
# to find out what best to do in case we want instance based function calls
# (http://adv-r.had.co.nz/Functions.html)
# in this case (same goes for init.plat...) we want to control, that getAreaAVGInc()
# is called before calls can be made to convSpeed_1 and 2 for an object 
#TODO: check assign() /S3 system to verify on that procedure 
'
init_plateaus = FALSE
init.plateaus = function(){
  init_plateaus <<- TRUE
}

init.areaAVG_Inc = FALSE
init.areaAVGINC = function(){
  init.areaAVG_Inc <<- TRUE
}

platFound__ = FALSE
init.platFound = function(){
  platFound__ <<- TRUE
}
'
# introduce plateaus in trajectory df
shoveRow = function(df, newr, r) {
  df[seq(r + 1, nrow(df) + 1), ] = df[seq(r, nrow(df)), ]
  df[r, ] = newr
  return(df)
}

# old: findPlat
insertPlateaus = function(solver_traj) { 
  foundPlat__ = FALSE
  l = length(solver_traj$iter) 
  i = 1L
  while(i < l){
    if(i == solver_traj[(i + 1), "iter"]) {
      i = i + 1
    } else {
      solver_traj = solver_traj %>% shoveRow(., .[i, ], r = i + 1)
      solver_traj[i + 1 , "iter"] = i
      foundPlat__ = TRUE
      attr(solver_traj,'plateaunized_called') <- TRUE
      attr(solver_traj,'plateaunized') <- TRUE
      l = l + 1
      i = i + 1
    }
  }
  if(!(isTRUE(foundPlat__))){
    #TODO: write exception handler / caller (FCM style) in call_bib.R
    attr(solver_traj,'plateaunized_called') <- TRUE 
    attr(solver_traj,'plateaunized') <- FALSE
    message("WARNING: \n NO PLATEAUS exhibited by trajectory!")
  }
  #init.plateaus()
  return(solver_traj)
}

# TODO: delete
'
getPlateauDF = function(solver_traj){
  if(init_plateaus == F){
    solver_traj = insertPlateaus(solver_traj)
    return(solver_traj)
  } else {
    return(solver_traj)
  }
}
'
# Idea: impute valid time stamps to Plateau values 
# (otherwise we would have the same timestamp multiple time biasing our statistics)
# old: time_corrector
imputeTimes = function(solver_traj){

  if(attr(solver_traj,'plateaunized') == T){
    tmp = plyr::ddply(solver_traj, .(incumbant), nrow)
    # all candidates with same time 
    plats = tmp[which(tmp$V1 > 1),]
    plats_positions = solver_traj[which(solver_traj$incumbant %in% plats$incumbant), 
                                  c("time.passed", "iter", "incumbant")]
    look_up_table = unique(plats_positions$incumbant)

    for(i in 1:length(look_up_table)){
	    # batch we need to derive numbers from
	    look_up_df = plats_positions[which(plats_positions$incumbant == look_up_table[i]), ] 
	    # numbers needed:
	    n = length(look_up_df$iter)
	    last_iter = look_up_df[n, "iter"] 
	    time_cache = look_up_df[1, "time.passed"]
	    # corrector +1 needed, since iter != df index :/
	    next.time.passed = solver_traj[(last_iter + 1) + 1, "time.passed"]   
	    time_diff = next.time.passed - time_cache # timespan of plateaus
	    time_fraction = time_diff/n

	    for(j in 1:(n-1)){
	    	solver_traj[look_up_df$iter[j]+2, "time.passed"] = time_cache + (j * time_fraction)
	    }
  }
    attr(solver_traj,'times_imputed_called') <- TRUE
    attr(solver_traj,'times_imputed') <- TRUE
  } else {
    attr(solver_traj,'times_imputed_called') <- TRUE
    attr(solver_traj,'times_imputed') <- FALSE
    message("WARNING: \n NO PLATEAUS exhibited by trajectory, so no time.passed imputable!")
  }
  return(solver_traj)
}

# only apply this to a copy of res_eax (i.e., res_eax_copy)
# TODO assert()  check "copy" attribute / plateau class class 
# old: impute_plateau
imputeLastPlateau = function(solver_traj){

  if(attr(solver_traj,'plateaunized_called') == T & 
     attr(solver_traj,'times_imputed_called') == T){
    
    iter_time_avg = solver_traj[length(solver_traj$iter), "time.passed"] / (length(solver_traj$iter)-1L) # -1 ???
    res_eax_cutoff_time = 5L
    timer_remainder = res_eax_cutoff_time - solver_traj[length(solver_traj$iter), "time.passed"]
    amnt_iter = (timer_remainder / iter_time_avg) %>% round(., 0)

    if(amnt_iter == 0  | amnt_iter < 0){
      return(solver_traj)
    } else {
      timels = list()
      for(i in 1:amnt_iter){
        timels[[i]] = solver_traj[length(solver_traj$iter), "time.passed"] + (iter_time_avg * i)
      }
      iterls = seq(from = solver_traj[length(solver_traj$iter), "iter"] + 1L,
                   to = (solver_traj[length(solver_traj$iter), "iter"] + amnt_iter))
      incls = rep(solver_traj[length(solver_traj$iter), "incumbant"], amnt_iter)
      avgls = rep(solver_traj[length(solver_traj$iter), "average.fitness"], amnt_iter)
      restartls = rep(0L, amnt_iter)

      attachable_df = data.frame(a = unlist(timels), 
                                 b = iterls, 
                                 c = incls, 
                                 d = avgls, 
                                 e = restartls)
      names(attachable_df) = names(solver_traj)

      solver_traj = rbind(solver_traj, attachable_df)
      attr(solver_traj,'plateaunized') <- TRUE
      return(solver_traj)
    }
  } else {
    message("Make sure insertPlateaus() and correctTimes() 
             are called before calling imputeLastPlateau() on that instance!")
  }
}

# dependcy to getFeatureSet (salesperson)
try.getFeatureSet = function(black.list, instance) {
  out = tryCatch(
    {
      getFeatureSet(instance, black.list = black.list) # change x
    },
    error=function(cond) {
      message(cond)
      return(0L)
    },
    warning=function(cond) {
      message(cond)
      return(0L)
    },
    finally={
      message("Could load getFeatureSet()")
    }
  )  
  if(is.integer(out)){
    sum_of_lowest_edge_values = out
  } else {
    sum_of_lowest_edge_values = out$sum_of_lowest_edge_values
  }
  return(sum_of_lowest_edge_values)
}

## helper function that tests whether an object is either NULL or list of NULLs
is.NullOb = function(x) {
  is.null(x) | all(sapply(x, is.null))
}
## Recursively step down into list, removing all such objects 
rmNullObs = function(x) {
  x = Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if(is.list(x)){
    rmNullObs(x)
    }  else {
      x
    }
  )
}






