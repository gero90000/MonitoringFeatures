# TODO:
# put roxygen2 comments here
# @author:Bjoern
# @Feature_6: "VIOLATION AREA RATIO"
# @description: 
#   IDEA_ID_6: VG in PLATEAU and vice versa (i.e., PLATEAUS in VG area).
#   the higher the ration the more unstable is the trajectory

# old: get_vio_plat
#' Title
#'
#' @param solver_traj 
#' @param VG_stats 
#' @param Plat_start_stats 
#' @param Plat_stats 
#'
#' @return
#' @export
#'
#' @examples
getVioPlat = function(solver_traj, VG_stats, Plat_start_stats, Plat_stats){
  resls = list()
  vg_last = NA
  vg = list()
  plat_pos_avg = list()
  # vg is only returned for plotting purpose

  if(attr(solver_traj,'plateaunized') == T & 
     attr(solver_traj,'vertical_gaps') == T){
    vg = VG_stats$improvements
    vg = as.data.frame(cbind(vg$improvement, seq(1:length(VG_stats$improvements$improvement))))
    vg = vg[which(vg$V1 >= VG_stats$vg_stats$vg_threshold), ]
    # only generated here and returned for plotting purpose 
    plat_pos_avg = Plat_start_stats$plat_start$plats_positions
    plateaus = Plat_start_stats$data[which(Plat_start_stats$data$V1 > 1), ]
    plateaus = res_eax$trajectory[which(res_eax$trajectory$incumbant %in% 
                                        plateaus$incumbant), c("iter", "incumbant")]
    vg_last = vg[length(vg$V1), "V2"]
    
    PLAT_cache = list()
    for(i in 1:length(plateaus$iter)){
      if(plateaus[i, "iter"] < vg_last){
        PLAT_cache[[i]] = plateaus[i, ]
      }
    }
    if(as.logical(length(PLAT_cache))){
      PLAT_cache = matrix(unlist(PLAT_cache), ncol = 2, by = 2) %>% as.data.frame(.)
      plat_violating = aggregate(PLAT_cache$V1, 
                                 list(PLAT_cache$V2), mean)
      
      vio_amnt_PLAT = length(plat_violating$Group.1)
      # @CALL: Plat_stats = calcPlateauStats(res_eax$trajectory, SuccessRatio)
      vio_ratio_PLAT = vio_amnt_PLAT / Plat_stats$plat_stats$Num_Plateau_Length
      # @CALL: VG_stats = get_VG_stats(res_eax$trajectory)
      vio_ratio_VG = vio_amnt_PLAT / VG_stats$vg_stats$Num_Vertical_Gaps
      vio_ratio_PLAT_VG = vio_amnt_PLAT / (Plat_stats$plat_stats$Num_Plateau_Length + 
                                           VG_stats$vg_stats$Num_Vertical_Gaps) 
    } else {
      vio_ratio_PLAT = 0L
      vio_ratio_VG = 0L
      vio_ratio_PLAT_VG = 0L
    }
  } else {
    message("WARNING: \n no violation stats computable since VG or PLATS are missing.")
    vio_ratio_PLAT = 0L
    vio_ratio_VG = 0L
    vio_ratio_PLAT_VG = 0L
  }
  resls = list.append(resls,
                      VG_boarder_X = vg_last,
                      vio_ratio_PLAT = vio_ratio_PLAT,
                      vio_ratio_VG = vio_ratio_VG,
                      vio_ratio_PLAT_VG = vio_ratio_PLAT_VG,
                      vg = vg, 
                      plat_pos_avg = plat_pos_avg) 
  return(resls)
}

# make_plateau_vio_plot
#' Title
#'
#' @param vio_stats 
#' @param solver_traj 
#' @param VGstats 
#'
#' @return
#' @export
#'
#' @examples
makePlateauVio_plot = function(vio_stats, solver_traj, VGstats){
  helper = vio_stats$vg
  C = vio_stats$plat_pos_avg
  
  helper2 = VGstats$improvements
  helper2 = as.data.frame(cbind(helper$improvement, seq(1:length(VGstats$improvements$improvement))))
  helper2 = helper[which(helper$V1 >= VGstats$vg_stats$vg_threshold), ]
  
  ggplot(data=solver_traj) +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "black") +
    ggtitle("Incumbent Trajectory") +
    geom_vline(xintercept = helper2$V2, linetype = "solid", color = "magenta", alpha = 0.99) +
    annotate("rect", xmin = 0 , 
             ymin = solver_traj[length(solver_traj$iter), "incumbant"], 
             xmax = helper[length(helper$V2), "V2"], 
             ymax = Inf, alpha = 0.3, fill = "green") +
    annotate("rect", xmin = helper[length(helper$V2), "V2"] , 
             ymin = solver_traj[length(solver_traj$iter), "incumbant"], 
             xmax = Inf, 
             ymax = Inf, alpha = 0.3, fill = "tomato") +
    geom_step(mapping=aes(x=iter, y=incumbant), color = "black") +
    geom_point(data = C, mapping = aes(x = x, y = Group.1), pch = 10, size = 10, color = "magenta", alpha = 0.9) +
    geom_vline(xintercept = vio_stats$VG_boarder_X) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white",
                                      size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.15, linetype = 'solid',
                                      colour = "white")
    )
}



