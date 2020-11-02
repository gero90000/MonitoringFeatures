#' Title
#'
#' @param solver_traj 
#' @param plot 
#'
#' @return
#' @export
#'
#' @examples
getQuadraticModelCoeff = function(solver_traj, plot = FALSE){
  resls = list()
  p = NA

  model_linear = lm(solver_traj$average.fitness ~ solver_traj$iter)
  summ_coeff_linear = summary(model_linear)
  model_poly = tryCatch(
    {
      lm(solver_traj$average.fitness ~ poly(solver_traj$iter, 2))
    },
    error=function(cond) {
      message(cond)
      return(NA)
    })
  summ_coeff_poly = tryCatch(
    { 
      summary(model_poly)
    },
    error=function(cond) {
      message(cond)
      return(NA)
    })
  if(isTRUE(plot)){
    p = tryCatch(
    {
      ggplot(solver_traj, aes(iter, incumbant)) + 
        geom_point(mapping=aes(x=iter, y=incumbant), color = "black") +
        geom_smooth(method='lm', formula = y~poly(x, 2), color = "tomato") +
        theme(
          panel.background = element_rect(fill = "white", colour = "white",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.15, linetype = 'solid',
                                          colour = "grey")
        )
    },
    error=function(cond) {
      message(cond)
      return(NA)
    })
  }
  resls = list.append(resls,
                      summ_coeff_linear = summ_coeff_linear,
                      summ_coeff_poly = summ_coeff_poly,
                      plot = p)
  return(resls)
}






