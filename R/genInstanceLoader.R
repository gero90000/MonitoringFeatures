# @author:Bjoern
# @Name "GENERATOR LOADER"
# @description: 
#   loads instances of defined amount for
#   RUE, NETGEN, and TSPLIB

################# generate RUE instances ###################################################
#' Title
#'
#' @param assembled 
#'
#' @return
#' @export
#'
#' @examples
load_rue_instances = function(assembled){
  tsp.rue.ls = list()
  for(i in 1:length(assembled)){
    name = paste("tsp.rue.", i, sep = "")
    tsp.rue = salesperson::generateRandomNetwork(n.points = assembled[i])  
    tsp.rue.ls[[name]] = tsp.rue
  }
  return(tsp.rue.ls)
}

################# generate NETGEN instances ################################################
#' Title
#'
#' @param assembled 
#' @param n.dim 
#' @param n.cluster 
#'
#' @return
#' @export
#'
#' @examples
load_netgen.cluster_instances = function(assembled, n.dim, n.cluster){
  tsp.netgen.cluster.ls = list()
  for(i in 1:length(assembled)){
    #opt1 = runif(1L, 1L, 15L) %>% round(., 0)
    name = paste("tsp.netgen.cluster.", i, sep = "")
    tsp.netgen.cluster = netgen::generateClusteredNetwork(n.cluster = n.cluster, #|| opt1 
                                               n.points = assembled[i], 
                                               n.dim  = n.dim) 
    tsp.netgen.cluster.ls[[name]] = tsp.netgen.cluster
  }
  return(tsp.netgen.cluster.ls)
}

#' Title
#'
#' @param assembled 
#' @param n.dim 
#'
#' @return
#' @export
#'
#' @examples
load_netgen.grid_instances = function(assembled, n.dim){
  tsp.netgen.grid.ls = list()
  for(i in 1:length(assembled)){
    amnt.dim = sqrt(assembled[i]) %>% round(., 0L)
    name = paste("tsp.netgen.grid.", i, sep = "")
    tsp.netgen.grid = netgen::generateGridNetwork(n.points.per.dim = amnt.dim, 
                                                  n.dim = n.dim)
    tsp.netgen.grid.ls[[name]] = tsp.netgen.grid
  }
  return(tsp.netgen.grid.ls)
}

################# generate TSPLIB instances ################################################
#' Title
#'
#' @param path_to_testdata 
#' @param path_to_salesperson 
#'
#' @return
#' @export
#'
#' @examples
load_tsplib_instances = function(path_to_testdata, path_to_salesperson){
  curwd = getwd()
  setwd(path_to_salesperson)
  tsp.lib.ls = list()
  tsplib_instance.names = list.files(path_to_testdata, 
                                     pattern="*.tsp$", full.names=TRUE)
  for(i in 1:length(tsplib_instance.names)){
    tsplib.instance = sub(".*testdata/", "", tsplib_instance.names[i])
    print(tsplib.instance)
    import.statement = paste("inst/testdata/", tsplib.instance, sep = "")
    tsp.tsplib = importFromTSPlibFormat(import.statement, round.distances = TRUE) 
    name = paste("tsp.tsplib.", tsplib.instance, sep = "")
    tsp.lib.ls[[name]] = tsp.tsplib
  }
  setwd(curwd)
  return(tsp.lib.ls)
}

################# __MAIN__()  ##############################################################
#' Title
#'
#' @param assembled 
#' @param n.dim 
#' @param n.cluster 
#' @param path_to_testdata 
#' @param path_to_salesperson 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
make_generator_instances = function(assembled, n.dim, n.cluster, path_to_testdata,
                                    path_to_salesperson, ...){
  generator_instances = list()
  tsp.rue.ls = load_rue_instances(assembled)
  tsp.netgen.cluster.ls = load_netgen.cluster_instances(assembled, n.dim, n.cluster) 
  tsp.netgen.grid.ls = load_netgen.grid_instances(assembled, n.dim)
  tsp.lib.ls = load_tsplib_instances(path_to_testdata, path_to_salesperson)
  generator_instances = list.append(generator_instances,
                                    tsp.rue = tsp.rue.ls, 
                                    tsp.netgen.cluster = tsp.netgen.cluster.ls,
                                    tsp.netgen.grid = tsp.netgen.grid.ls,
                                    tsp.lib= tsp.lib.ls)
  return(generator_instances)
}

