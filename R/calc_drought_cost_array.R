#' calc_drought_cost_array
#'
#'
#'
#'
#'


calc_drought_cost_array <- function(drought_array, clim) {
  
  
  list_dim <- dim(drought_array)
  location_num <- list_dim[1]
  veg_num <- list_dim[2]
  pollution <- list_dim[3]
  
  drought_thresh2 = array(dim=c(location_num,veg_num, pollution))    
  
  source("R/calc_drought_cost.R")
  for (i in 1:location_num) {
    for (j in 1:veg_num) {
      for (k in 1:pollution) {
        value = calc_drought_cost(clim, clim$aet, clim$pet, drought_thresh[i,j,k], 50)
        drought_thresh2[i,j,k]=value
      }
    }
  }
  
  dimnames(drought_thresh2) = dimnames(drought_array)
  
  return(drought_thresh2)
  
}