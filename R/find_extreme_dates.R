#' find_extreme_dates


## Find which days of the year have the most extreme climate values
## This only works if the the date column is called "date"!

find_extreme_dates <- function(clim, min_xtreme = FALSE) {
  
  # Change data format so date is the rowname
  col_num <- which(colnames(clim)=="date")
  clim_tmp <- clim[,-col_num]
  rownames(clim_tmp) <- clim[,col_num]
  
  
  # Create temp data frame for maximums
  clim_var_names <- colnames(clim_tmp)
  
  tmp_data <- data.frame(clim_var = clim_var_names, max = NA, day = NA)
  
  for (i in 1:length(clim_tmp)) {
    tmp_data$max[i] = max(clim_tmp[i])
  }
  
  xtreme_dates <- rownames(clim_tmp)[apply(clim_tmp,2,which.max)]
  
  tmp_data$day <- xtreme_dates
  
  tmp_data2 <- data.frame("Climate Variable" = clim_var_names, "Maximum Value" = tmp_data$max, Date = tmp_data$day)
  
  if(min_xtreme) {
    
    min_data <- data.frame(clim_var = clim_var_names, min = NA, day = NA)
    
    for (i in 1:length(clim_tmp)) {
      min_data$min[i] = min(clim_tmp[i])
    }
    
    xtreme_min_dates <- rownames(clim_tmp)[apply(clim_tmp,2,which.min)]
    
    min_data$day <- xtreme_dates
    
    min_data2 <- data.frame("Climate Variable" = clim_var_names, "Minimum Value" = min_data$min, Date = min_data$day)
    
    return(Minimums = min_data2)
  }
  
  else(return(Maximums = tmp_data2))
  
}
