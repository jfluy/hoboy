calc_HI <- function(clim, low, med, high) {
  
  avg_temp <- data.frame(avg_tmp = NA, date = clim$date, "heat index" = NA)
  
  for (i in 1:nrow(avg_temp)) {
    avg_temp$avg_tmp[i] = (clim$tmax[i]+clim$tmin[i])/2
  }
  
  # Now, let's assign heat index values for each of the average temperatures
  
  for (i in 1:nrow(avg_temp)) {
    avg_temp$heat.index[i] = ifelse(avg_temp$avg_tmp[i] < low, 0, avg_temp$heat.index[i]) 
    avg_temp$heat.index[i] = ifelse(avg_temp$avg_tmp[i] > low & avg_temp$avg_tmp[i] < med, 1, avg_temp$heat.index[i])
    avg_temp$heat.index[i] = ifelse(avg_temp$avg_tmp[i] > med & avg_temp$avg_tmp[i] < high, 2, avg_temp$heat.index[i])
    avg_temp$heat.index[i] = ifelse(avg_temp$avg_tmp[i] > high, 3, avg_temp$heat.index[i])
  }
  
  zero <- sum(avg_temp$heat.index==0)
  one <- sum(avg_temp$heat.index==1)
  two <- sum(avg_temp$heat.index==2)
  three <- sum(avg_temp$heat.index==3)
  
  heat_index <- c("0", "1","2", "3")
  count_days <- c(zero, one, two, three)
  
  HI_count <- data.frame("Heat Index" = heat_index, "Number of Days"=count_days)
  
  return(HI_count)
}