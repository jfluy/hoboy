calc_drought_cost <- function(clim, aet, pet, drought_threshold, cost_per_drought_day) {
  
  cwd_clim <- data.frame(aet = aet, pet = pet, cwd = NA, drought_stress = NA, avg_cwd = NA)
  
  for (i in 1:nrow(clim)) {
    cwd_clim$cwd[i] = cwd_clim$aet[i]-cwd_clim$pet[i]
  }
  
  # Calculate average CWD over a period of 7 days 
  last_row = nrow(cwd_clim) - 7
  
  # If average is under Xmm, then the species will likely dessicate
  
  for ( i in 7:last_row) {
    cwd_clim$avg_cwd[i] = mean(cwd_clim$cwd[i-6],cwd_clim$cwd[i-5],cwd_clim$cwd[i-4],cwd_clim$cwd[i-3], cwd_clim$cwd[i-2],cwd_clim$cwd[i-1],cwd_clim$cwd[i], cwd_clim$cwd[i+6],cwd_clim$cwd[i+5],cwd_clim$cwd[i+4],cwd_clim$cwd[i+3], cwd_clim$cwd[i+2],cwd_clim$cwd[i+1])
    
    cwd_clim$drought_stress[i] = ifelse(cwd_clim$avg_cwd[i] > drought_threshold, 1, 0)
  }
  
  
  # Average cwd DURING days that ARE droughts. Not Average CWD for the year.
  av_drought <- cwd_clim %>% 
    filter(drought_stress == 1)
  
  one <- nrow(av_drought)
  
  av_drought2 <- mean(av_drought$avg_cwd)
  
  av_cwd <- cwd_clim %>% filter(!is.na(drought_stress))
  
  av_cwd <- mean(av_cwd$avg_cwd)
  
  cost <- one*(cost_per_drought_day)
  
  return(cost)
  
}