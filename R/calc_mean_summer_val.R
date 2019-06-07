#' calc_mean_summer_val
#'
#'
#'
#'




## Mean summer precipitation
## Summer calendar days of the year: 151 to 241

library(stringr)

calc_mean_summer_val <- function(clim, var_name) {
  
  clim2 <- data.frame(Variable = var_name, date = clim$date)
  clim2$date <- as.character(clim2$date)
  
  jun <- clim2 %>% filter(str_detect(date, "-06-"))
  jul <- clim2 %>% filter(str_detect(date, "-07-"))
  aug <- clim2 %>% filter(str_detect(date, "-08-"))
  
  summer_months <- rbind(jun, jul, aug)
  
  sum_avg_val <- mean(summer_months$Variable)
  
  return(sum_avg_val)
}

