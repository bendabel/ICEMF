convert_swg_out_to_DayCent_in <- function(path, stn_nm, sim_tmax, sim_tmin, sim_prcp, sim_dates) {
  #DayCent columns are: Day, Month, Year, Day of Year, TMAX (degC), TMIN (degC), PRCP (cm)
  #create individual day, month, year, and day of year columns
  {
    sim_days <- day(sim_dates)
    sim_mons <- month(sim_dates)
    sim_yrs <- year(sim_dates)
    sim_doy <- yday(sim_dates)
  }
  
  #create directory in which to write all files
  path_in <- paste0(path, "input/")
  dir.create(path_in, recursive = T, showWarnings = F)

  #####
  ###Use each realization to create a wth file for DayCent
  #####
  
  # SWG outputs in units of input data - in this case degC and mm
  # convert mm to cm by dividing by 10 for use with DayCent
  for(i in 1:(ncol(sim_tmax)) ) {
    sim_tmax_temp <- round( sim_tmax[ , i], 2 )
    sim_tmin_temp <- round( sim_tmin[ , i], 2 )
    sim_prcp_temp <- round( sim_prcp[ , i] / 10, 2 )
    
    #Combine into one data frame
    station_data_daycent_format <- tibble(
      day = sim_days,
      month = sim_mons,
      year = sim_yrs,
      day_of_year = sim_doy,
      tmax_C = sim_tmax_temp,
      tmin_C = sim_tmin_temp,
      prcp_cm = sim_prcp_temp
    )
    
    #DayCent requires that wth files start at day 1 (Jan 1)
    # need to remove all rows up to that point
    while(station_data_daycent_format$day_of_year[1] != 1) {
      station_data_daycent_format <- station_data_daycent_format[-1, ]
    }
    #DayCent also requires that wth files end at the last day of the year
    # need to remove all rows at the end
    while(!(station_data_daycent_format$month[nrow(station_data_daycent_format)] == 12 &&
          station_data_daycent_format$day[nrow(station_data_daycent_format)] == 31)) {
      station_data_daycent_format <- station_data_daycent_format[-nrow(station_data_daycent_format), ]
    }
    
    write_tsv(station_data_daycent_format, paste0(path_in, "swg_out_", i, ".wth"), col_names = F)
  }
  
  sim_st_yr <- station_data_daycent_format$year[1]
  sim_end_yr <- station_data_daycent_format$year[nrow(station_data_daycent_format)]
  sim_yr_range <- list(st_yr = sim_st_yr, end_yr = sim_end_yr)
  return(sim_yr_range)
}