swg_multisite_probs_SEPPR <- function(nsim, sim_st_yr, sim_end_yr, sea_sim_type_prcp, sea_sim_type_tmp) {
  ##########################################
  ##
  ## Simulations
  ##
  ##########################################
  # simulation metadata
  x_coords = lon.lat[ , 1]; y_coords = lon.lat[ , 2]
  n.x_coords <- length(x_coords); n.y_coords <- length(y_coords)
  # elevation at each station 
  elev <- stns[ , "elev"]
  
  #progress bar
  pb1 <- txtProgressBar(min = 0, max = length(sim_st_yr:sim_end_yr), style = 3)
  prog <- 1
  
  SIMamt_all <- SIMocc_all <- SIMmax_all <- SIMmin_all <- sim.dates_all <- vector()
  for(yr.to.sim in sim_st_yr:sim_end_yr) {
    SIMamt_year <- SIMocc_year <- SIMmax_year <- SIMmin_year <- sim.dates_year <- vector()
    for(sea_sim_num in 1:4) {
      sea_sim <- seasons[[sea_sim_num]]
      
      dates_temp <- seq(as.Date(paste0(yr.to.sim, "-", sea_sim[1], "-01")), length.out = 3, by = "month")
      sea.length <- sum(days_in_month(dates_temp))
      yr.sim <- c(rep(year(dates_temp[1]), days_in_month(dates_temp[1])), rep(year(dates_temp[2]), days_in_month(dates_temp[2])),
                  rep(year(dates_temp[3]), days_in_month(dates_temp[3])) )
      mo.sim <- c(rep(sea_sim[1], days_in_month(dates_temp[1])), rep(sea_sim[2], days_in_month(dates_temp[2])),
                  rep(sea_sim[3], days_in_month(dates_temp[3])))
      da.sim <- c(1:days_in_month(dates_temp[1]), 1:days_in_month(dates_temp[2]), 1:days_in_month(dates_temp[3]))
      if(2 %in% sea_sim && leap_year(year(dates_temp[which(2 == month(dates_temp))]))) { #checks for February
        st_ind <- sum(yearID[1:2]) + 1 # this is to move to a starting index of a leap year
        ct.sim <- ct[ (yday(dates_temp[1]) + st_ind) : ( (yday(dates_temp[1]) + st_ind) + sea.length - 1) ]
        st.sim <- st[ (yday(dates_temp[1]) + st_ind) : ( (yday(dates_temp[1]) + st_ind) + sea.length - 1) ]
      } else {
        ct.sim <- ct[ yday(dates_temp[1]) : (yday(dates_temp[1]) + sea.length - 1) ]
        st.sim <- st[ yday(dates_temp[1]) : (yday(dates_temp[1]) + sea.length - 1) ]
      }
      ## to neglect temperature trends, and be centered on the mean of your historical data
      ## temperatures, set Rt.sim = 0 for all days
      Rt.sim <- rep(0, length(yr.sim))
      #Rt.sim <- rep(1,length(yr.sim)) # if you want simulated temperatures to be high
      #Rt.sim <- rep(-1,length(yr.sim)) # if you want simulated temperatures to be low
      
      # number of days to simulate
      nt.sim <- length(Rt.sim)
      ## years to simulate (just for convenience, they are in fact arbitrary years)
      uyr.sim <- unique(yr.sim)
      ## number of years to simulate (also arbitrary)
      nyr.sim <- length(uyr.sim)
      
      # simulation time
      sim.start.date <- as.Date(paste(yr.sim[1], "-", mo.sim[1], "-", da.sim[1], sep = ""))   # Beginning of simulated series
      sim.end.date <- as.Date(paste(yr.sim[length(yr.sim)], "-", mo.sim[length(mo.sim)], "-", da.sim[length(da.sim)], sep = ""))     # End of simulated series
      sim.dates <- seq(from = sim.start.date, to = sim.end.date, by = "days")
      
      # number of realizations 
      RNum <- 1:nsim; n.realizations <- length(RNum)
      # id for missing data
      mv <- -9999
      
      # bootstrapping season (i.e., ST4, SMN4, SMX4) values to condition output
      # FORECAST FOR PRECIPITATION (B, N, A): 30:40:30  (checked 8.10.2020)
      # FORECAST FOR TEMPERATURE (B, N, A):   17:33:50  (checked 8.10.2020)
      # see IRI seasonal forecasts
      
      # sample nsim different season precip totals with IRI probability (B, N, A) = (0.30, 0.40, 0.30)
      if(sea_sim_type_prcp == "normal") { prcp_probs <- c(0.33, 0.34, 0.33)
      } else if(sea_sim_type_prcp == "wet") { prcp_probs <- c(0.25, 0.35, 0.40) #(0.20, 0.30, 0.50)
      } else if(sea_sim_type_prcp == "dry") { prcp_probs <- c(0.40, 0.35, 0.25) }
      
      st.samp = sample(1:3, nsim, prob = prcp_probs, replace = T)
      
      #breaks seatot values into three subsets to sample from - broken by 33.33 and 66.67 quantile seatot values
      st.levels = list()
      st.levels[[1]] = seatot[ , sea_sim_num][seatot[ , sea_sim_num] <= quantile(seatot[ , sea_sim_num], (1/3))]
      st.levels[[2]] = seatot[ , sea_sim_num][seatot[ , sea_sim_num] <= quantile(seatot[ , sea_sim_num], (2/3)) & seatot[ , sea_sim_num] > quantile(seatot[ , sea_sim_num], (1/3))]
      st.levels[[3]] = seatot[ , sea_sim_num][seatot[ , sea_sim_num] >  quantile(seatot[ , sea_sim_num], (2/3))]
      
      # we are only simulating one season, so other ST variables = rep(0, season.length) ... where season.length differs between them
      ST1.sim = ST2.sim = ST3.sim = ST4.sim = matrix(0, nrow = nsim, ncol = length(ct.sim))
      if(sea_sim_num == 1) {
        for(k in 1:nsim) ST1.sim[k, ] = rep(sample(st.levels[[st.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 2) {
        for(k in 1:nsim) ST2.sim[k, ] = rep(sample(st.levels[[st.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 3) {
        for(k in 1:nsim) ST3.sim[k, ] = rep(sample(st.levels[[st.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 4) {
        for(k in 1:nsim) ST4.sim[k, ] = rep(sample(st.levels[[st.samp[k]]], 1), length(ct.sim))
      }
      
      # sample nsim different season max and min temp totals with IRI probability (B, N, A) = (0.17, 0.33, 0.50)
      if(sea_sim_type_tmp == "normal") { tmp_probs <- c(0.33, 0.34, 0.33)
      } else if(sea_sim_type_tmp == "warm") { tmp_probs <- c(0.20, 0.30, 0.50)
      } else if(sea_sim_type_tmp == "cold") { tmp_probs <- c(0.50, 0.30, 0.20) }
      
      tmp.samp = sample(1:3, nsim, prob = tmp_probs, replace = T)
      mx.levels = list()
      mx.levels[[1]] = seamax[ , sea_sim_num][seamax[ , sea_sim_num] <= quantile(seamax[ , sea_sim_num], (1/3))]
      mx.levels[[2]] = seamax[ , sea_sim_num][seamax[ , sea_sim_num] <= quantile(seamax[ , sea_sim_num], (2/3)) & seamax[ , sea_sim_num] > quantile(seamax[ , sea_sim_num], (1/3))]
      mx.levels[[3]] = seamax[ , sea_sim_num][seamax[ , sea_sim_num] >  quantile(seamax[ , sea_sim_num], (2/3))]
      
      # we are only simulating one season, so other SMX variables = rep(0, season.length) ... where season.length differs between them
      SMX1.sim = SMX2.sim = SMX3.sim = SMX4.sim = matrix(0, nrow = nsim, ncol = length(ct.sim))
      if(sea_sim_num == 1) {
        for(k in 1:nsim) SMX1.sim[k, ] = rep(sample(mx.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 2) {
        for(k in 1:nsim) SMX2.sim[k, ] = rep(sample(mx.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 3) {
        for(k in 1:nsim) SMX3.sim[k, ] = rep(sample(mx.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 4) {
        for(k in 1:nsim) SMX4.sim[k, ] = rep(sample(mx.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } 
      
      mn.levels = list()
      mn.levels[[1]] = seamin[ , sea_sim_num][seamin[ , sea_sim_num] <= quantile(seamin[ , sea_sim_num], (1/3))]
      mn.levels[[2]] = seamin[ , sea_sim_num][seamin[ , sea_sim_num] <= quantile(seamin[ , sea_sim_num], (2/3)) & seamin[ , sea_sim_num] > quantile(seamin[ , sea_sim_num], (1/3))]
      mn.levels[[3]] = seamin[ , sea_sim_num][seamin[ , sea_sim_num] >  quantile(seamin[ , sea_sim_num], (2/3))]
      
      # we are only simulating one season, so other SMN variables = rep(0, season.length) ... where season.length differs between them
      SMN1.sim = SMN2.sim = SMN3.sim = SMN4.sim = matrix(0, nrow = nsim, ncol = length(ct.sim))
      if(sea_sim_num == 1) {
        for(k in 1:nsim) SMN1.sim[k, ] = rep(sample(mn.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 2) {
        for(k in 1:nsim) SMN2.sim[k, ] = rep(sample(mn.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 3) {
        for(k in 1:nsim) SMN3.sim[k, ] = rep(sample(mn.levels[[tmp.samp[k]]], 1), length(ct.sim))
      } else if(sea_sim_num == 4) {
        for(k in 1:nsim) SMN4.sim[k, ] = rep(sample(mn.levels[[tmp.samp[k]]], 1), length(ct.sim))
      }
      
      # define matrices for simulated series
      SIMocc <- SIMamt <- SIMmax <- SIMmin <- array(dim = c(nsim, nt.sim, np)) # simulations in array of (# days) x (# stations) x (# trajs)
      
      ## GAMMA MODEL -- OBTAIN SHAPE AND SCALE
      SC <- matrix(NA, nrow = nt.sim, ncol = np); SH <- numeric(np)
      
      # can't have max temp less than min temp
      #min.diff <- min(MX-MN,na.rm=T)
      min.diff <- quantile(MX-MN, probs = 1/10000, na.rm = T)
      SIMocc.old.2 <- (mvrnorm(n = 1, mu = rep(0, np), Sigma = PRCPcor[[mo[nt.sim]]]) > 0) + 0
      
      for(i in 1:nsim) {
        # this loop is redefining the Gamma parameters for each sample
        for(k in 1:np) {
          SH[k] <- gamma.shape(GAMMA[[k]])$alpha
          SC[ , k] <- exp(apply(coefamt[ , k]*rbind(1, ct.sim, st.sim, ST1.sim[i , ], ST2.sim[i , ], ST3.sim[i , ], ST4.sim[i , ]), FUN = sum, MAR = 2, na.rm = T))/SH[k]
        }
        SIMocc.old <- SIMocc.old.2
        SIMmax.old <- apply(MX[mo==12 & da==31, ], 2, mean, na.rm = T)
        SIMmin.old <- apply(MN[mo==12 & da==31, ], 2, mean, na.rm = T)
        for(d in 1:nt.sim) { 
          # amounts are transformed from mvrnorm with PRCPcor as covariance matrix 
          SIMamt[i, d, ] <- qgamma(pnorm(mvrnorm(n = 1, mu = rep(0, np), Sigma = PRCPcor[[mo.sim[d]]])), shape = SH, scale = SC[d, ])
          # occurrence is mean function + mvrnorm with PRCPcor as covariance matrix (coerced to 0 or 1)
          # covariates for mean function are POCC, cos[d], sin[d]
          SIMocc[i, d, ] <- (apply(rbind(1, SIMocc.old, ct.sim[d], st.sim[d], ST1.sim[i, d], ST2.sim[i, d], ST3.sim[i, d], ST4.sim[i, d])*coefocc, 2, sum) + mvrnorm(1, mu = rep(0, np), Sigma = PRCPcor[[mo.sim[d]]]) > 0) + 0
          # max is mean function + mvrnorm with TEMPcov as covariance matrix
          # covariates for mean function are PMN, PMX, ct, st, OCC, Rt
          SIMmax[i, d, ] <- apply(rbind(1, SIMmin.old, SIMmax.old, ct.sim[d], st.sim[d], SIMocc[i, d, ], Rt.sim[d], SMN1.sim[i, d], SMN2.sim[i, d], SMN3.sim[i, d], SMN4.sim[i, d], SMX1.sim[i, d], SMX2.sim[i, d], SMX3.sim[i, d], SMX4.sim[i, d])*coefmax, 2, sum) + mvrnorm(1, mu = rep(0, np), Sigma = TEMPcov[[mo.sim[d]]])
          # min is mean function + mvrnorm with TEMPcov as covariance matrix (different mvrnorm than max)
          # covariates for mean function are PMN, PMX, ct, st, OCC, Rt
          SIMmin[i, d, ] <- apply(rbind(1, SIMmin.old, SIMmax.old, ct.sim[d], st.sim[d], SIMocc[i, d, ], Rt.sim[d], SMN1.sim[i, d], SMN2.sim[i, d], SMN3.sim[i, d], SMN4.sim[i, d], SMX1.sim[i, d], SMX2.sim[i, d], SMX3.sim[i, d], SMX4.sim[i, d])*coefmin, 2, sum) + mvrnorm(1, mu = rep(0, np), Sigma = TEMPcov[[mo.sim[d]]])
          
          # can't have max temp less than min temp
          #while(min(SIMmax[i, d, ] - SIMmin[i, d, ]) < min.diff) {
          while( sum(SIMmax[i, d, ] < SIMmin[i, d, ]) > 0 ) {
            SIMmin[i, d, ] <- apply(rbind(1, SIMmin.old, SIMmax.old, ct.sim[d], st.sim[d], SIMocc[i, d, ], Rt.sim[d], SMN1.sim[i, d], SMN2.sim[i, d], SMN3.sim[i, d], SMN4.sim[i, d], SMX1.sim[i, d], SMX2.sim[i, d], SMX3.sim[i, d], SMX4.sim[i, d])*coefmin, 2, sum) + mvrnorm(1, mu = rep(0, np), Sigma = TEMPcov[[mo.sim[d]]])
            SIMmax[i, d, ] <- apply(rbind(1, SIMmin.old, SIMmax.old, ct.sim[d], st.sim[d], SIMocc[i, d, ], Rt.sim[d], SMN1.sim[i, d], SMN2.sim[i, d], SMN3.sim[i, d], SMN4.sim[i, d], SMX1.sim[i, d], SMX2.sim[i, d], SMX3.sim[i, d], SMX4.sim[i, d])*coefmax, 2, sum) + mvrnorm(1, mu = rep(0, np), Sigma = TEMPcov[[mo.sim[d]]])
          }
          SIMocc.old <- SIMocc[i, d, ]
          SIMmax.old <- SIMmax[i, d, ]
          SIMmin.old <- SIMmin[i, d, ]
        }
      }
      
      SIMamt[SIMamt < 0.1] = 0
      SIMocc[SIMamt ==  0] = 0
      SIMamt[SIMocc ==  0] = 0
      
      if(sea_sim_num == 1) {
        SIMamt_year <- SIMamt
        SIMocc_year <- SIMocc
        SIMmax_year <- SIMmax
        SIMmin_year <- SIMmin
        sim.dates_year <- sim.dates
      } else {
        SIMamt_year <- abind(SIMamt_year, SIMamt, along = 2)
        SIMocc_year <- abind(SIMocc_year, SIMocc, along = 2)
        SIMmax_year <- abind(SIMmax_year, SIMmax, along = 2)
        SIMmin_year <- abind(SIMmin_year, SIMmin, along = 2)
        sim.dates_year <- append(sim.dates_year, sim.dates)
      }
    }
    if(yr.to.sim == sim_st_yr) {
      SIMamt_all <- SIMamt_year
      SIMocc_all <- SIMocc_year
      SIMmax_all <- SIMmax_year
      SIMmin_all <- SIMmin_year
      sim.dates_all <- sim.dates_year
    } else {
      SIMamt_all <- abind(SIMamt_all, SIMamt_year, along = 2)
      SIMocc_all <- abind(SIMocc_all, SIMocc_year, along = 2)
      SIMmax_all <- abind(SIMmax_all, SIMmax_year, along = 2)
      SIMmin_all <- abind(SIMmin_all, SIMmin_year, along = 2)
      sim.dates_all <- append(sim.dates_all, sim.dates_year)
    }
    setTxtProgressBar(pb1, prog)
    prog <- prog+1
  }
  sim_output <- list(
    "sim_prcp" = SIMamt_all,
    "sim_occ" = SIMocc_all,
    "sim_tmax" = SIMmax_all,
    "sim_tmin" = SIMmin_all,
    "sim_dates" = sim.dates_all
  )
  return(sim_output)
}
