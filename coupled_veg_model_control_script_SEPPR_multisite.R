# --- Coupled vegetation model control script

# --- Load necessary R packages ----
# --- Define repository from which R packages will be downloaded.
options(repos=c(CRAN="http://lib.stat.cmu.edu/R/CRAN/"), error=traceback)

list.of.packages <- c("fields","lubridate","MASS","data.table","plyr","abind","tidyverse")

for (pack in list.of.packages) {
  if (!require(pack, character.only = TRUE)) {
    install.packages(pack)
    require(pack, character.only = TRUE)
  }
}

rm(list.of.packages, pack)

# Set the working directory #
# This should be the root folder - subfolders should be swg_setup and DayCent
root <- "D:" #D (laptop), H (PC)
setwd(paste0(root, "/CUBoulder_Student_Documents/Dissertation/PPR_Vegetation_Model/coupled_vegetation_model/"))

### Run setup ###
# output will be .Rdata file with all required variables
source("./swg_setup_SEPPR.R")

### Load data from setup ###
# this is unnecessary if sourcing above function for the first time
#load( paste0("./swg_setup/swg_setup_SEPPR.RData") )

### Run multisite seasonal setup ###
## choose whether or not to use seasonal covariates
sea_cov <- "with" #"with" or "without"
## NOTE: simulating using probabilities will only work using covariates

source( paste0("./swg_multisite_cond_seas_sim_setup_SEPPR.R") )

### Run conditional SWG for all sites ###
# choose to use observed data to drive simulations or probabilities to drive simulations
sim_type <- "probs" #obs or probs
if(sim_type == "obs") {
  source( paste0("./swg_multisite_cond_seas_sim_obs_SEPPR_func.R") )
} else if(sim_type == "probs") {
  source( paste0("./swg_multisite_cond_seas_sim_probs_SEPPR_func.R") )
}

# choose number of simulations and a range of years to sim (note, this is arbitrary since nothing depends on it)
## NOTE: choose a sim start year one prior to the desired year to begin - this is because
##  DayCent requires wth files to start on Jan 1 and the SWG generates years from Mar 1 to Feb 28/29
nsim <- 250; sim_st_yr <- 2016; sim_end_yr <- 2018
if(sim_type == "obs") {
  sim_output <- swg_multisite_obs_SEPPR(nsim, sim_st_yr, sim_end_yr)
} else if(sim_type == "probs") {
  # choose the type of prcp (normal, wet, dry) and temp (normal, warm, cold) season you'd like to simulate
  sea_sim_type_prcp <- "normal"; sea_sim_type_tmp <- "normal"
  sim_output <- swg_multisite_probs_SEPPR(nsim, sim_st_yr, sim_end_yr, sea_sim_type_prcp, sea_sim_type_tmp)
}

### Convert SWG output to DayCent input ###
# place each piece of output in its own variable
sim_tmin_all <- sim_output$sim_tmin; sim_tmax_all <- sim_output$sim_tmax; sim_prcp_all <- sim_output$sim_prcp
sim_dates <- sim_output$sim_dates

####################
##
## START HERE TO ONLY PERFORM DAYCENT RUNS FOR ANOTHER STATION ##
##
####################

# choose at which station to simulate vegetation
stn_sim <- 5 #number associates to row in stns
stn_nm <- tolower(strsplit(stns[stn_sim, ]$name, split = " ")[[1]][1]) #station name
if(stn_nm == "webster") { stn_nm <- "webster_city"}

# record path of directory for the current station
path <- paste0("./DayCent/", stn_nm, "/")

# place the swg output for the station into a variable
sim_tmax <- t(sim_tmax_all[ , , stn_sim]); sim_tmin <- t(sim_tmin_all[ , , stn_sim])
sim_prcp <- t(sim_prcp_all[ , , stn_sim])

source( paste0("./convert_swg_to_DayCent_format_func.R") )
sim_yr_range <- convert_swg_out_to_DayCent_in(path, stn_nm, sim_tmax, sim_tmin, sim_prcp, sim_dates)

### Run DayCent with SWG output ###
do_spin <- F; veg_type <- "cattail" #grass or cattail
source( paste0("./DayCent_multisite_func.R") )
#progress bar
pb1 <- txtProgressBar(min = 0, max = nsim, style = 3)
for(n in 1:nsim) {
  daycent_multisite(path, stn_nm, do_spin, veg_type, n)
  setTxtProgressBar(pb1, n)
}
