# --- Integrated Climate-Ecological Modeling Framework (ICEMF) control script

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
root <- "<drive letter>:" #The letter of the drive where all the ICEMF files and scripts are located
setwd(paste0(root, "/<file path>/")) #The file path to the directory where all the ICEMF files and scripts (including this control script) are located

### Run stochastic weather generator (SWG) setup ###
# Output will be an .Rdata file with all required variables
source("./swg_setup_SEPPR.R")

### Load data from setup ###
# This is unnecessary if sourcing above function for the first time
#load( paste0("./swg_setup/swg_setup_SEPPR.RData") )

### Run multisite seasonal setup ###
## Choose whether or not to use seasonal covariates
sea_cov <- "with" #"with" or "without"
## NOTE: simulating using probabilities will only work using covariates

source( paste0("./swg_multisite_cond_seas_sim_setup_SEPPR.R") )

### Run conditional SWG for all sites ###
# Choose to use observed (obs) data to drive simulations or probabilities (probs) to drive simulations
sim_type <- "probs" #obs or probs
if(sim_type == "obs") {
  source( paste0("./swg_multisite_cond_seas_sim_obs_SEPPR_func.R") )
} else if(sim_type == "probs") {
  source( paste0("./swg_multisite_cond_seas_sim_probs_SEPPR_func.R") )
}

# Choose number of simulations (nsim) and a range of years to sim (sim_st_yr and sim_end_yr)
## NOTE: choose a sim start year one prior to the desired year to begin - this is because
##  DayCent requires weather (.wth) files to start on Jan 1 and the SWG generates years from Mar 1 to Feb 28/29
nsim <- <number of simulations>; sim_st_yr <- <start year>; sim_end_yr <- <end year>
if(sim_type == "obs") {
  sim_output <- swg_multisite_obs_SEPPR(nsim, sim_st_yr, sim_end_yr)
} else if(sim_type == "probs") {
  # Choose the type of precipitation (prcp - normal, wet, dry) and temperature (tmp - normal, warm, cold) season you'd like to simulate
  sea_sim_type_prcp <- "normal"; sea_sim_type_tmp <- "normal"
  sim_output <- swg_multisite_probs_SEPPR(nsim, sim_st_yr, sim_end_yr, sea_sim_type_prcp, sea_sim_type_tmp)
}

### Convert SWG output to DayCent input ###
# Place each piece of SWG output in its own variable
sim_tmin_all <- sim_output$sim_tmin; sim_tmax_all <- sim_output$sim_tmax; sim_prcp_all <- sim_output$sim_prcp
sim_dates <- sim_output$sim_dates

####################
##
## START HERE TO PERFORM DAYCENT RUNS FOR ANOTHER STATION ##
##
####################

# Choose at which station to simulate vegetation
stn_sim <- 1 #number associates to row in stns
stn_nm <- tolower(strsplit(stns[stn_sim, ]$name, split = " ")[[1]][1]) #station name
if(stn_nm == "webster") { stn_nm <- "webster_city"}

# Set file path for the current station
path <- paste0("./DayCent/", stn_nm, "/")

# Place the SWG output for the station into a variable
sim_tmax <- t(sim_tmax_all[ , , stn_sim]); sim_tmin <- t(sim_tmin_all[ , , stn_sim])
sim_prcp <- t(sim_prcp_all[ , , stn_sim])

# Convert the SWG output into acceptable DayCent format and write files
source( paste0("./convert_swg_to_DayCent_format_func.R") )
sim_yr_range <- convert_swg_out_to_DayCent_in(path, stn_nm, sim_tmax, sim_tmin, sim_prcp, sim_dates)

### Run DayCent with SWG output ###
# Choose whether to do model spinup and the vegetation type to simulate
# NOTE: Spinup for each station has been provided
do_spin <- F; veg_type <- "grass" #grass or cattail
source( paste0("./DayCent_multisite_func.R") )
# Track progress of the model using progress bar
pb1 <- txtProgressBar(min = 0, max = nsim, style = 3)
# Simulate using each realization of weather from SWG
for(n in 1:nsim) {
  daycent_multisite(path, stn_nm, do_spin, veg_type, n)
  setTxtProgressBar(pb1, n)
}
