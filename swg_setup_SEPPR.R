##################################################
## Load the setup data
##################################################

# Station metadata
stns = read_csv('./swg_setup/data/chosen_stations.csv') %>% arrange(desc(name)) #arrange in reverse alphabetical order
# Longitude, latitude, and distance matrix
lon.lat = stns %>% dplyr::select("lon", "lat")
dist.mat <- rdist.earth( as.matrix(lon.lat), miles = F )
diag(dist.mat) <- 0

# Read in data
fyr <- 1929; lyr <- 2018 #choose first (fyr) and last (lyr) year of data
PP2 = read_csv( paste0("./swg_setup/data/all_stn_PRCP_", fyr, "-", lyr, "_ts.csv") )
MX2 = read_csv( paste0("./swg_setup/data/all_stn_TMAX_", fyr, "-", lyr, "_ts.csv") )
MN2 = read_csv( paste0("./swg_setup/data/all_stn_TMIN_", fyr, "-", lyr, "_ts.csv") )

# Define data sets without dates for processing
PP = PP2 %>% dplyr::select(-date)
MX = MX2 %>% dplyr::select(-date)
MN = MN2 %>% dplyr::select(-date)

# Define number of days (nt), number of stations (np)
nt = nrow(PP); np = ncol(PP)
## Year, month, day vectors
yr <- year(PP2$date); mo <- month(PP2$date); da <- day(PP2$date)
## Years of record
uyr <- unique(yr); nyr <- length(uyr)
firstyear <- uyr[1]; lastyear  <- uyr[length(uyr)]
## Define leap years
leapID = which(uyr %in% uyr[leap_year(uyr)])
yearID = rep(365, nyr); yearID[leapID] = 366

##################################################
## Create Covariates
##################################################

# cos(t) and sin(t)
ct <- st <- c()
for(i in 1:nyr) {
  ct <- c(ct, cos((2*pi*(1:yearID[i]))/yearID[i]))
  st <- c(st, sin((2*pi*(1:yearID[i]))/yearID[i]))
}
# OCC = occurrence (where recorded precipitation is at least equal to 0.1mm)
OCC <- (PP >= 0.1) + 0
# POCC = previous day's occurrence
POCC <- OCC 
POCC[1, ] <- NA
POCC[2:nrow(POCC), ] <- OCC[1:(nrow(OCC)-1), ]
# PPI holds only positive precipitation values 
PPI <- PP # PPI is precipitation intensity, so remove zeros
PPI[PPI < 0.1] <- NA
# PMN = previous day's minimum temperature
PMN <- MN
PMN[1, ] = NA
PMN[2:nrow(PMN), ] <- MN[1:(nrow(MN)-1), ]
# PMX = previous day's maximum temperature
PMX <- MX
PMX[1, ] = NA
PMX[2:nrow(PMX), ] <- MX[1:(nrow(MX)-1), ]
# Rt = linear trend from -1 to +1 to account for temperature trends
Rt = seq(from = -1, to = 1, length.out = nrow(OCC))

##################################################

# Save setup data to an RData file
save.image( paste0("./swg_setup/swg_setup_SEPPR.RData") )