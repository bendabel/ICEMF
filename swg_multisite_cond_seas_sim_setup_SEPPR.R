##################################################
## Simulation Setup
##################################################

## Compute regional average seasonal total precipitation to use as predictor
montot = array(dim = c(length(uyr), 12, np))
for(i in 1:np) {
  tmp = as.data.table(cbind(yr, mo, da, PP[ , i]))
  names(tmp)[4] <- "V4" #rename column to work with below function
  for(j in 1:length(unique(mo))) {
    tmp1 = tmp[mo == j]
    montot[ , j, i] = as.vector(t(ddply(tmp1, .(yr), summarise, total = sum(V4, na.rm = T), na.rm = T)[2]))
  }
}

# Create variables to define seasons with month numbers
sea1 <- 3:5; sea2 <- 6:8; sea3 <- 9:11; sea4 <- c(12, 1, 2)
seasons <- list(sea1, sea2, sea3, sea4)

allseatot = array(dim = c(length(uyr), 4, np))
for(j in 1:np) {
  for(i in 1:length(uyr)) {
    allseatot[i, 1, j] = sum(montot[i, sea1, j])
    allseatot[i, 2, j] = sum(montot[i, sea2, j])
    allseatot[i, 3, j] = sum(montot[i, sea3, j])
    allseatot[i, 4, j] = sum(montot[i, sea4, j])
  }
}

# Calculate averages
avgmontot = apply(montot, 1:2, mean, na.rm = T)

seatot = matrix(NA, nr = length(uyr), nc = 4)
for(i in 1:length(uyr)) {
  seatot[i, 1] = sum(avgmontot[i, sea1])
  seatot[i, 2] = sum(avgmontot[i, sea2])
  seatot[i, 3] = sum(avgmontot[i, sea3])
  seatot[i, 4] = sum(avgmontot[i, sea4])
}

## Create seasonal total vector (repeats seasonal value for every day within season)
sealen = c(sum(days_in_month(sea1)), sum(days_in_month(sea2)), sum(days_in_month(sea3)), sum(days_in_month(sea4)))
sealen.leap = c(sum(days_in_month(as_date(paste0("2020-", sea1, "-01")))),
                sum(days_in_month(as_date(paste0("2020-", sea2, "-01")))),
                sum(days_in_month(as_date(paste0("2020-", sea3, "-01")))),
                sum(days_in_month(as_date(paste0("2020-", sea4, "-01")))))

ST = c()
for(i in 1:nrow(seatot)) {
  ST_tmp <- rep(0, yearID[i])
  for(j in 1:ncol(seatot)) {
    if(yearID[i] == 365) {
      for(k in 1:length(seasons[[j]])) {
        ST_tmp[lubridate::yday(paste0("2019-", seasons[[j]], "-01"))[k]:
                 (lubridate::yday(paste0("2019-", seasons[[j]], "-01"))[k] + days_in_month(seasons[[j]][k]) -1)] <-
          seatot[i, j]
      }
    } else if(yearID[i] == 366) {
      for(k in 1:length(seasons[[j]])) {
        ST_tmp[lubridate::yday(paste0("2020-", seasons[[j]], "-01"))[k]:
                 (lubridate::yday(paste0("2020-", seasons[[j]], "-01"))[k] + days_in_month(seasons[[j]][k]) -1)] <-
          seatot[i, j]
      }
    }
  }
  ST <- c(ST, ST_tmp)
}

ST1 = ST2 = ST3 = ST4 = ST
ST1[mo!=sea1[1] & mo!=sea1[2] & mo!=sea1[3]] = 0
ST2[mo!=sea2[1] & mo!=sea2[2] & mo!=sea2[3]] = 0
ST3[mo!=sea3[1] & mo!=sea3[2] & mo!=sea3[3]] = 0
ST4[mo!=sea4[1] & mo!=sea4[2] & mo!=sea4[3]] = 0

# Regional mean maximum temperatures as covariates (by season)
maxmean = array(dim = c(length(uyr), 12, np))
for(i in 1:np) {
  tmp = as.data.table(cbind(yr, mo, da, MX[ , i]))
  names(tmp)[4] <- "V4" #rename column to work with below function
  for(j in 1:length(unique(mo))) {
    tmp1 = tmp[mo == j]
    maxmean[ , j, i] = as.vector(t(ddply(tmp1, .(yr), summarise, total = mean(V4, na.rm = T), na.rm = T)[2]))
  }
}

allseamax = array(dim = c(length(uyr), 4, np))
for(j in 1:np) {
  for(i in 1:length(uyr)) {
    allseamax[i, 1, j] = mean(maxmean[i, sea1, j])
    allseamax[i, 2, j] = mean(maxmean[i, sea2, j])
    allseamax[i, 3, j] = mean(maxmean[i, sea3, j])
    allseamax[i, 4, j] = mean(maxmean[i, sea4, j])
  }
}

# Calculate averages
avgmaxmean = apply(maxmean, 1:2, mean, na.rm = T)

seamax = matrix(NA, nr = length(uyr), nc = 4)
for(i in 1:length(uyr)) {
  seamax[i, 1] = mean(avgmaxmean[i, sea1])
  seamax[i, 2] = mean(avgmaxmean[i, sea2])
  seamax[i, 3] = mean(avgmaxmean[i, sea3])
  seamax[i, 4] = mean(avgmaxmean[i, sea4])
}

## Create seasonal vector (repeats seasonal value for every day within season)
SMX = c()
for(i in 1:nrow(seamax)) {
  SMX_tmp <- rep(0, yearID[i])
  for(j in 1:ncol(seamax)) {
    if(yearID[i] == 365) {
      for(k in 1:length(seasons[[j]])) {
        SMX_tmp[lubridate::yday(paste0("2019-", seasons[[j]], "-01"))[k]:
                  (lubridate::yday(paste0("2019-", seasons[[j]], "-01"))[k] + days_in_month(seasons[[j]][k]) -1)] <-
          seamax[i, j]
      }
    } else if(yearID[i] == 366) {
      for(k in 1:length(seasons[[j]])) {
        SMX_tmp[lubridate::yday(paste0("2020-", seasons[[j]], "-01"))[k]:
                  (lubridate::yday(paste0("2020-", seasons[[j]], "-01"))[k] + days_in_month(seasons[[j]][k]) -1)] <-
          seamax[i, j]
      }
    }
  }
  SMX <- c(SMX, SMX_tmp)
}

SMX1 = SMX2 = SMX3 = SMX4 = SMX
SMX1[mo!=sea1[1] & mo!=sea1[2] & mo!=sea1[3]] = 0
SMX2[mo!=sea2[1] & mo!=sea2[2] & mo!=sea2[3]] = 0
SMX3[mo!=sea3[1] & mo!=sea3[2] & mo!=sea3[3]] = 0
SMX4[mo!=sea4[1] & mo!=sea4[2] & mo!=sea4[3]] = 0

# Regional mean minimum temperatures as covariates (by season)
minmean = array(dim = c(length(uyr), 12, np))
for(i in 1:np) {
  tmp = as.data.table(cbind(yr, mo, da, MN[ , i]))
  names(tmp)[4] <- "V4" #rename column to work with below function
  for(j in 1:length(unique(mo))) {
    tmp1 = tmp[mo == j]
    minmean[ , j, i] = as.vector(t(ddply(tmp1, .(yr), summarise, total = mean(V4, na.rm = T), na.rm = T)[2]))
  }
}

allseamin = array(dim = c(length(uyr), 4, np))
for(j in 1:np) {
  for(i in 1:length(uyr)) {
    allseamin[i, 1, j] = mean(minmean[i, sea1, j])
    allseamin[i, 2, j] = mean(minmean[i, sea2, j])
    allseamin[i, 3, j] = mean(minmean[i, sea3, j])
    allseamin[i, 4, j] = mean(minmean[i, sea4, j])
  }
}

# Calculate averages
avgminmean = apply(minmean, 1:2, mean, na.rm = T)

seamin = matrix(NA, nr = length(uyr), nc = 4)
for(i in 1:length(uyr)) {
  seamin[i, 1] = mean(avgminmean[i, sea1])
  seamin[i, 2] = mean(avgminmean[i, sea2])
  seamin[i, 3] = mean(avgminmean[i, sea3])
  seamin[i, 4] = mean(avgminmean[i, sea4])
}

## Create seasonal vector (repeats seasonal value for every day within season)
SMN = c()
for(i in 1:nrow(seamin)) {
  SMN_tmp <- rep(0, yearID[i])
  for(j in 1:ncol(seamin)) {
    if(yearID[i] == 365) {
      for(k in 1:length(seasons[[j]])) {
        SMN_tmp[lubridate::yday(paste0("2019-", seasons[[j]], "-01"))[k]:
                  (lubridate::yday(paste0("2019-", seasons[[j]], "-01"))[k] + days_in_month(seasons[[j]][k]) -1)] <-
          seamin[i, j]
      }
    } else if(yearID[i] == 366) {
      for(k in 1:length(seasons[[j]])) {
        SMN_tmp[lubridate::yday(paste0("2020-", seasons[[j]], "-01"))[k]:
                  (lubridate::yday(paste0("2020-", seasons[[j]], "-01"))[k] + days_in_month(seasons[[j]][k]) -1)] <-
          seamin[i, j]
      }
    }
  }
  SMN <- c(SMN, SMN_tmp)
}

SMN1 = SMN2 = SMN3 = SMN4 = SMN
SMN1[mo!=sea1[1] & mo!=sea1[2] & mo!=sea1[3]] = 0
SMN2[mo!=sea2[1] & mo!=sea2[2] & mo!=sea2[3]] = 0
SMN3[mo!=sea3[1] & mo!=sea3[2] & mo!=sea3[3]] = 0
SMN4[mo!=sea4[1] & mo!=sea4[2] & mo!=sea4[3]] = 0

## Precipitation occurrence is modeled using probit regression (i.e. fit a Generalized Linear Model (GLM) at each location separately)
# Save the coefficients, save the model fit in a list, one list element for each location
PROBIT <- list()
# Save the model residuals in a matrix of same dimensions as OCC
PROBITres <- matrix(NA, nrow = nrow(OCC), ncol = ncol(OCC))
# Begin loop over locations
for(i in 1:np) {
  # Define response variable
  Y.OCC <- OCC[ , i]
  
  # Define design matrix (i.e. covariates)
  if(sea_cov == "with") {
    X.OCC <- cbind(POCC[ , i], ct, st, ST1, ST2, ST3, ST4) #with seasonal covariates
  } else if(sea_cov == "without") {
    X.OCC <- cbind(POCC[ , i], ct, st) #without seasonal covariates
  }
  
  # Save model in list element "i"
  PROBIT[[i]] <- glm(Y.OCC ~ X.OCC, family = binomial(probit))
  # Save model residuals in column "i" of PROBITres matrix
  # Identify any missing values
  missing.id <- (!is.na(Y.OCC) & !is.na(apply(X.OCC, 1, sum)))
  PROBITres[missing.id, i] <- PROBIT[[i]]$residuals
}
coefocc <- matrix(NA, nrow = length(PROBIT[[1]]$coefficients), ncol = np)
for(i in 1:np) {
  coefocc[ , i] <- PROBIT[[i]]$coefficients 
}

## Precipitation amount is modeled using a Gamma model with spatially-varying shape and spatio-temporally varying scale 
# Save the model fit in a list, one list element for each location
GAMMA <- list()
# No model residuals - assumed to be negligible
# Begin loop over locations
for(i in 1:np) {
  # Identify which values are NA, remove them (Gamma GLM hates NAs)
  missing.id <- (!is.na(PPI[ , i]))[ , 1]
  # Define response variable
  Y.AMT <- as_vector(PPI[missing.id, i])
  
  # Define design matrix (i.e. covariates)
  if(sea_cov == "with") {
    X.AMT <- cbind(ct[missing.id], st[missing.id], ST1[missing.id], ST2[missing.id], ST3[missing.id], ST4[missing.id]) #with seasonal covariates
  } else if(sea_cov == "without") {
    X.AMT <- cbind(ct[missing.id], st[missing.id]) #without seasonal covariates
  }
  
  # Save model in list element "i"
  GAMMA[[i]] <- glm(Y.AMT ~ X.AMT, family = Gamma(link = log))
}
coefamt <- matrix(NA, nrow = length(GAMMA[[1]]$coefficients), ncol = np)
for(i in 1:np) {
  coefamt[ , i] <- GAMMA[[i]]$coefficients 
}

## Minimum temperature is modeled using linear regression
# Save the model fit in a list, one list element for each location
TMIN <- list()
# Save the model residuals in a matrix of same dimensions as MN
TMINres <- matrix(NA, nrow = nrow(MN), ncol = ncol(MN))
# Begin loop over locations
for(i in 1:np) {
  # Define response variable
  Y.MIN <- as_vector(MN[ , i])
  
  # Define design matrix (i.e. covariates)
  if(sea_cov == "with") {
    X.MIN <- as.matrix(cbind(PMN[ , i], PMX[ , i], ct, st, OCC[ , i], Rt, SMN1, SMN2, SMN3, SMN4, SMX1, SMX2, SMX3, SMX4)) #with seasonal covariates
  } else if(sea_cov == "without") {
    X.MIN <- as.matrix(cbind(PMN[ , i], PMX[ , i], ct, st, OCC[ , i], Rt)) #without seasonal covariates
  }
  
  # Save model in list element "i"
  TMIN[[i]] <- lm(Y.MIN ~ X.MIN)
  # Save model residuals in column "i" of TMINres matrix
  # Identify any missing values
  missing.id <- (!is.na(Y.MIN) & !is.na(apply(X.MIN, 1, sum)))
  TMINres[missing.id, i] <- TMIN[[i]]$residuals 
}
coefmin <- matrix(NA, nrow = length(TMIN[[1]]$coefficients), ncol = np)
for(i in 1:np) {
  coefmin[ , i] <- TMIN[[i]]$coefficients 
}

## Maximum temperature is modeled using linear regression
# Save the model fit in a list, one list element for each location
TMAX <- list()
# Save the model residuals in a matrix of same dimensions as MX
TMAXres <- matrix(NA, nrow = nrow(MX), ncol = ncol(MX))
# Begin loop over locations
for(i in 1:np) {
  # Define response variable
  Y.MAX <- as_vector(MX[ , i])
  
  # Define design matrix (i.e. covariates)
  if(sea_cov == "with") {
    X.MAX <- as.matrix(cbind(PMN[ , i], PMX[ , i], ct, st, OCC[ , i], Rt, SMN1, SMN2, SMN3, SMN4, SMX1, SMX2, SMX3, SMX4)) #with seasonal covariates
  } else if(sea_cov == "without") {
    X.MAX <- as.matrix(cbind(PMN[ , i], PMX[ , i], ct, st, OCC[ , i], Rt)) #without seasonal covariates
  }
  
  # Save model in list element "i"
  TMAX[[i]] <- lm(Y.MAX ~ X.MAX)
  # Save model residuals of column "i" of TMAXres matrix
  # Identify any missing values
  missing.id <- (!is.na(Y.MAX) & !is.na(apply(X.MAX, 1, sum)))
  TMAXres[missing.id, i] <- TMAX[[i]]$residuals 
}
coefmax <- matrix(NA, nrow = length(TMAX[[i]]$coefficients), ncol = np)
for(i in 1:np) {
  coefmax[ , i] <- TMAX[[i]]$coefficients 
}

# Residuals from minimum and maximum temperatures are assumed to come from the same distribution,
#  therefore we combine them to estimate the spatial covariance matrices...
TEMPcov <- list()
# Begin loop over months to account for seasonality trends in model residuals
for(i in 1:12) {
  TEMPcov[[i]] <- cov(rbind(TMAXres[mo == i, ], TMINres[mo == i, ]), use = "pairwise.complete")
}

# Now calculate the spatial correlation matrix for precipitation occurrence residuals...
#  CORRELATION matrices are used instead of COVARIANCE matrices because probit regression has variance unity by definition
PRCPcor <- list()
# Begin loop over months to account for seasonality trends in model residuals
for(i in 1:12) {
  PRCPcor[[i]] <- cor(PROBITres[mo == i, ], use = "pairwise.complete")
}