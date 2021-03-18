setwd("D:/R Work/FacilityAnomalies")
library(modi)
library(dplyr)
dwh <- read.csv('./DWH_AS_Logs.csv', header = TRUE, stringsAsFactors = FALSE)

# Upload date inconsistencies ------------------------

vars <- c("TXCURR", "Patients", "ART", "Baseline", "Visits", "Pharm", "Labs", "Exits", "adv")
dwh[, vars] <- apply(dwh[, vars], 2, as.numeric)
dwh[is.na(dwh)] <- 0
vars_var <- apply(dwh[, vars], 2, var)

# Split dataset by facility-upload date combinations
dwh_split <- split(dwh, paste0(dwh$MFLCode, dwh$DateUploaded))

differences <- data.frame()

# For each facility_dateupload combo:
for(i in 1:length(dwh_split)){

  dat <- dwh_split[[i]]

  if(nrow(dat) == 1){next}

  for(j in 1:length(vars)){

    max_value <- max(dat[, vars[j]])
    min_value <- min(dat[, vars[j]])
    dif_var <- max_value - min_value
    dif_norm <- dif_var / vars_var[j]

    if(dif_var > 0){
      df <- data.frame(MLFCode = dat$MFLCode[1],
                       DateUploaded = dat$DateUploaded[1],
                       Variable = vars[j],
                       Max_Value = max_value,
                       Min_Value = min_value,
                       Difference = dif_var,
                       Difference_Normalized = dif_norm)

      differences <- rbind(differences, df)
    }

  }
}

# Many of these appear to be roughly double a value from a similar upload
# Let's add a flag if the max is between 199% and 201% of the min, as this suggest as doubling
differences <- differences %>%
  mutate(Gap = Max_Value / Min_Value,
         Double = ifelse(Gap > 1.99 & Gap < 2.01, "Yes", "No"))

write.csv(differences, './dwh_norm_diff_uploaddate.csv', row.names = FALSE)

# Changes between upload dates -----------------------------------
# Split dataset by facility-upload date combinations
dwh_split <- split(dwh, dwh$MFLCode)

differences_fac <- data.frame()

# For each:
for(i in 1:length(dwh_split)){

  dat <- dwh_split[[i]]

  if(nrow(dat) == 1){next}

  for(j in 1:length(vars)){

    max_value <- max(dat[, vars[j]])
    min_value <- min(dat[, vars[j]])
    dif_var <- max_value - min_value
    dif_norm <- dif_var / vars_var[j]

    if(dif_var > 0){
      df <- data.frame(MLFCode = dat$MFLCode[1],
                       Variable = vars[j],
                       Max_Value = max_value,
                       Min_Value = min_value,
                       Difference = dif_var,
                       Difference_Normalized = dif_norm)

      differences_fac <- rbind(differences_fac, df)
    }

  }
}

write.csv(differences_fac, './dwh_norm_diff_facility.csv', row.names = FALSE)

# Look for covariance based anomalies ----------------------------
dwh_vars <- dwh[, vars]

# get sparse mu (vector of means)
mu <- colMeans(dwh_vars) # means
k <- length(vars) # number of variables

N <- matrix(0, k, k) # set up k by k matrix
diag(N) <- nrow(dwh_vars) # diagonal initiated with count of present values

i_mat <- matrix(0, k, k) # set up identity matrix
diag(i_mat) <- 1

S <- matrix(0, k, k)

for (i in 1:nrow(dwh_vars)){
  dat <- dwh_vars[i, ]
  inds <- which(!is.na(dat))
  yt <- dat[inds]
  yt_mu <- as.matrix(yt - mu[inds])

  Hyt <- as.matrix(i_mat[inds, ])
  if(dim(Hyt)[2] == 1){Hyt <- t(Hyt)}

  S <- S + (t(Hyt) %*% (t(yt_mu) %*% yt_mu) %*% Hyt)
}

N_sqrt <- sqrt(N)
diag(N_sqrt) <- 1/(diag(N_sqrt))
R <- (N_sqrt %*% S %*% N_sqrt)


# Calculate Mahalanobis distance
dwh$MD_sp <- MDmiss(dwh_vars, center = mu, cov = R)
cv<-qchisq(.95,df=length(vars)-1)
dwh$outlier <- ifelse(dwh$MD_sp>cv, 1, 0)

write.csv(dwh, './dwh_mahalanobis.csv', row.names = FALSE)
