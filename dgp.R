# This file contains functions for specifying DGP for monte-carlo exercise

# Data generation process

# nobs        : number of observations
# no_of_lags  : number of lags, effectively number of regressors
# weight      : effectively betas
# betaN       : constant
# rho         : a scalar works like a loading on X %*% weight

DGP <- function(nobs, no_of_lags, weight, betaN, rho){
  
  X = matrix(rnorm(nobs * no_of_lags), nobs)
  y = betaN + rho * X %*% weight + rnorm(nobs)
  
  return(list(X = X, y = y))
  
}

# A function to generate exponential almon lag data
gen_exp_almon_data <- function(nobs, no_of_lags, alpha, betaN, rho) {
  
  # calculate the weight
  correct_almon_weight = almon_weight(alpha, no_of_lags)
  
  # generate data
  data_cor_specification = DGP(nobs, no_of_lags,
                               correct_almon_weight, betaN, rho)  
  
  # bind them togther
  df = cbind.data.frame(y = data_cor_specification$y,
                        data_cor_specification$X)
  
  result = list(weight = correct_almon_weight,
                data = df)
  
  return(result)
}

# A function to generate misspecified exponential almon lag data
gen_mis_exp_almon_data <- function(nobs, no_of_lags, weight, betaN, rho) {
  
  # generate data
  data_misspecification = DGP(nobs, no_of_lags, weight, betaN, rho)
  
  # bind them togther
  df = cbind.data.frame(y = data_misspecification$y,
                             data_misspecification$X)
  
  result = list(weight = weight,
                data = df)
  
  return(result)
}
