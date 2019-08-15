# Exponetial almon weights

# alpha1 : first parameter of Almon polynomial weights
# alpha2 : second parameter of Almon polynomial weights

almon <- function(alpha1, alpha2, j) {exp(alpha1 * j + alpha2 * j^2)}

almon_weight <- function(alpha, no_of_lags) {
  
  alpha1 = alpha[1]
  alpha2 = alpha[2]
  
  j = 0:(no_of_lags - 1)
  almon(alpha1, alpha2, j)
  
}