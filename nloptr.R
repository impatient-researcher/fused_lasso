# Grid search for nloptr algorithm

# init        : initial search grid, it takes a matrix that has 4 columns and any number of rows
# outcome     : a vector of y variables
# features    : a conformable matrix of x variables
# no_of_lags  : number of lags, effectively number of regressors
# rss         : residual sum of square
# gradient    : gradient

opts <- list("algorithm" = "NLOPT_LD_SLSQP",                       # options given in the shell script
            "xtol_rel" = 1.0e-9,                                   # these are stopping rules for the algorithm
            "ftol_rel" = 1.0e-6,
            "maxeval" = 500)

init <- as.matrix(expand.grid(seq(-8, 8, 2),
                              seq(-8, 8, 2),
                              seq(-8, 8, 2),
                              seq(-8, 0, 2))) 

nloptr_grid_search <- function(outcome, features, no_of_lags,
                               init, rss, gradient) {
  
  n_init = length(init[,1]);
  
  nloptr_obj = matrix(Inf, n_init)                                 # for storing all objective function values w.r.t n_init
  nloptr_sol = matrix(0, n_init, 4)                                # for storing all solution values w.r.t n_init
  
  for (i in 1:n_init){
    nloptr = try(nloptr(x0 = init[i,],
                        eval_f = rss,
                        eval_grad_f = gradient,
                        outcome = outcome,
                        features = features,
                        no_of_lags = no_of_lags,
                        ub = c(Inf, Inf, Inf, -0.00001),	         # upperbound is suggested by the question paper - "a very small number"
                        opts = opts), silent = T)                  # silent=T means supressing error messages
    
    if(is(nloptr,"try-error")) {                                   # place a catch-all fikter here to skip error cases
      
      next
    
    }
   
    nloptr_obj[i] = nloptr$objective                               # append successful cases to the matrix
    nloptr_sol[i,] = nloptr$solution
  
  }
  
  init[is.na(nloptr_obj)] <- Inf                                   # replace nan by Inf
  init_final <- init[which.min(nloptr_obj),]                       # search for one of the initial sets that can output the "global" minimum
  
  
  plot(nloptr_obj,                                                 # plot the search process
       main = "Optimal value of objective function", 
       xlab = "Search index",
       ylab = "Value of objective function")
  
  return(init_final)

}

# Perform NLOPTR

# A helper function for generating predicted y
y_pred = function(theta, features, no_of_lags){
  
  y = theta[2] + theta[1] * features %*% almon(theta[3], theta[4], 1:no_of_lags)
  
  return(y)
}

# Actual NLOPTR functions
do_nloptr <- function(outcome, features, no_of_lags,
                      init, rss, gradient) {
  
  #require(ggplotify)
  require(caret)
  require(nloptr)
  require(optimx)
  
  init_final <- nloptr_grid_search(outcome = outcome,
                                   features = features,
                                   no_of_lags = no_of_lags,
                                   init = init,
                                   rss = rss,
                                   gradient = gradient)

  nloptr_results <- nloptr(x0 = init_final,                        # run NLOPTR again with the best initial value
                           eval_f = rss,
                           eval_grad_f = gradient,
                           outcome = outcome,
                           features = features,
                           no_of_lags = no_of_lags,
                           ub = c(Inf, Inf, Inf, -0.00001),
                           opts = opts)
  
  theta_est <- nloptr_results$solution                             # save the estimated parameters
  
  y_pred <- y_pred(theta_est,                                      # save predicted values
                        features = features,
                        no_of_lags = no_of_lags)  
  
  rmse = RMSE(outcome, y_pred)
  
  #y_pred_plot = as.grob(function() {
    
    plot(outcome,
         main = "Visualisating actual and predictions based on NLOPTR",
         ylab = "Value")
    lines(outcome)
    lines(y_pred, col = "red")
    legend("topright", legend=c("Estimates", "Actual"),
           col = c("red", "black"), lty=1, cex=0.8)
  
  #})
  
  second_opinion = optimx(par = init_final,                         # automatically call optimx too to cross-check
                                 fn = rss,
                                 gr = gradient,
                                 outcome = outcome,
                                 features = features,
                                 no_of_lags = no_of_lags,
                                 control = list(all.methods = TRUE,
                                                save.failures = TRUE, maxit = 500))
  
  result_list = list(model = nloptr_results,
                     second_opinion = second_opinion,
                     theta_est = theta_est,
                     y_pred = y_pred,
                     rmse = rmse)
                     #y_pred_plot = y_pred_plot)
  
  return(result_list)
  
}
