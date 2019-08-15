# Create a wrapper for all 4 machine learning algorithm

# training_data    : training data used, outcome variable must be named as y
# rf_grid          : paramter grid for random forest based on ranger
# gbm_grid         : paramter grid for gbm
# lasso_grid       : paramter grid for lasso
# fused_lasso_grid : paramter grid for fused lasso
# control          : training control for caret

do_ml <- function(training_data, rf_grid, gbm_grid, lasso_grid, fused_lasso_grid, control){
  
  require(caret)
  source("fused_lasso.R")
  
  rf_fit = train(y ~ ., training_data, method = "ranger", tuneGrid = rf_grid, trControl = control)
  gbm_fit = train(y ~ ., training_data, method = "gbm", tuneGrid = gbm_grid, trControl = control)
  lasso_fit = train(y ~ ., training_data, method = "glmnet", tuneGrid = lasso_grid, trControl = control)
  fused_fit = train(y ~ ., training_data, method = fused_lasso, tuneGrid = fused_lasso_grid, trControl = control)
  fused_fit$method = "fused_lasso"
  
  # put all models together
  all_models = list(rf_fit, gbm_fit, lasso_fit, fused_fit)
  
  # extract the method used - and name the list with that
  names(all_models) = sapply(all_models, function(x) x$method) 
  
  return(all_models)
}

# Extract predictions made by the *best-tuned* ml model on *validation* dataset
# This is essential for stacking/blending ml models

# model : model object returned by do_ml function defined above

get_validation_prediction <- function(model) {
  
  require(dplyr)
  
  if(model$method == "ranger"){
    validation_predictions = model$pred %>%
      dplyr::filter(mtry == model$bestTune$mtry,
                    splitrule == model$bestTune$splitrule,
                    min.node.size == model$bestTune$min.node.size)
    
  } else if (model$method == "gbm"){
    
    validation_predictions = model$pred %>%
      dplyr::filter(shrinkage == model$bestTune$shrinkage,
                    interaction.depth == model$bestTune$interaction.depth,
                    n.minobsinnode == model$bestTune$n.minobsinnode,
                    n.trees == model$bestTune$n.trees)
    
  } else if (model$method == "glmnet") {
    validation_predictions = model$pred %>%
      dplyr::filter(alpha == model$bestTune$alpha,
                    lambda == model$bestTune$lambda)
  }
  
  else if (model$method == "fused_lasso") {
    validation_predictions = model$pred %>%
      dplyr::filter(gamma == model$bestTune$gamma,
                    lambda == model$bestTune$lambda)
  }
  
  # extract only the predictions
  validation_predictions %>%
    arrange(rowIndex) %>%
    pull(pred)

}

# Forecast combination using stacking/blending technique
two_layer_stack <- function(training_data, models, control, resample_index) {
  
  require(caret)
  require(dplyr)
  
  # create a training data set for stacking/blending,
  # which consists of actual y in the validation set
  # as well as object returned by get_validation_prediction above
  
  validation_y = training_data %>% slice(resample_index$test %>% unlist())  # obtain actual y in validation set
  
  validation_predictions = sapply(models, get_validation_prediction)        # obtain predictions in validation set
    
  validation_df = cbind.data.frame(validation_y, validation_predictions)    # combine these 2
  
  # actual stacking algorithm - currently set as regularised regression
  
  stacking_fit = train(y ~  ranger + gbm + glmnet + fused_lasso,
                    data = validation_df, 
                    method = "glmnet", tuneLength = 10)
  
  
  stacking_coefficients = coef(stacking_fit$finalModel,                     # obtain the coefficients 
                                stacking_fit$bestTune$lambda) %>%
    as.vector(.)
  
  stacking_validation_prediction = validation_df %>%                        # generate predicted value
    dplyr::select(ranger, gbm, glmnet, fused_lasso) %>%
    cbind(1,.) %>%                                                          # give it column of 1 for intercept
    as.matrix(.) %*% stacking_coefficients                                  # essentially xb
  
  validation_df_with_stack = cbind(validation_df,                           # append predictions made by stacking
                                   stacking = stacking_validation_prediction)
  
  results = list(model = stacking_fit,
                 coefficents = stacking_coefficients,
                 validation_predictions = validation_df_with_stack)
  
  return(results)

}

# A small function to calculate RMSE for *validation data*
get_validation_rmse <- function(df) {
  
  df %>%
    dplyr::select(y, ranger, gbm, glmnet, fused_lasso, stacking) %>%
    mutate_at(vars(ranger, gbm, glmnet, fused_lasso, stacking),
              funs(abs((. - y)))) %>%                       # why abs not squared?
                                                            # because horizon = 1, they are the same
    dplyr::select(-y) %>%
    summarise_all(funs(mean))
  
}
  
# A small function to calculate RMSE for *test data*
get_test_rmse <- function(df) {
  
  require(caret)
  
  df %>%
    #summarise_at(vars(nloptr, ranger, gbm, glmnet, fused_lasso, stacking),
    summarise_at(vars(ranger, gbm, glmnet, fused_lasso, stacking),             
                 funs(RMSE(y, .)))
  
}

