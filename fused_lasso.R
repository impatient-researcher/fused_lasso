fused_lasso <- list(library = "genlasso",
                    
                    type = "Regression",
                    
                    parameters = data.frame(parameter = c('lambda', 'gamma'),
                                            class = c("numeric", "numeric"),
                                            label = c('Fusion Penalty', 'Sparsity Loading')),
                    
                    grid = function(x, y, len = NULL, search = "grid") {
                      
                      if(search == "grid") {
                        
                        grid <- expand.grid(lambda = exp(-seq(1, 10, length = len)),
                                            gamma = seq(1,5,1))
                        
                      } else {grid <- expand.grid(lambda = 2^runif(len, min = -10, max = 3),
                                                  gamma = c(1,2))}
                      
                      return(grid)},
                    
                    loop = function(grid) {
                      
                      gamma <- unique(grid$gamma)
                      loop <- data.frame(gamma = gamma)
                      loop$lambda <- NA
                      submodels <- vector(mode = "list", length = length(gamma))
                      
                      for(i in seq(along = gamma)) {
                        
                        np <- grid[grid$gamma == gamma[i], "lambda"]
                        
                        loop$lambda[loop$gamma == gamma[i]] <- np[which.max(np)]
                        
                        submodels[[i]] <- data.frame(lambda = np[-which.max(np)])
                        
                      }
                      
                      list(loop = loop, submodels = submodels)
                      
                    },
                    
                    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                      
                      theDots <- list(...)
                      
                      if(!(class(x)[1] %in% c("matrix", "sparseMatrix")))
                        x <- Matrix::as.matrix(x)
                      
                      modelArgs <- c(list(X = x, y = y, 
                                          gamma = param$gamma), theDots)
                      
                      out <- do.call(genlasso::fusedlasso1d, modelArgs)
                      
                      if(!is.na(param$lambda[1])) out$lambdaOpt <- param$lambda[1]
                      
                      return(out)
                      
                    },
                    
                    predict = function(modelFit, newdata, submodels = NULL) {
                      
                      if(!is.matrix(newdata)) newdata <- Matrix::as.matrix(newdata)

                      out <- predict(object = modelFit, Xnew = newdata, lambda = modelFit$lambdaOpt)$fit
                      
                      if(is.matrix(out)) out <- out[,1]
                      
                      if(!is.null(submodels)) {
                        
                        tmp <- as.list(as.data.frame(predict(object = modelFit,
                                                             Xnew = newdata, lambda = submodels$lambda)$fit))
                        out <- c(list(out), tmp)
                        
                      }
                      
                      return(out)
                      
                    },
                    
                    prob = NULL,
                    
                    sort = function(x) x[order(-x$lambda, x$gamma),]
)