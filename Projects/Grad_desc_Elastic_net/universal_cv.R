# A: Test case -------------------------------------------------------------------------

#x_mat = apply(matrix(rnorm(100^2), nrow=100, ncol=100),2,scale)
#beta = rnorm(100+1, sd=4)
#y_actual = rbinom(n=nrow(X), prob = logit(cbind(1,X)%*%beta), size = 1)

#test <- lambda_cv(X=x_mat, y=y_actual, lambda=c(0, 0.005, 0.05, 0.5, 1), alpha=0.05,
#                  family="binomial", fun=logistic_coord_descent,
#                  folds=5, max_iter=10000)
#test

# B: Lambda by CV ----------------------------------------------------------------------

# Finds optimal lambda for given alpha and function; written by Diana Liang
lambda_cv <- function(X, y, lambda=exp(seq(-5, 5, 0.5)),
                      alpha, alpha_step=0.1,
                      beta0=rep(0,(dim(X)[2]+1)), fun=coord.descent,
                      folds=10, step_size=1E-6,
                      tol=1E-6, max_iter=1E6, ...){
  ## Inputs:
  ##  X: covariate matrix
  ##  y: response vector
  ##  lambda: vector of possible lambdas to check
  ##  alpha: single numeric of alpha
  ##  family: "gaussian" for linear or "binomial" for logistic
  ##  fun: name of function to use (no quotation marks)
  ##  folds: number of folds to run
  ##  Any other inputs you'd put into specific elastic net function
  
  ## Outputs:
  ## lambda_mse: single numeric lambda for lowest MSE
  ## mse: vector of MSE measurements
  ## lambda_loss : single numeric lambda for lowest loss
  ## loss: vector of loss measurements
  ## lambda: vector of inputed lambda values
  ## folds: number of folds
  
  # Detect family
  if(as.character(expr(fun)) %in% c("coord_descent", "grad_desc_enet", "linear_enet")){
    family <- "gaussian"
  } else if (as.character(expr(fun)) == "logistic_coord_descent"){
    family <- "binomial"
  } else{
    family <- "unknown"
  }
  
  # Set up values
  n <- dim(X)[1]
  per_fold <- floor(n/folds)
  
  # Find MSE and loss for each lambda
  mse <- rep(0, length(lambda))
  loss <- rep(0, length(lambda))
  for(l in 1:length(lambda)){
    # Cycle through each lambda
    current_lambda <- lambda[l]
    
    # Calculate MSE and loss for each fold
    cv_mse <- rep(0, folds)
    cv_loss <- rep(0, folds)
    for(k in 1:folds){
      
      # Assign train or test for each fold
      if(k != folds){
        idx <- c(((k-1)*per_fold+1):(k*per_fold))
      } else{
        idx <- c(((k-1)*per_fold+1):n)
      }
      train_X <- X[-idx,]; test_X <- X[idx,]
      train_y <- y[-idx]; test_y <- y[idx]
      
      # Run model for that fold
      model <- fun(X=train_X, x=train_X, Y=train_y, y=train_y, beta0=beta0, lambda=current_lambda, alpha=alpha,
                   my_alpha=alpha, step_size=step_size, tol=tol, max_iter=max_iter, maxit=max_iter)
      
      # Check if converged and calculate MSE and loss
      if(length(model$convergence) != 0){
        if(model$convergence==0){
          if(family == "binomial"){
            pred_y <- ifelse(logit(cbind(1, test_X) %*% model$beta) > 0.5, 1, 0)
            cv_mse[k] <- sum(test_y != pred_y)/length(test_y)
            cv_loss[k] <- true_loss_fun(beta = model$beta, x=cbind(1,train_X), y=train_y,
                                   alpha = alpha, lambda = current_lambda)
          } else{
            err <- test_y - (cbind(1, test_X) %*% model$beta)
            cv_mse[k] <- mean(err^2)
            cv_loss[k] <- loss_fun(beta = model$beta, X=cbind(1,train_X), Y=train_y,
                                   alpha = alpha, lambda = current_lambda)
          }
        } else{
          cv_mse[k] <- NA
          cv_loss[k] <- NA
        }
      } else{
        err <- test_y - (cbind(1, test_X) %*% model$beta)
        cv_mse[k] <- mean(err^2)
        cv_loss[k] <- loss_fun(beta = model$beta, X=cbind(1,train_X), Y=train_y,
                               alpha = alpha, lambda = current_lambda)
      }
      
    }
    
    # Average folds
    mse[l] <- try(mean(cv_mse))
    loss[l] <- try(mean(cv_loss))
  }
  
  
  # Find lambda for minimum
  mse <- rev(mse); loss <- rev(loss); lambda <- rev(lambda)
  lambda_mse <- lambda[which.min(mse)]
  lambda_loss <- lambda[which.min(loss)]
  
  return(list(lambda_mse=lambda_mse,mse=rev(mse),
              lambda_loss=lambda_loss,loss=rev(loss),
              lambda=rev(lambda),folds=folds))
}
