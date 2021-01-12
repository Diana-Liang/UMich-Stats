# Toy Example ----------------------------------------------------------------
#A <- rnorm(100, 0, 5); B <- rnorm(100, 10, 5)
#C <- rnorm(100, 100, 5); D <- rnorm(100, -10, 5)
#E <- rnorm(100, 0, 5); G <- rnorm(100, 120, 5)
#H <- rnorm(100, 1000, 5); I <- rexp(100)

#y_actual <- (-5)*A + 10*B + 2*C + 0*D +
#  0.001*E + (-5)*G + 0.0005*H + (-10)*I
#x_mat <- matrix(c(A, B, C, D, E, G, H, I), ncol=8)

# Function -------------------------------------------------------------------

# Solves elastic net by gradient descent; written by Diana Liang
grad_desc_enet <- function(X, y, beta0=rep(0,(dim(X)[2]+1)),
                        lambda, alpha,
                        step_size=1E-6,
                        tol=1E-6, max_iter=1E6, ...){
  ## Input
  ## X: matrix of covariate values
  ## y: vector of response
  ## beta0: vector of starting estimates
  ## lambda: nonnegative number
  ## alpha: number inclusively between 0 and 1
  ## step_size, tol, max_iter: numbers
  
  ## Output
  ## beta: vector of final estimates
  ## loss, step_size, convergence, iter: numbers
  
  # Set up values
  n <- dim(X)[1]; p <- dim(X)[2]
  oneX <- cbind(1, X)
  
  # Initialize
  if(sum(beta0)==0){
    ## ridge reg modified from MK ------------##
    crossX <- crossprod(oneX)
    diag(crossX) <- diag(crossX) + lambda
    beta_est <- solve(crossX, crossprod(oneX,y))
    ## ridge reg modified from MK ------------##
  } else{
    beta_est <- beta0
  }
  d_beta <- beta_est
  loss_f <- 0
  convergence <- 1
  
  # Update through iters
  for(iter in 1:max_iter){
    
    # Set up for tolerance
    loss_last <- loss_f
    
    # Calculate y difference
    y_pred <- oneX %*% beta_est
    y_diff <- y-y_pred
    
    # Calculate d_beta for each beta
    d_beta <- as.vector(-1*crossprod(oneX, y_diff))/n
    step <- (1-alpha)*lambda*beta_est
    step[1] <- 0
    d_beta <- d_beta + step
    step <- ifelse(beta_est > 0, 1, -1)
    step[1] <- 0
    d_beta <- d_beta + step*lambda*alpha
    
    # Update with step_size
    beta_est <- beta_est - step_size*d_beta
    ## Soft thresholding modified from MK -----------------------##
    beta_est <- ifelse(abs(beta_est) > (alpha*lambda), beta_est, 0)
    ## Soft thresholding modified from MK -----------------------##
    
    # Recalculate loss
    loss_f <- loss_fun(beta = beta_est, X=oneX, Y=y,
                       alpha = alpha, lambda = lambda)
    
    # Check tolerance
    if(abs(loss_f-loss_last) < (tol*(abs(loss_f)+abs(loss_last))) ){
      # If tolerance reached escape loop
      convergence = 0
      break
    }
    
  }
  
  # Return values at the end
  return(list(beta=beta_est,loss=loss_f,step_size = step_size,
              convergence=convergence,iter=iter))
}

#system.time(t1 <- grad_desc_enet(x_mat, y_actual, lambda=0.5, alpha=1, step_size=1E-9))
#library(glmnet)
#t2 <- glmnet(x_mat, y_actual, family='gaussian', lambda=0.5, alpha=1)