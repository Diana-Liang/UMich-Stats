setwd("~/2020/Biostats 615")

# Prob 1: Find the stationary distribution of a discrete Markov chain
args <- commandArgs(trailingOnly = TRUE)
stat_distrib <- function(l_st){
  # set up the matrix
  p_con <- file(l_st, "rb")
  n <- readBin(p_con, integer(), n=1)
  p_mat <- matrix(readBin(p_con, numeric(), n=(n^2)),
                  nrow=n, ncol=n)
  close.connection(p_con)
  
  Q <- p_mat-diag(n)
  Q <- rbind(Q, rep(1, n))
  b <- c(rep(0, n), 1)

  A <- crossprod(Q)
  b <- crossprod(Q,b)
  
  pi_vector <- solve(A, b)
  for(i in 1:length(pi_vector)){
    cat(formatC(pi_vector[i], digits=3,flag="-"), "\n")
  }
}
stat_distrib(args)
system.time({ stat_distrib("test1.prob") })
stat_distrib("test2.prob")

#-------------------------------------------------------------------------------

# Prob 2: Fit Kernel Ridge Regression
args <- commandArgs(trailingOnly = TRUE)

kernel <- function(x, x_prime, h, p){
  val <- ifelse(abs(x-x_prime)>h, 0, exp(-1*p*(x-x_prime)^2))
  return(val)
}

kernalRidReg <- function(l_st){
  #require(Matrix)
  df <- matrix(read.csv(l_st[[1]]))
  l <- as.numeric(l_st[[2]])
  p <- as.numeric(l_st[[3]])
  h <- as.numeric(l_st[[4]])
  x_train <- df[[1]]
  y_train <- df[[2]]
  
  K_mat <- outer(x_train, x_train,
                 function(x, x_prime) kernel(x=x, x_prime=x_prime, h=h, p=p))
  diag(K_mat) <- 1+l
  #dims <- dim(K_mat)
  #idx <- which(K_mat != 0, arr.ind=TRUE)
  #v <- K_mat[K_mat!=0]
  #K_mat <- sparseMatrix(i=idx[,1], j=idx[,2],
  #                      x=v, dims=dims)
  
  b_hat_1 <- solve(K_mat, y_train)
  
  #A <- crossprod(K_mat)
  #b <- crossprod(K_mat, y_train)
  #U <- chol(A)
  
  #z <- forwardsolve(U, b,
  #                  upper.tri=TRUE, transpose=TRUE)
  #b_hat_2 <- as.vector(backsolve(U, z))
  
  x_test <- df[[3]]
  y_test <- df[[4]]
  K_test <-outer(x_train, x_test,
                 function(x, x_prime) kernel(x=x, x_prime=x_prime, h=h, p=p))
  pred_test <- b_hat_1 %*% K_test
  pmse <- sum((y_test-pred_test)^2)/length(x_test)
  cat(formatC(pmse, digits=4,flag="-"), "\n")
  
}
kernalRidReg(args)
system.time({ kernalRidReg(list("test2.csv", 6, 0.1, 5)) })
system.time({ kernalRidReg(list("test1.csv", 3, 1.0, 3)) })

