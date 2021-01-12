# Prob 1: Calculate first 4 moments
args <- commandArgs(trailingOnly = TRUE)
momentStats <- function(l_st) {
  l_st <- as.numeric(l_st)
  n <- length(l_st)
  avg <- mean(l_st)
  cat(formatC(avg, digits=8,flag="-"), "\n")
  s_2 <- var(l_st)
  cat(formatC(s_2, digits=8,flag="-"), "\n")
  s_3 <- sqrt(s_2)^3
  s_4 <- sqrt(s_2)^4
  if(s_2 !=0){
    skew <- 0
    kurt <- 0
    for(i in 1:n){
      skew <- skew + ((l_st[i]-avg)^3)
      kurt <- kurt + ((l_st[i]-avg)^4)
    }
    skew <- skew/(n*s_3)
    cat(formatC(skew, digits=8,flag="-"), "\n")
    kurt <- (kurt/(n*s_4))-3
    cat(formatC(kurt, digits=8,flag="-"), "\n")
  }
}
momentStats(args)
#momentStats(c(1, 2, 3, 4, 5))

#----------------------------------------------------------------------------------------------

# Prob 2: Calculate betas for polynomial
args <- commandArgs(trailingOnly = TRUE)
conPolyReg <- function(l_st){
  l_st <- as.numeric(l_st)
  
  p <- as.integer(l_st[1])
  y <- l_st[-1]
  n <- length(y)
  
  x_begin <- (1:n)/n
  x <- 1
  for(each in p:2){
    x <- 1+(x_begin/each)*x
  }
  x <- x_begin*x
  
  y_cen <- y - mean(y)
  x_cen <- x - mean(x)
  sig_y2 <- sum(y_cen*y_cen)/(n-1)
  sig_x2 <- sum(x_cen*x_cen)/(n-1)
  sig_xy <- sum(x_cen*y_cen)/(n-1)
  rho_xy <- sig_xy/sqrt(sig_y2*sig_x2)
  b1 <- rho_xy*sqrt(sig_y2/sig_x2)
  
  b0 <- mean(y)-(b1*mean(x))
  
  betas <- c(mean(b0), b1)
  for(each in 2:p){
    betas[each+1] <- betas[each]/each
  }
  cat(formatC(betas, digits=8,flag="-"))

}
conPolyReg(args)
#conPolyReg(c(3, 0.5, 1, 1.5))
#conPolyReg(c(5, 1, 3, 3, 3, 4, 5, 3, 2, -1))

#--------------------------------------------------------------------------------------------------

# Prob 3: Simplified logistic regression 
args <- commandArgs(trailingOnly = TRUE)
predProb <- function(l_st){
  x <- as.numeric(l_st)
  
  rhs <- c()
  for(k in 1:5){
    j_th <- 0
    for(j in 1:length(x)){
      j_th <- j_th + (2^abs(k-j))*x[j]
    }
    j_th <- j_th+2^(-1*k)
    rhs[k] <- sum(j_th)
  }
  rhs <- rhs-max(rhs)
  
  prob <- exp(rhs)/sum(exp(rhs))
  for(each in 1:5){
    cat(formatC(prob[each],digits=8,flag="-"), "\n")
  }
}
predProb(args)
#predProb(c(0, 0.1, 0, 0.5))

#----------------------------------------------------------------------------------------------

# Prob 4: Simplified ridge regression
args <- commandArgs(trailingOnly = TRUE)
fastRidgeReg <- function(l_st){
  x_con <- file(l_st[1], "rb")
  y_con <- file(l_st[2], "rb")
  x_dim <- readBin(x_con, integer(), n=2)
  y_dim <- readBin(y_con, integer(), n=2)
  x_mat <- matrix(readBin(x_con, numeric(), n=(x_dim[1]*x_dim[2])),
                  nrow=x_dim[1], ncol=x_dim[2])
  y_mat <- matrix(readBin(y_con, numeric(), n=(y_dim[1]*y_dim[2])),
                  nrow=y_dim[1], ncol=y_dim[2])
  close.connection(x_con)
  close.connection(y_con)
  
  
  lambda <- as.numeric(l_st[3])
  stopifnot(x_dim[1]==y_dim[1])
  p <- x_dim[2]
  
  A <- crossprod(x_mat) + lambda*diag(p)
  b <- crossprod(x_mat, y_mat)
  U <- chol(A)
  
  z <- forwardsolve(U, b,
                    upper.tri=TRUE, transpose=TRUE)
  b_hat <- backsolve(U, z)
  
  b_compare <- floor(b_hat) + 0.5
  for(each in 1:length(b_hat)){
    if(b_hat[each] > b_compare[each]){
      beta <- ceiling(b_hat[each])
    } else{
      beta <- floor(b_hat[each])
    }
    if(beta != 0){
      cat(each, formatC(beta,digits=8,flag="-"), "\n")
    }
  }

}
fastRidgeReg(args)
#setwd("~/2020/Biostats 615")
#fastRidgeReg(c("test1.X", "test1.Y", 5000))