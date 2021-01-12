setwd("~/2020/Biostats 615/hw3")

# Prob 1: Solve AX + XB = C -------------------------------------------------------------

## Load data
load("solveMatrixEquation.test1.RData")
load("solveMatrixEquation.test2.RData")

args = commandArgs(trailingOnly = TRUE)
load(args)

## Set up relevant numbers
require(Matrix)
dims <- dim(A)
n <- dims[1]
  
## Make sparse matrices
idx <- which(A != 0, arr.ind=TRUE)
a <- A[A!=0]
A <- sparseMatrix(i=idx[,1], j=idx[,2],
                      x=a, dims=dims)
idx <- which(B != 0, arr.ind=TRUE)
b <- B[B!=0]
B <- sparseMatrix(i=idx[,1], j=idx[,2],
                   x=b, dims=dims)
idx <- which(C != 0, arr.ind=TRUE)
c <- C[C!=0]
C <- sparseMatrix(i=idx[,1], j=idx[,2],
                  x=c, dims=dims)
  
## Solve for x
L <- kronecker(diag(n), A)
R <- kronecker(t(B), diag(n))
G <- L+R
col_C <- as.vector(C)

left <- crossprod(G)
right <- crossprod(G, col_C)
U <- chol(left)
z <- forwardsolve(U, right,
                  upper.tri=TRUE, transpose=TRUE)
x <- backsolve(U, z)
x <- round(x)
X <- matrix(x, nrow=n)
  
## Get indices
idx <- which(X != 0, arr.ind=TRUE)
num <- X[X!=0]
output <- cbind(idx, num)

for(i in 1:length(num)){
  cat(formatC(output[i,],flag="-"), "\n")
}

# Prob 2: Piecewise linear interpolation -------------------------------------------------------------

## Load data
load("approxErrors.test1.RData")
load("approxErrors.test2.RData")

args = commandArgs(trailingOnly = TRUE)
load(args)

## Function definitions
eval.poly <- function(x, coefs){
  y <- rep(0,length(x))
  for(i in length(coefs):1L){
    y <- coefs[i] + x*y
  }
  return(y)
}

## Find coefs for fz_hat
n <- length(X)
od_x <- order(X)
y <- Y[od_x]
x <- X[od_x]
m = diff(y)/diff(x)
b = y[-1] - m*x[-1]
coef <- cbind(x=x[-1],b,m)

## Evaluate fz_hat
n <- nrow(coef)
z_bound = c(-Inf, coef[-n,"x"], Inf)
fz_hat <- rep(0, length=length(Z))
for(i in 1:n){
  idx <- which((Z <= z_bound[i+1]) & (Z > z_bound[i]))
  fz_hat[idx] <- eval.poly(Z[idx], coef[i, c("b","m")])
}


## Find coefficients for fz
X_mat <- cbind(1, X, X^2)
A <- crossprod(X_mat)
b <- crossprod(X_mat, Y)
b_hat <- solve(A,b)

## Evaluate fz
fz <- eval.poly(Z, b_hat)

## Evaluate errors
err <- sum((fz_hat-fz)^2)/length(Z)
cat(formatC(err, digits=5,flag =" -"))


# Prob 3: Nearest Neighbor interpolation -------------------------------------------------------------

## Load data
load("nearestNeighborInterpolation.test1.RData")
load("nearestNeighborInterpolation.test2.RData")

args = commandArgs(trailingOnly = TRUE)
load(args)

## Useful values
n <- dim(X)[1]
p <- dim(X)[2]
m <- dim(Z)[1]

# Create distance matrix
A <- (X*X) %*% (matrix(1, nrow=p, ncol=m))
B <- -2*(X %*% t(Z))
C <- rep(1, n) %*% t((Z*Z) %*% rep(1, p))
dist_mat <- A+B+C

gz_hat <- rep(0, m)
for(i in 1:m){
  ## find p+1 closest Xi's
  idx <- order(dist_mat[, i])[1:(p+1)]
  close <- cbind(1, X[idx, ])
  
  ## find bhat_i
  A <- crossprod(close)
  b <- crossprod(close, Y[idx])
  U <- chol(A)
  z <- forwardsolve(U, b,
                    upper.tri=TRUE, transpose=TRUE)
  bhat_i <- backsolve(U, z)
  
  ## solve for gz_hat
  gz_hat[i] <- sum(c(1, Z[i, ])*bhat_i)
}


## Calculate R
R <- cor(g_Z, gz_hat)
cat(formatC(R, digits=5,flag =" -"))
