setwd("~/2020/Biostats 615/hw4")

# Prob 1: Non Linear Reg with Secant ----------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)

secant<-function(f, x0, x1, tol=1e-10, max_iter=1000){
  convergence = 1
  f0 = f(x0)
  f1 = f(x1)
  if(abs(f0 - f1) < tol){
    warning("Expect a huge jump!")
    break
  }
  x12<- -f1/(f1 - f0)*(x1 - x0)
  x2 <- x1 + x12
  for(iter in 1:max_iter){
    if(abs(x12)< tol){
      convergence = 0
      break
    }
    f0 <- f1
    x1 <- x2
    f1 <- f(x2)
    f01<- f1-f0
    if(abs(f01)< tol){
      warning("Expect a huge jump!")
      break
    }
    x12 <- -f1/f01*x12
    x2<- x1 + x12
  }
  return(list(root=x2, f_root = f(x2), iter = iter, convergence=convergence))
}

f <- function(a, x=data$X, y=data$Y, z=data$Z){
  f_sum <- 0
  for(i in 1:length(x)){
    f_seg <- (x[i]-2*a*(z[i]*z[i]))
    f_seg <- f_seg*(y[i]-(exp(a*x[i]-a^2*(z[i]*z[i]))/(1+exp(a*x[i]-a^2*(z[i]*z[i])))))
    f_sum <- f_sum + f_seg
  }
  return(f_sum)
} 

data <- read.csv(args[1])

start <- seq(-5,5,0.1)[which.max(f(seq(-5,5,0.1)))]
a0 <- start
a1 <- -1*start
output <- secant(f, a0, a1)$root

cat(formatC(output, digits=5, flag="-"), "\n")

# Prob 2: Fit Neural network -----------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)

## Nelder.Mead from Jian Kang 615 Course Notes ##
Nelder.Mead <- function(f, x0, tol = 1e-10, max_iter = 1000,...){
  #dimension of the simplex
  d <- length(x0)
  cout <- 0
  
  #set d+1 simplex points
  X <- matrix(x0,nrow=d,ncol=d+1)
  #create a simplex 
  X[,-(d+1)] <- X[,-(d+1)] + diag(d)
  
  #evaluate function
  Y <- apply(X,2,f,...)
  
  idx_max <- NULL; idx_min <- NULL; idx_2ndmax <- NULL
  mid_point <- NULL; tru_line <- NULL
  
  #Update the extremes 
  update.extremes <- function(){
    if(Y[1] > Y[2]){
      idx_max <<- 1; idx_min <<- 2; idx_2ndmax <<- 2
    } else{
      idx_max <<- 2; idx_2ndmax <<- 1; idx_min <<- 1
    }
    if(d>1){
      for(i in 3:(d+1)){
        if(Y[i] <= Y[idx_min]){
          idx_min <<- i
        } else if(Y[i] > Y[idx_max]){
          idx_2ndmax <<- idx_max; idx_max <<- i
        } else if(Y[i] > Y[idx_2ndmax]){
          idx_2ndmax <<- i
        }
      } 
    }
  }
  
  
  #Update the mid-point and the tru-line 
  update.mid.point <- function(){
    mid_point <<- apply(X[,-idx_max,drop=FALSE],1,mean)
    tru_line <<- X[,idx_max] - mid_point
  }
  
  #Update the next point
  update.next.point <- function(step_scale){
    
    next_point <- mid_point + tru_line*step_scale
    Y_next <- f(next_point,...)
    if(Y_next < Y[idx_max]){
      X[,idx_max] <<- next_point
      Y[idx_max] <<- Y_next
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
  
  #contract simplex
  contract.simplex <- function(){
    X[,-idx_min] <<- 0.5*(X[,-idx_min] + X[,idx_min])
    Y[-idx_min] <<- apply(X[,-idx_min],2,f,...)
  }
  
  convergence = 1
  #the whole procedure
  for(iter in 1:max_iter){
    update.extremes()
    #determine the direction for update
    update.mid.point()
    if(abs(Y[idx_max]-Y[idx_min]) <= tol*(abs(Y[idx_max])  + abs(Y[idx_min]))){
      convergence = 0
      break
    }
    #Refelection 
    update.next.point(-1.0)
    if(Y[idx_max] < Y[idx_min]){
      #Expansion
      update.next.point(-2.0)
    } else if(Y[idx_max] >= Y[idx_2ndmax]){
      #One direction contraction
      if(!update.next.point(0.5)){
        #all contraction
        contract.simplex()
      }
    }
  }
  
  return(list(xmin=X[,idx_min],fmin=Y[idx_min],
              convergence=convergence,iter=iter))
}
## Nelder.Mead from Jian Kang 615 Course Notes ##

loss_f <- function(a, x=data$X, y=data$Y, p=p_val){
  count <<- count + 1
  a0 <- a[1]
  a_mat <- a[2:length(a)]
  dim(a_mat) <- c(p, 3)
  a1 <- a_mat[,1]
  a2 <- a_mat[,2]
  a3 <- a_mat[,3]
  n <- length(x)
  f_a <- rep(0, n)
  for(i in 1:n){
    relu_val <- a2+a3*x[i]
    relu_sum <- ifelse(relu_val > 0, a1*relu_val, 0)
    relu_sum <- sum(relu_sum)
    f_a[i] <- y[i] - a0 - relu_sum
  }
  f_a <- mean(f_a^2)
  return(f_a)
}

data <- read.csv(args[1])
p_val <- as.integer(args[2])

x0 <- rep(0, 3*p_val+1)
count <- 0
output <- Nelder.Mead(loss_f, x0, tol=1e-5, max_iter=10000)

cat(formatC(output$fmin, digits=5, flag="-"), "\n")
cat(formatC(count,flag="-"), "\n")

# Prob 3: Rejection sampling ------------------------------------------------------------------------

args = commandArgs(trailingOnly = TRUE)
ar.example <- function(n, a, b){
  Y <- rbeta(n, a, b)
  f <- function(x, a, b){
    return(exp(-x^2)*x^(a-1)*(1-x)^(b-1))
  }
  g <- function(x, a, b){
    return(x^(a-1)*(1-x)^(b-1))
  }
  M <- 1
  U <- runif(n,max=M*g(Y, a, b))
  idx = which(U < f(Y, a, b))
  return(list(Y=Y,U=U,idx=idx,g=g,f=f,M=M,R=length(idx)/n))
}

a <- as.numeric(args[1])
b <- as.numeric(args[2])

output <- ar.example(5000000, a, b)
output2 <- quantile(output$Y[output$idx], probs = c(0.01, 0.25, 0.5, 0.75, 0.99))

cat(formatC(output$R,digits=1, flag="-"), "\n")
cat(formatC(output2,digits=1, flag="-"), "\n")

# Prob 4: MVN Sampling ----------------------------------------------

args = commandArgs(trailingOnly = TRUE)

rmvnorm_chol <- function(n,mu=rep(0,nrow(Sigma)),Sigma){
  p = length(mu)
  Z <- matrix(rnorm(n*p),nrow=p,ncol=n)
  U <- chol(Sigma)
  X <- mu + crossprod(U, Z)
  return(t(X))
}

sig_f <- function(x, y, p, ro){
  return(exp(p*log(ro)*abs((x-y)/p)^1.99-abs(cos((x-1)/p))-abs(cos((y-1)/p))))
}
make_sigma <- function(p, ro){
  obj <- outer(seq(1, p), seq(1, p), function(x, y) sig_f(x=x, y=y, p=p, ro=ro))
  return(obj)
}

e_max <- function(x_mat){
  max_v <- apply(x_mat, 1, max)
  return(mean(max_v))
}

e_calc <- function(x_mat){
  x_mat <- x_mat*x_mat
  calc_v <- apply(x_mat, 1, sum)
  calc_v <- sqrt(calc_v)
  return(mean(calc_v))
}

prop_ro <- function(x_mat, ro){
  mult_v <- x_mat[, 1]*x_mat[, 2]
  mult_v <- ifelse(mult_v > 0.5*ro, 1, 0)
  return(sum(mult_v)/length(mult_v))
}

p <- as.integer(args[1])
ro <- as.numeric(args[2])

samples <- rmvnorm_chol(100000, Sigma=make_sigma(p, ro))
output1 <- e_max(samples)
output2 <- e_calc(samples)
output3 <- prop_ro(samples, ro)

cat(formatC(output1,digits=1, flag="-"), "\n")
cat(formatC(output2,digits=1, flag="-"), "\n")
cat(formatC(output3,digits=1, flag="-"), "\n")
