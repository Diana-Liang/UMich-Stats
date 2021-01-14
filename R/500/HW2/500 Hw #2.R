##PART A: USWAGES: WAGES~EDUC+EXPER-----------------

library(faraway)
data(uswages)

uswages$exper[uswages$exper < 0] =NA

rmodel = lm(wage ~ educ + exper, data = uswages)
summary(rmodel)

which.max(residuals(rmodel))

summary(residuals(rmodel))

fmodel = fitted(rmodel)

summary(fmodel)

cor(residuals(rmodel),fmodel)

plot(fmodel,residuals(rmodel), main = "Effect of Predicted Weekly Wages on Residuals", xlab = "Predicted Weekly Wages in $", ylab = "Residuals")


##PART B: Y=Xb+e

X = matrix(c(1,1,1,1,1,1,1,1,1,1,2,-1,3,3,2,1,0,0,-1,0,-2,-2,-2,3,3,3,0,0,0,1))
dim(X) = c(10,3)

beta = matrix(c(1,-1,2), nrow=3, ncol=1)
err = matrix(rnorm(10,0,1),nrow=10, ncol=1)
Y = X %*% beta + err

beta_est = solve(t(X) %*% X) %*% t(X) %*% Y
beta_est

t_var_b = solve(t(X) %*% X)
t_var_b

rss = sum((Y- X %*% beta_est)^2)
n = 10
p = 3
sigma_2= rss/(n-p)
sigma_2

# question 4/5
beta_val = data.frame(b1=double(),b2=double(),b3=double(), o2=double())
for(i in 1:1000){
  e = matrix(rnorm(10,0,1),nrow=10,ncol=1)
  y = X %*% beta + e
  b_est = solve(t(X) %*% X) %*% t(X) %*% y
  sig2=sum((y- X %*% b_est)^2)/(10-3)
  add = data.frame(b1=b_est[[1]], b2=b_est[[2]], b3=b_est[[3]], o2=sig2)
  beta_val = rbind(beta_val,add)
}

hist(beta_val$b1)
var(beta_val$b1)

hist(beta_val$b2)
var(beta_val$b2)

hist(beta_val$b3)
var(beta_val$b3)

hist(beta_val$o2)

# question 6
uni_beta_val = data.frame(b1=double(),b2=double(),b3=double(), o2=double())
for(i in 1:1000){
  e = matrix(runif(10,(-1*sqrt(3)),(1*sqrt(3))),nrow=10,ncol=1)
  y = X %*% beta + e
  b_est = solve(t(X) %*% X) %*% t(X) %*% y
  sig2=sum((y- X %*% b_est)^2)/(10-3)
  add = data.frame(b1=b_est[[1]], b2=b_est[[2]], b3=b_est[[3]], o2=sig2)
  uni_beta_val = rbind(uni_beta_val,add)
}

hist(uni_beta_val$b1)
var(uni_beta_val$b1)

hist(uni_beta_val$b2)
var(beta_val$b2)

hist(uni_beta_val$b3)
var(uni_beta_val$b3)

hist(uni_beta_val$o2)