### Universal Analysis Function ###

## Contents:
## A: Create Data (skip if loading data)
## B: Create 2 Tables (beta_table and info_table)
## C: Universal Analysis (fill tables)
## D: Glmnet Version (creates glmnet equivalent table)
## E: Include CV (analysis of CV)
## F: Visualization (melt tables and plot)

# Load libraries
library(glmnet)

# A: Create Data -------------------------------------------------------------------

# Linear Case
mus <- floor(rnorm(50, 0, 5))
sigmas <- ceiling(abs(rnorm(50, 0, 2)))
betas_lin <- floor(rnorm(51, 0, 5))

x_lin <- matrix(nrow=50, ncol=50)
for(i in 1:50){
  x_lin[, i] <- rnorm(50, 0, sigmas[i])
}
x_lin <- apply(x_lin, 2, scale)
y_lin <- cbind(1, x_lin) %*% betas_lin

# Logistic Case
x_bin = apply(matrix(rnorm(50^2), nrow=50, ncol=50),2,scale)
betas_bin = rnorm(51, sd=4)
y_bin <- rbinom(n=nrow(x_bin), prob = logit(cbind(1,x_bin)%*%betas_bin), size = 1)

# Save the data


# B: Create 2 tables ----------------------------------------------------------------

# Beta estimate table
# make vectors
lambdas <- 1
alphas <- rep(c(0, 0.005, 0.25, 0.5, 0.75, 1), 4)
func <- rep(c("linear grad", "linear RRGG", "linear coord", "logistic coord"), each=6)
# set up data.frame
beta_table <- data.frame(alpha=alphas, func= func,
                         beta1=rep(0, 24), beta2=rep(0,24), beta3=rep(0,24), beta4=rep(0,24), beta5=rep(0,24),
                         beta6=rep(0, 24), beta7=rep(0,24), beta8=rep(0,24), beta9=rep(0,24), beta10=rep(0,24),
                         beta11=rep(0, 24), beta12=rep(0,24), beta13=rep(0,24), beta14=rep(0,24), beta15=rep(0,24),
                         beta16=rep(0, 24), beta17=rep(0,24), beta18=rep(0,24), beta19=rep(0,24), beta20=rep(0,24),
                         beta21=rep(0, 24), beta22=rep(0,24), beta23=rep(0,24), beta24=rep(0,24), beta25=rep(0,24),
                         beta26=rep(0, 24), beta27=rep(0,24), beta28=rep(0,24), beta29=rep(0,24), beta30=rep(0,24),
                         beta31=rep(0, 24), beta32=rep(0,24), beta33=rep(0,24), beta34=rep(0,24), beta35=rep(0,24),
                         beta36=rep(0, 24), beta37=rep(0,24), beta38=rep(0,24), beta39=rep(0,24), beta40=rep(0,24),
                         beta41=rep(0, 24), beta42=rep(0,24), beta43=rep(0,24), beta44=rep(0,24), beta45=rep(0,24),
                         beta46=rep(0, 24), beta47=rep(0,24), beta48=rep(0,24), beta49=rep(0,24), beta50=rep(0,24),
                         beta51=rep(0, 24))

# Other info table
info_table <- data.frame(alpha=alphas, func=func,
                         loss=rep(0, 24), ntrue=rep(0, 24), time=rep(0, 24))

# C: Universal Analysis ------------------------------------------------------------------
for(tab in 1:24){
  # get current values
  current_alpha <- info_table$alpha[tab]
  current_func <- info_table$func[tab]
  
  # run for specific function
  if(current_func == "linear grad"){
    print("grad")
    runtime <- system.time({ outcome <- grad_desc_enet(X=x_lin, y=y_lin, lambda=lambdas, alpha=current_alpha,
                                                       step_size=1E-3, tol=1E-7, max_iter=5E6) })
    print("done running")
    if(outcome$convergence == 1){
      print("Linear Grad did not converge")
      break
    }
  } else if(current_func == "linear RRGG"){
    print("rrgg")
    runtime <- system.time({ outcome <- linear_enet(X=x_lin, Y=y_lin, lambda=lambdas, my_alpha=current_alpha,
                                                    step_size=1E-3, tol=1E-7) })
    print("done running")
  } else if(current_func=="linear coord"){
    print("coord")
    runtime <- system.time({ outcome <- coord.descent(alpha=current_alpha, lambda=lambdas,
                                                      Y=y_lin, X=cbind(1, apply(x_lin, 2, scale)), maxit=100000) })
    print("done running")
    if(outcome$convergence == 1){
      print("Linear Coord did not converge")
      break
    }
  } else if(current_func=="logistic coord"){
    print("log coord")
    runtime <- system.time({ outcome <- logistic_coord_descent(y=y_bin, x=x_bin, lambda=lambdas, alpha=current_alpha,
                                                               beta0=rep(0, 51), max_iter=100000) })
    print("done running")
    if(outcome$convergence == 1){
      print("Logistic Coord did not converge")
      break
    }
  } else{ print("Do not recognize function")}
  
  # fill in beta_table
  for(j in 1:length(outcome$beta)){
    beta_table[tab, (j+2)] <- outcome$beta[j]
  }
  
  # fill in info_table
  # loss
  if(current_func %in% c("linear grad", "linear RRGG", "linear coord")){
    info_table$loss[tab] <- loss_fun(beta=outcome$beta, X=cbind(1, x_lin), Y=y_lin,
                                     alpha = current_alpha, lambda = lambdas)
  } else if(current_func == "logistic coord"){
    info_table$loss[tab] <- true_loss_fun(beta=outcome$beta, x=cbind(1, x_bin), y=y_bin,
                                          alpha=current_alpha, lambda=lambdas)
  } else{ print("Still don't recognize function")}
  # ntrue
  info_table$ntrue[tab] <- sum(ifelse(outcome$beta==0, 0, 1))
  # time
  info_table$time[tab] <- runtime[[3]]
}


# D: Glmnet Version ---------------------------------------------------------------------
glmnet_table <- data.frame(alpha=rep(c(0, 0.005, 0.25, 0.5, 0.75, 1), 2),
                           func=rep(c("linear glmnet", "logistic glmnet"), each=6), loss=rep(0, 12), ntrue=rep(0, 12),
                           beta1=rep(0, 12), beta2=rep(0,12), beta3=rep(0,12), beta4=rep(0,12), beta5=rep(0,12),
                           beta6=rep(0, 12), beta7=rep(0,12), beta8=rep(0,12), beta9=rep(0,12), beta10=rep(0,12),
                           beta11=rep(0, 12), beta12=rep(0,12), beta13=rep(0,12), beta14=rep(0,12), beta15=rep(0,12),
                           beta16=rep(0, 12), beta17=rep(0,12), beta18=rep(0,12), beta19=rep(0,12), beta20=rep(0,12),
                           beta21=rep(0, 12), beta22=rep(0,12), beta23=rep(0,12), beta24=rep(0,12), beta25=rep(0,12),
                           beta26=rep(0, 12), beta27=rep(0,12), beta28=rep(0,12), beta29=rep(0,12), beta30=rep(0,12),
                           beta31=rep(0, 12), beta32=rep(0,12), beta33=rep(0,12), beta34=rep(0,12), beta35=rep(0,12),
                           beta36=rep(0, 12), beta37=rep(0,12), beta38=rep(0,12), beta39=rep(0,12), beta40=rep(0,12),
                           beta41=rep(0, 12), beta42=rep(0,12), beta43=rep(0,12), beta44=rep(0,12), beta45=rep(0,12),
                           beta46=rep(0, 12), beta47=rep(0,12), beta48=rep(0,12), beta49=rep(0,12), beta50=rep(0,12),
                           beta51=rep(0, 12), time=rep(0,12))

for(i in 1:10){
  current_alpha <- glmnet_table$alpha[i]
  if(i < 7){
    runtime <- system.time({ outcome <- glmnet(x=x_lin, y=y_lin, family="gaussian",
                      lambda=lambdas, alpha=current_alpha) })
  } else{
    runtime <- system.time({ outcome <- glmnet(x=x_bin, y=y_bin, family="binomial",
                      lambda=lambdas, alpha=current_alpha) })
  }
  glm_b <- as.vector(outcome$beta)
  glm_b <- c(outcome$a0, glm_b)
  for(j in 1:(length(outcome$beta)+1)){
    glmnet_table[i, (j+4)] <- glm_b[j]
  } 
  if(i < 7){
    glmnet_table$loss[i] <- loss_fun(beta=glm_b, X=cbind(1, x_lin), Y=y_lin,
                                     alpha = current_alpha, lambda = lambdas)
  } else{
    glmnet_table$loss[i] <- true_loss_fun(beta=glm_b, x=cbind(1, x_bin), y=y_bin,
                                          alpha=current_alpha, lambda=lambdas)
  }
  glmnet_table$ntrue[i] <- sum(ifelse(glm_b==0, 0, 1))
  glmnet_table$time[i] <- runtime[[3]]
}


# E: Include CV ----------------------------------------------------------------

# set up data.frame
beta_cv <- data.frame(alpha=c(1, 1), func=c("linear grad", "logistic coord"),
                         beta1=rep(0, 2), beta2=rep(0,2), beta3=rep(0,2), beta4=rep(0,2), beta5=rep(0,2),
                         beta6=rep(0, 2), beta7=rep(0,2), beta8=rep(0,2), beta9=rep(0,2), beta10=rep(0,2),
                         beta11=rep(0, 2), beta12=rep(0,2), beta13=rep(0,2), beta14=rep(0,2), beta15=rep(0,2),
                         beta16=rep(0, 2), beta17=rep(0,2), beta18=rep(0,2), beta19=rep(0,2), beta20=rep(0,2),
                         beta21=rep(0, 2), beta22=rep(0,2), beta23=rep(0,2), beta24=rep(0,2), beta25=rep(0,2),
                         beta26=rep(0, 2), beta27=rep(0,2), beta28=rep(0,2), beta29=rep(0,2), beta30=rep(0,2),
                         beta31=rep(0, 2), beta32=rep(0,2), beta33=rep(0,2), beta34=rep(0,2), beta35=rep(0,2),
                         beta36=rep(0, 2), beta37=rep(0,2), beta38=rep(0,2), beta39=rep(0,2), beta40=rep(0,2),
                         beta41=rep(0, 2), beta42=rep(0,2), beta43=rep(0,2), beta44=rep(0,2), beta45=rep(0,2),
                         beta46=rep(0, 2), beta47=rep(0,2), beta48=rep(0,2), beta49=rep(0,2), beta50=rep(0,2),
                         beta51=rep(0, 2))

# Other info table
info_cv <- data.frame(alpha=c(1, 1), func=c("linear grad", "logistic coord"),
                      lambda=rep(0,2), loss=rep(0, 2), ntrue=rep(0, 2), time=rep(0, 2))

# Run CV for grad desc
runtime <- system.time({ 
  grad_lambda <- lambda_cv(X=x_lin, y=y_lin,
                         alpha=1, beta0=rep(0, 51),
                         family="gaussian", fun=grad_desc_enet,
                         folds=10, step_size=1E-3, tol=1E-6)
  outcome <- grad_desc_enet(X=x_lin, y=y_lin, lambda=grad_lambda$lambda_mse,
                                                   alpha=1, step_size=1E-3, tol=1E-6) })
for(j in 1:length(outcome$beta)){
  beta_cv[1, (j+2)] <- outcome$beta[j]
}
info_cv$loss[1] <- loss_fun(beta=outcome$beta, X=cbind(1, x_lin), Y=y_lin,
                                 alpha =1, lambda = grad_lambda$lambda_mse)
info_cv$ntrue[1] <- sum(ifelse(outcome$beta==0, 0, 1))
info_cv$time[1] <- runtime[[3]]
info_cv$lambda[1] <- grad_lambda$lambda_mse

# Run CV for logistic coord
runtime <- system.time({ 
  log_lambda <- lambda_cv(X=x_bin, y=y_bin,
                        lambda=exp(seq(-10, 0, 0.5)),
                        alpha=1, beta0=rep(0, 51),
                        family="binomial", fun=logistic_coord_descent,
                        folds=10, tol=1E-9)
  outcome <- logistic_coord_descent(y=y_bin, x=x_bin, lambda=log_lambda$lambda_mse, alpha=1,
                                                           beta0=rep(0, 51), max_iter=100000) })
for(j in 1:length(outcome$beta)){
  beta_cv[2, (j+2)] <- outcome$beta[j]
}
info_cv$loss[2] <- true_loss_fun(beta=outcome$beta, x=cbind(1, x_bin), y=y_bin,
                                 alpha=1, lambda=log_lambda$lambda_mse)
info_cv$ntrue[2] <- sum(ifelse(outcome$beta==0, 0, 1))
info_cv$time[2] <- runtime[[3]]
info_cv$lambda[2] <- log_lambda$lambda_mse

# F: Visualization -----------------------------------------------------------------------

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# melt the beta tables
melted_alg <- pivot_longer(beta_table, cols=starts_with("beta"),
                           names_to="beta", values_to="estimate")
melted_glmnet <- pivot_longer(glmnet_table[, -c(3,4,56)], cols=starts_with("beta"),
                              names_to="beta", values_to="estimate")
melted <- rbind(melted_alg, melted_glmnet)

new_melt <- melted %>%
  filter(func %in% c("linear grad", "linear RRGG", "linear coord", "linear glmnet"))
  

# make beta plot
ggplot(new_melt, aes(x=alpha, y=estimate, colour=beta)) +
  geom_point() + geom_line() +
  facet_grid(.~func) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Alpha Values") +
  ylab("Beta Estimates") +
  labs(title="Beta Estimates for Different Alphas and Functions of Elastic Net")

# combine info tables
info_glmnet <- data.frame(alpha=glmnet_table$alpha, func=glmnet_table$func,
                          loss=glmnet_table$loss, ntrue=glmnet_table$ntrue,
                          time=glmnet_table$time)
info <- rbind(info_table, info_glmnet)

# melt info table
info <- gather(info, "metric", "values", "loss":"time")

info_loss <- info %>%
  filter(metric=="loss") %>%
  select(func, alpha, values)

ggplot(info_loss, aes(x=alpha, y=values, colour=func)) +
  geom_point() + geom_line() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Alpha Values") +
  ylab("Loss") +
  labs(title="Loss for Diff Alpha and Functions")

info_ntrue <- info %>%
  filter(metric=="ntrue") %>%
  select(func, alpha, values)

ggplot(info_ntrue, aes(x=alpha, y=values, colour=func)) +
  geom_point() + geom_line() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Alpha Values") +
  ylab("# Parameters Remaining") +
  labs(title="Parameters Remaining for Diff Alpha and Functions")


info_time <- info %>%
  filter(metric=="time") %>%
  select(func, alpha, values)

ggplot(info_time, aes(x=alpha, y=values, colour=func)) +
  geom_point() + geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Alpha Values") +
  ylab("Run Time (s)") +
  labs(title="Run Time(s) for Diff Alphas and Functions")

# make info plot
#ggplot(info, aes(x=alpha, y=log(values + 1E-4), colour=metric)) +
#  geom_point() + geom_line() +
#  scale_color_manual(labels = c("Loss", "# Params", "Time (s)"), values = c("red", "green", "blue")) +
#  facet_grid(.~func) +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  xlab("Alpha Values") +
#  ylab(" ") +
#  labs(title="Metrics for Different Alphas and Functions of Elastic Net")

