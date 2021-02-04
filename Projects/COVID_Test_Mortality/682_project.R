# Set up libraries -----------------------------------------------------------
library(dplyr)
library(tidyr)
library(anytime)
library(R2jags)

# Data cleaning -------------------------------------------------------------

## Resulting data.frame:
## state = 2 letter state ID
## month = numeric month of observation
## days = days since first positive case in state
## logpos_1 = log of cumulative positives for week before
## logpos_2 = log of cumulative positives for second week before
## logpos_3 = log of cumulative positives for third week before

dat <- read.csv("https://api.covidtracking.com/v1/states/daily.csv")
data1 <- dat %>%
  transmute(date=anydate(date),
            state,
            positive=positiveIncrease,
            death=deathIncrease) %>%
  filter(positive >= 0, death >= 0) %>%
  group_by(state) %>%
  mutate(first=min(date),
         month=as.numeric(format(date, "%m")),
         days=as.numeric(date-first)) %>%
  select(state, month, days, positive, death) %>%
  arrange(state, days) %>%
  mutate(cumpos= cumsum(positive)) %>%
  filter(cumpos > 0) %>%
  transmute(month,
            days=days-min(days),
            positive, death,
            pos_1=0,
            pos_2=0,
            pos_3=0)
  
for(i in 1:nrow(data1)){
  state <- data1$state[i]
  days <- data1$days[i]
  pos_mat <- rep(0, 21)
  if(days >= 20){
    for(j in 0:20){
      idx <- which(data1$state==state & data1$days==(days-j))
      if(length(idx)>0){
        pos_mat[j+1] <- data1$positive[idx]
      }
    }
  }
  pos_mat <- matrix(pos_mat, ncol=3)
  pos_mat <- colSums(pos_mat)
  data1$pos_1[i] <- pos_mat[1]
  data1$pos_2[i] <- pos_mat[2]
  data1$pos_3[i] <- pos_mat[3]
}

data1 <- data1 %>%
  ungroup() %>%
  filter(days >= 20) %>%
  transmute(death, state, month, days,
            logpos_1 = log(pos_1),
            logpos_2 = log(pos_2),
            logpos_3 = log(pos_3))

# (STILL WORKING) Bayesian Hierarchical Poisson GLM -------------------------
deaths <- data1$death
logpos_1 <- data1$logpos_1
logpos_2 <- data1$logpos_2
logpos_3 <- data1$logpos_3
state <- as.integer(as.factor(data1$state))
N_observ <- length(deaths)
N_state <- unique(state)
time <- data1$days
covid.data = list("deaths", "logpos_1", "logpos_2", "logpos_3", "N_observ",
                  "state", "N_state", "time")

set.seed(1998)
covid.init = function(){
  list("beta[1]"=0,"beta[2]"=0, "beta[3]"=0,
       "t_beta[1]"=0, "t_beta[2]"=0, "t_beta[3]"=0, "t_beta[4]"=0,
       "state_effect[1]"=0, "state_effect[2]"=0, "state_effect[3]"=0, "state_effect[5]"=0,
       "state_effect[6]"=0, "state_effect[7]"=0, "state_effect[8]"=0, "state_effect[9]"=0,
       "state_effect[10]"=0, "state_effect[11]"=0, "state_effect[12]"=0, "state_effect[13]"=0,
       "state_effect[14]"=0, "state_effect[15]"=0, "state_effect[16]"=0, "state_effect[17]"=0,
       "state_effect[18]"=0, "state_effect[19]"=0, "state_effect[20]"=0, "state_effect[21]"=0,
       "state_effect[22]"=0, "state_effect[23]"=0, "state_effect[24]"=0, "state_effect[25]"=0,
       "state_effect[26]"=0, "state_effect[27]"=0,"state_effect[28]"=0, "state_effect[29]"=0,
       "state_effect[30]"=0, "state_effect[31]"=0, "state_effect[32]"=0, "state_effect[33]"=0,
       "state_effect[34]"=0, "state_effect[35]"=0, "state_effect[36]"=0, "state_effect[37]"=0,
       "state_effect[38]"=0, "state_effect[39]"=0, "state_effect[40]"=0, "state_effect[41]"=0,
       "state_effect[42]"=0, "state_effect[43]"=0, "state_effect[44]"=0, "state_effect[45]"=0,
       "state_effect[46]"=0, "state_effect[47]"=0, "state_effect[48]"=0, "state_effect[49]"=0,
       "state_effect[50]"=0, "state_effect[51]"=0, "state_effect[52]"=0, "state_effect[53]"=0,
       "state_effect[54]"=0, "state_effect[55]"=0,"state_effect[56]"=0)
}
covid.params = c("beta", "t_beta", "state_effect")

## Operation Get it to Work
covid.model = function(){
  for(i in 1:N_observ){
    deaths[i] ~ dpois(lambda[i])
    log(lambda[i]) = beta[1]*logpos_1[i] + beta[2]*logpos_2[i] + beta[3]*logpos_3[i] +
      t_beta[1] + t_beta[2]*time[i] + t_beta[3]*pow(time[i],2) + t_beta[4]*pow(time[i],3) +
      state_effect[state[i]]
  }
  for(j in N_state){
    state_effect[j] ~ dnorm(0, tau)
  }
  for(k in 1:4){
    beta.mu[k] ~ dnorm(0, 100)
    beta.tau[k] ~ dgamma(0.01, 10)
    t_beta[k] ~ dnorm(beta.mu[k], beta.tau[k])
  }
  beta[1] ~ dunif(-1E3, 1E3)
  beta[2] ~ dunif(-1E3, 1E3)
  beta[3] ~ dunif(-1E3, 1E3)
  tau ~ dgamma(1E-5, 1E-6)
}

covid.out = jags(data=covid.data,
                 parameters.to.save=covid.params,
                 n.chains=1,n.iter=6000,n.burnin=1000,
                 model.file=covid.model,init=covid.init,n.thin=1)


# summary
covid.out
# mixing
traceplot(covid.out)
# complete posterior distributions with traceplots
covid.mcmc <- as.mcmc(covid.out)
plot(covid.mcmc)
gelman.diag(covid.mcmc)

