
## Set up libraries--------------------------------------------------------
setwd("~/2020/Stats 504/student")
library(dplyr)
library(tidyr)
library(gbm)

## Data cleaning-----------------------------------------------------------
data <- read.csv("student-mat.csv", sep=";")
data <- data %>%
  select(-G1, -G2) %>%
  filter(G3 > 0)
data$absent <- ifelse(data$absences >= 3, "Treatment", "Control")
data$absent <- as.factor(data$absent)
data$macol<-ifelse(data$Medu == 4, 1, 0)
data$macol <- as.factor(data$macol)
data$pacol<-ifelse(data$Fedu == 4, 1, 0)
data$pacol <- as.factor(data$pacol)
data$close <- ifelse(data$traveltime <= 1 , 1, 0)
data$close <- as.factor(data$close)
data$studytime <-ifelse(data$studytime <= 2 , 1, 0)
data$studytime <- as.factor(data$studytime)

data <- data %>%
  select(school, sex, age, address, famsize, Pstatus, schoolsup,
         famsup, activities, nursery, higher, internet, romantic,
         famrel, freetime, goout, Dalc, Walc, health, macol, pacol,
         close, G3, absent)

## Creating baseline table-------------------------------------------------
baseline <- matrix(NA, ncol=4, nrow=23)
for(i in 1:(ncol(data)-1)){
  if(is.factor(data[,i]) == TRUE){
    temp <- table(data[,i], data$absent)
    temp <- rbind(temp, temp[1,]+temp[2,])
    temp <- cbind(temp[,1]+temp[,2], temp)
    temp <- as.vector(temp[1,]*100/temp[3,])
    temp <- paste0(sprintf("%0.1f", temp), "%")
    p_val <- chisq.test(data[,i], data$absent)$p.value
    temp <- c(temp, sprintf("%0.3f", p_val))
  } 
  else{
    temp_c <- data[,i][data$absent=="Control"]
    temp_t <- data[,i][data$absent=="Treatment"]
    temp_mean <- c(mean(data[,i]), mean(temp_c),
                   mean(temp_t))
    temp_sd <- c(sd(data[,i]), sd(temp_c),
                  sd(temp_t))
    temp <- sprintf("%0.1f(%0.2f)", temp_mean, temp_sd)
    p_val <- chisq.test(data[,i], data$absent)$p.value
    temp <- c(temp, sprintf("%0.3f", p_val))
  }
  baseline[i,] <- temp
}
baseline <- data.frame("Variable" = colnames(data)[1:23],
                       "Total (357)" = baseline[,1],
                       "Control (145)" = baseline[,2],
                       "Treatment (212)" = baseline[,3],
                       "p-value" = baseline[,4])
knitr::kable(baseline, caption="Baseline Characteristics")

## Logistic Weights--------------------------------------------------------
prop.mod <- glm(absent ~ school + sex + age + address + 
                  famsize + Pstatus  + schoolsup + famsup + 
                  activities + nursery + higher + internet + 
                  romantic + famrel + freetime + goout + 
                  Dalc + Walc + health, 
                data=data, family=binomial())
summary(prop.mod)

treat_prop <- predict(prop.mod, newdata = data[,-23],
                      type = 'response')
data$logistic_prop <- treat_prop
data$logistic_weight <- ifelse(data$absent == "Treatment",
                               1/(1-treat_prop), 1/treat_prop)
boxplot(treat_prop ~ absent, data=data)
glm1 <- glm(G3 ~ absent, weights = logistic_weight, data = data)
summary(glm1)

## Boosting Weights--------------------------------------------------------
data$absent <- ifelse(data$absent=="Treatment", 1, 0)
boosted.mod <- ps(absent ~ school + sex + age + address + 
                    famsize + Pstatus  + schoolsup + famsup + 
                    activities + nursery + higher + internet + 
                    romantic + famrel + freetime + goout + 
                    Dalc + Walc + health, 
                  data=data,
                  estimand = "ATE",
                  n.trees = 5000, 
                  interaction.depth=2, 
                  perm.test.iters=0, 
                  verbose=FALSE, 
                  stop.method = c("es.mean"))
summary(boosted.mod)
summary(boosted.mod$gbm.obj,
        n.trees=boosted.mod$desc$es.mean.ATE$n.trees, 
        plot=FALSE)
data$boosted <- get.weights(boosted.mod)
plot(boosted.mod, plots=3)
bal.table(boosted.mod)
glm2 <- glm(G3 ~ absent, weights = boosted, data = data)
summary(glm2)