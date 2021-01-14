setwd("~/2020/Stats 504/hw5")

library(haven)
library(dplyr)
library(tidyr)
library(splines)
library(lme4)
library(geepack)

dat <- read_dta("russian_data.dta")

dat <- dat %>%
  filter(J1==1) %>%
  mutate(female=as.integer(H5==2)) %>%
  select(-J1, -H5)
dat[complete.cases(dat), ]
dat <- dat %>%
  mutate(year=year-2000) %>%
  filter(age<99999996, educ<99999996, J10<99999996, J69_8A<99999996, OCCUP08<99999996) %>%
  group_by(idind) %>%
  transmute(year, psu, status, OCCUP08, educ, J10, J69_8A, female, age,
            age_mean = mean(age)/n()) %>%
  ungroup() %>%
  mutate(age_c=age-mean(age_mean)) %>%
  select(-age_mean)
dat$OCCUP08 <- as.factor(dat$OCCUP08)
dat$psu <- as.factor(dat$psu)
dat$status <- as.factor(dat$status)

mixed_all <- lmer(log(J69_8A,2) ~ C(status) + bs(year,3) + bs(age_c,3) + bs(educ,3) + female + log(J10,2)
               + (year|idind), data=dat, REML=TRUE)
summary(mixed_all)
mixed_noeduc <- lmer(log(J69_8A,2) ~ C(status) + (bs(year,3) + bs(age_c,3)) * female + log(J10,2)
                  + (year|idind), data=dat, REML=TRUE)
summary(mixed_noeduc)


gee_all <- geeglm(log(J69_8A,2) ~ C(status) + bs(year,3) + bs(age_c,3) + bs(educ,3) + female + log(J10,2),
               id=idind, waves=year, data=dat, corstr="exchangeable")
summary(gee_all)
gee_educ <- geeglm(log(J69_8A,2) ~ C(status) + (bs(year,3) + bs(age_c,3) + bs(educ,3)) * female + log(J10,2),
                  id=idind, waves=year, data=dat, corstr="exchangeable")
summary(gee_educ)
gee_noeduc <- geeglm(log(J69_8A,2) ~ C(status) + (bs(year,3) + bs(age_c,3)) * female + log(J10,2),
                  id=idind, waves=year, data=dat, corstr="exchangeable")
summary(gee_noeduc)

ols_all <- lm(log(J69_8A,2) ~ C(status) + bs(year,3) + bs(age_c,3) + bs(educ,3) + female + log(J10,2),
              data=dat)
summary(ols_all)
ols_educ <- lm(log(J69_8A,2) ~ C(status) + (bs(year,3) + bs(age_c,3) + bs(educ,3)) * female + log(J10,2),
              data=dat)
summary(ols_educ)
ols_noeduc <- lm(log(J69_8A,2) ~ C(status) + (bs(year,3) + bs(age_c,3)) * female + log(J10,2),
              data=dat)
summary(ols_noeduc)


