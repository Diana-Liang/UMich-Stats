# A: GET DATA STARTED ------------------------------------------------------------------

library(tidyverse); library(haven); library(janitor)
library(boxr); library(knitr); library(plotly)
usethis::usethis::edit_r_environ()

boxr::box_auth(BOX_CLIENT_ID, BOX_CLIENT_SECRET)

dat <- box_search("STYH_2017_STATCOM.sav") %>%
  box_read(read_fun = read_sav) %>%
  na_if(-1) %>%
  as_factor() %>%
  mutate(Flint = factor(Flint, levels = 0:1, labels = c("Out County", "Flint")))

# B: ONLY USABLE DATA ------------------------------------------------------------------

skipvars <- c("SCID", "Q_Consent", "Q_screen", "Q1", "Q2", "Q_source", 
              "Q_source_8_TEXT", "Q83", "Q11", "Q25_6_TEXT", "Q26_C", 
              "Q30_1_TEXT", "Q99_1_TEXT", "Q32_11_TEXT", "Q38_7_TEXT", 
              "Q42_Text", "Q59_M", "Q59_Y", "Q61_4_TEXT","Q62_8_TEXT",
              "Q62_9_TEXT", "Q63_22_TEXT", "Q65_3_TEXT", "Q65_8_TEXT",
              "Q67_7_TEXT", "Q77_8_TEXT", "Q81_2_TEXT", "Flint", "Age", "wt")
cols <- colnames(dat)

fun <- data.frame(samp=1)
for(i in 1:ncol(dat)){
  if(!(cols[i] %in% skipvars)){
    fun <- cbind(fun, dat[i])
  }
}
fun <- fun %>% select(-samp)
funame <- colnames(fun)

# C: ONLY PRED AND RESP ON INTEREST ------------------------------------

keepin <- c("Q70_1", "Q70_2", "Q70_3",
            "Q3_1", "Q3_2", "Q5_1", "Q5_2",
            "Q49_1", "Q60", "Q61", "Q63", "Q66",
            "Q62_1", "Q62_11", "Q62_12", "Q62_13", "Q62_14", "Q62_15", "Q62_16", "Q62_8",
            "Q65_1", "Q65_2", "Q65_3", "Q65_4", "Q65_5", "Q65_6", "Q65_7", "Q65_8")
full <- data.frame(samp=1:855)
for(i in 1:ncol(fun)){
  if(funame[i] %in% keepin){
    full <- cbind(full, fun[i])
  }
}
fullname <- colnames(full)

# D: Create response variables ------------------------------------------------------------

temp <- full

test <- temp %>%
  mutate(b_e = as.numeric(Q70_1)-as.numeric(Q70_3),
         b_d = as.numeric(Q70_1)-as.numeric(Q70_2),
         d_e = as.numeric(Q70_2)-as.numeric(Q70_3))

# E: Create predictor variables: Race -----------------------------------------------------------

# figure out how many racial identities
race <- temp %>%
  select("Q62_1", "Q62_11", "Q62_12", "Q62_13",
         "Q62_14", "Q62_15", "Q62_16", "Q62_8") %>%
  mutate_all(as.character) %>%
  mutate_all(function(x) ifelse(x=="Yes", 1, 0)) %>%
  mutate(total = rowSums(.)) %>%
  mutate(samp = temp$samp)

# how abt putting some words to these numbers
race_list <- c("Native", "Asian", "Black", "Latinx",
               "Middle East", "Hawaiian", "White", "Multi")
for(i in 1:8){
  race[i] <- ifelse(race[i]==1, race_list[i], 0)
}

# look at the ones that are two (2)
multicult <- race %>%
  group_by(Q62_8) %>%
  filter(Q62_8 == "Multi") %>%
  arrange(total) %>%
  na_if(., 0)

# READY THE TABLES
only_tot <- race %>%
  select(total, samp)

only_race <- race %>%
  select(-total) %>%
  na_if(., 0) # %>% left_join(only_tot, by="samp")
only_race[is.na(only_race)] <- ""

# SMASH OPTION race
smash <- only_race %>%
  gather(key="category", value="race", Q62_1:Q62_8) %>%
  group_by(samp) %>%
  summarize(smash_race = paste0(race, collapse=""))

smash_freq <- smash %>%
  group_by(smash_race) %>%
  summarize(freq = n(),
            perc = n()/855) %>%
  arrange(-freq)

# ONE (1) OPTION race
one <- race %>%
  filter(total <= 1) %>%
  select(-total) %>%
  na_if(., 0)
one[is.na(one)] <- ""
one <- one %>%
  gather(key="category", value="race", Q62_1:Q62_8) %>%
  group_by(samp) %>%
  summarize(one_race = paste0(race, collapse=""))

#another_one <- race %>%
#  filter(total > 1) %>%
#  select(-total) %>%
#  na_if(., 0)
#another_one[is.na(another_one)] <- ""
#write.csv(another_one$samp, "another_one.csv")

another_one <- readr::read_csv("another_one.csv")
one <- rbind(one, another_one) %>%
  rbind(c(440, "")) %>%
  arrange(samp)

one_freq <- one %>%
  group_by(one_race) %>%
  summarize(freq = n(),
            perc = n()/855) %>%
  arrange(-freq)

# F: Create predictor variables: Employment -----------------------------------------------------------

employ <- temp %>%
  select(samp, Q65_1, "Q65_2", "Q65_3", "Q65_4",
         "Q65_5", "Q65_6", "Q65_7", "Q65_8") %>%
  mutate_if(is.factor, as.character)

# SMASH OPTION employ
smash_employ <- employ
smash_employ[is.na(smash_employ)] <- ""
smash_employ <- smash_employ %>%
  gather(key="category", value="employ", Q65_1:Q65_8) %>%
  group_by(samp) %>%
  summarize(smash_employ = paste0(employ, collapse=""))

employ_freq <- smash_employ %>%
  group_by(smash_employ) %>%
  summarize(freq = n(),
            perc = n()/855) %>%
  arrange(-freq)

# ONLY ONE (1) OPTION employ
one_employ <- employ
one_employ[is.na(one_employ)] <- ""

employ_count <- one_employ %>%
  select(-samp) %>%
  mutate_all(function(x) ifelse(x=="", 0, 1)) %>%
  mutate(total = rowSums(.),
         samp = one_employ$samp) %>%
  select(samp, total)

one_employ <- employ_count %>%
  left_join(one_employ, by="samp")

#this_one <- one_employ %>%
#  filter(total > 1) %>%
#  select(-total)
#write.csv(this_one, "this_one.csv")


