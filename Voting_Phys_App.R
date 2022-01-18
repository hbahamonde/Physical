############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Voting_Physical_Appearance/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# importing data
p_load(foreign,readxl,data.table)
#
phys.cong.d <- read.dta("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/phys_cong.dta") 
#
id.1.d <- read_excel("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/id.xls", sheet = 1)
id.2.d <- read_excel("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/id.xls", sheet = 2)
id.3.d <- read_excel("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/id.xls", sheet = 3)
id.4.d <- read_excel("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/id.xls", sheet = 4)
id.5.d <- read_excel("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/id.xls", sheet = 5)
id.6.d <- read_excel("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/id.xls", sheet = 6)
id.d = data.frame(rbind(id.1.d, id.2.d, id.3.d, id.4.d, id.5.d, id.6.d))
#
p_load(rio,tibble)
electoral.d <- rio::import("https://seafile.utu.fi/f/295e920af4084e0c8102/?dl=1")
# electoral.d = fread("/Users/hectorbahamonde/research/Voting_Physical_Appearance/data/electoral_data.csv")
p_load(dplyr)
electoral.d = electoral.d %>% 
  group_by(municipality,party,city,firstname,lastname,occup.elect.off) %>% 
  summarise(turnout = sum(turnout))

# merging data
dat.temp = merge(phys.cong.d, id.d, by=c("id"))
dat = merge(dat.temp, electoral.d, by=c("lastname", "party", "city"), all.x = FALSE)

# recoding
dat$party = as.factor(dat$party)
dat$gender = recode(dat$gender, Mies = "Man", Nainen = "Woman")

# Models
options(scipen=999)
m1 = glm(turnout ~ phys_occ_cong + attractiveness*gender + party + city, family="poisson", data=dat)

p_load(effects)
plot(predictorEffects(m1))
