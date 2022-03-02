############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Physical/")

# Pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# importing data
p_load(foreign,readxl,data.table)
#
phys.cong.d <- read.dta("/Users/hectorbahamonde/research/Physical/data/phys_cong.dta") 
dat.temp = phys.cong.d

########################################################
# p_load(rio,tibble)
# isco.d <- rio::import("https://raw.githubusercontent.com/hbahamonde/Physical/main/data/isco_data.csv")
# p_load(dplyr,tidyverse)
# isco.d = isco.d %>% 
#  group_by(ISCO_code) %>% 
#  mutate(ISCO_job_desc_collaps = paste0(ISCO_job_description, collapse = " ")) %>% 
#  distinct(ISCO_code, ISCO_job_desc_collaps)
# isco.d$ISCO_code = ifelse(isco.d$ISCO_code<1000,0, isco.d$ISCO_code)
# isco.d$ISCO_group = as.factor(gsub("(^\\d{1}).*", "\\1", isco.d$ISCO_code))



# isco.d$ISCO_group = recode_factor(isco.d$ISCO_group, 
#                                  `0` = "Army", 
#                                  `1` = "Managers", 
#                                  `2` = "Professional", 
#                                  `3` = "Technicians",
#                                  `4` = "Clerical",
#                                  `5` = "Service",
#                                  `6` = "Skilled Agricultural",
#                                  `7` = "Craft",
#                                  `8` = "Machine Operators",
#                                  `9` = "Elementary Occupations")
# https://en.wikipedia.org/wiki/International_Standard_Classification_of_Occupations
# http://www.ilo.org/public/english/bureau/stat/isco/docs/index08-draft.xlsx
# phys.cong.d = merge(phys.cong.d,isco.d, by = "ISCO_code")
########################################################


#
id.1.d <- read_excel("/Users/hectorbahamonde/research/Physical/data/id.xls", sheet = 1)
id.2.d <- read_excel("/Users/hectorbahamonde/research/Physical/data/id.xls", sheet = 2)
id.3.d <- read_excel("/Users/hectorbahamonde/research/Physical/data/id.xls", sheet = 3)
id.4.d <- read_excel("/Users/hectorbahamonde/research/Physical/data/id.xls", sheet = 4)
id.5.d <- read_excel("/Users/hectorbahamonde/research/Physical/data/id.xls", sheet = 5)
id.6.d <- read_excel("/Users/hectorbahamonde/research/Physical/data/id.xls", sheet = 6)
id.d = data.frame(rbind(id.1.d, id.2.d, id.3.d, id.4.d, id.5.d, id.6.d))


#
p_load(rio,tibble)
electoral.d <- rio::import("https://seafile.utu.fi/f/295e920af4084e0c8102/?dl=1")
# electoral.d = fread("/Users/hectorbahamonde/research/Physical/data/electoral_data.csv")
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

# 
dat$municipality = ifelse(is.na(dat$municipality.x), dat$municipality.y, dat$municipality.y)

# party ideology
dat$ideology = ifelse(
  dat$party=="KESK", "Center", ifelse(
    dat$party=="KD", "Center-Right", ifelse(
      dat$party=="KOK", "Center-Right", ifelse(
        dat$party=="PIR", "Center-Right", ifelse(
          dat$party=="PS","Right", ifelse(
            dat$party=="RKP","Center", ifelse(
              dat$party=="SDP", "Center-Left", ifelse(
                dat$party=="SKP", "Left", ifelse(
                  dat$party=="VAS", "Left", ifelse(
                    dat$party=="VIHR", "Center", NA
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
## recode
dat$ideology <- ordered(dat$ideology, levels = c("Left", "Center-Left", "Center", "Center-Right", "Right"))

# 
p_load(dplyr)
dat = dat %>% dplyr::select(id,
                            city,
                            municipality,
                            municipality.x,
                            municipality.y,
                            firstname,
                            lastname,
                            gender,
                            age,
                            party,
                            ideology,
                            candidate.number,
                            turnout,
                            occupation_phys_cong_data,
                            occup.elect.off,
                            ISCO_code,
                            everything()
                            )
# table(dat$municipality.y==dat$municipality) # municipalities match 100%
## I was getting duplicated municipality columns because of the NA's. Delete redundant columns.
dat = dat %>% select(c(-municipality.x,-municipality.y))


## I was getting duplicated rows because the occupation character vector was different. Will select distinct based on the evaluation columns.
p_load(dplyr)
dat = dat %>% dplyr::distinct(candidate.number,ideology,age,masculinity,attractiveness,femininity, .keep_all = TRUE)

# save as STATA file
p_load(foreign)
dat.stata = subset(dat, select = -c(occup.elect.off,occupation_phys_cong_data))
write.dta(dat.stata, paste0(getwd(),"/data.dta",""))
# load Stata DO file with code below
p_load(RStata)
options("RStata.StataPath" = "/Applications/Stata/StataIC.app/Contents/MacOS/StataIC") 
# this will cause an error, but it's just because it's calling the app, not the Terminal. I couldn't get to open the Terminal instead.
## https://github.com/lbraglia/RStata/issues/11
## I also put a ticket.
options("RStata.StataVersion" = 15)
stata("Occupation_Conv.do")
#### OR ALTERNATIVELY GO TO STATA
dat <- read.dta(paste0(getwd(),"/dat.dta",""))


# recode 
dat$esec.r = recode_factor(dat$esec, 
                           "Large employers, higher mgrs/professionals" = "Upper Class", 
                           "Lower mgrs/professionals, higher supervisory/technicians" = "Upper Class", 
                           # Intermediate
                           "Intermediate occupations" = "Middle Class", 
                           "Small employers and self-employed (non-agriculture)" = "Middle Class",
                           "Small employers and self-employed (agriculture)" = "Middle Class",
                           "Lower supervisors and technicians" = "Middle Class",
                           # Working Class
                           "Lower sales and service" = "Working Class",
                           "Lower technical" = "Working Class",
                           "Routine" = "Working Class"
)


############################## 
# Descriptive Plots
############################## 

p_load(ggplot2,gridExtra)

# Attractiveness
##
d.p1.a = ggplot(dat, aes(x = attractiveness, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "bottom")
##
d.p2.a = ggplot(dat, aes(x = attractiveness, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
d.p3.a = ggplot(dat, aes(x = attractiveness, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
grid.arrange(d.p1.a, d.p2.a, d.p3.a, nrow = 1, ncol= 3)


# Physical-Occupation Congruence
##
d.p1.p = ggplot(dat, aes(x = phys_occ_cong, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "bottom")
##
d.p2.p = ggplot(dat, aes(x = phys_occ_cong, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
d.p3.p = ggplot(dat, aes(x = phys_occ_cong, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
grid.arrange(d.p1.p, d.p2.p, d.p3.p, nrow = 1, ncol= 3)


# Masculinity
##
d.p1.m = ggplot(dat, aes(x = masculinity, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "bottom")
##
d.p2.m = ggplot(dat, aes(x = masculinity, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
d.p3.m = ggplot(dat, aes(x = masculinity, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
grid.arrange(d.p1.m, d.p2.m, d.p3.m, nrow = 1, ncol= 3)


# Femininity
##
d.p1.f = ggplot(dat, aes(x = femininity, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "bottom")
##
d.p2.f = ggplot(dat, aes(x = femininity, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
d.p3.f = ggplot(dat, aes(x = femininity, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom")
##
grid.arrange(d.p1.f, d.p2.f, d.p3.f, nrow = 1, ncol= 3)


############################## 
# Models
############################## 
options(scipen=999)

# base models
m0 = glm(turnout ~ phys_occ_cong*esec.r + party + age + city + city, family="poisson", data=dat)
m1.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + city, family="poisson", data=dat[dat$gender=="Man",])
m1.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + city, family="poisson", data=dat[dat$gender=="Woman",])


# robustness checks
m2 = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + city, family="poisson", data=dat)
m3 = glm(turnout ~ phys_occ_cong*esec.r + party + age + masculinity + city, family="poisson", data=dat)
m4 = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + femininity + city, family="poisson", data=dat)

m2.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + city, family="poisson", data=dat[dat$gender=="Man",])
m2.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + city, family="poisson", data=dat[dat$gender=="Woman",])

m3.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + masculinity + city, family="poisson", data=dat[dat$gender=="Man",])
m3.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + femininity + city, family="poisson", data=dat[dat$gender=="Woman",])

m4.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + masculinity + city, family="poisson", data=dat[dat$gender=="Man",])
m4.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + femininity + city, family="poisson", data=dat[dat$gender=="Woman",])



p_load(texreg)
screenreg( # screenreg texreg
  list(m0, m1.m, m1.w, m2, m3, m4, m2.m, m2.w, m3.m, m3.w, m4.m, m4.w),
  custom.model.names = c("Full", "Man", "Woman", "Full","Full","Full",  "Man", "Woman", "Man", "Woman", "Man", "Woman"),
  #custom.coef.names = NULL,
  omit.coef = "city",
  #custom.coef.names = c("Intercept", "Vote Share (%)", "Points Accumulated (delta)", "Ideological Distance", "Party Budget", "Pivotal Voter"),
  # custom.header = list( "Poisson" = 1),
  stars = c(0.001, 0.01, 0.05, 0.1),
  include.adjrs = FALSE,
  symbol = "\\cdot",
  label = "reg:t",
  caption = "Statistical Model (OLS): Amount of Vote-Buying Offer.",
  float.pos="H",
  use.packages = FALSE,
  threeparttable = TRUE,
  custom.note = "\\item %stars. \\item City fixed effects omitted. Dependent variable is Turnout. Functional form is Poisson regression for all models."
)


p_load(sjPlot)

p1 = plot_model(m0, type = "int", show.legend = F, title = "Combined Data") + theme_sjplot()
p2 = plot_model(m1.m, type = "int", show.legend = TRUE, title = "Man Data") + theme_sjplot() + theme(legend.position = "bottom")
p3 = plot_model(m1.w, type = "int", show.legend = F, title = "Woman Data") + theme_sjplot()

p_load(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1, ncol= 3)






