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

#### GO TO STATA and RUN DO FILE

# load Stata DO file with code below
#p_load(RStata)
#options("RStata.StataPath" = '/Applications/Stata/StataIC.app/Contents/MacOS/StataIC') 
#chooseStataBin <- '/Applications/Stata/StataIC.app/Contents/MacOS/StataIC'
#options("chooseStataBin" = '/Applications/Stata/StataIC.app/Contents/MacOS/StataIC')
# this will cause an error, but it's just because it's calling the app, not the Terminal. I couldn't get to open the Terminal instead.
## https://github.com/lbraglia/RStata/issues/11
## I also put a ticket.
## BUT ALWAYS MAKE SURE net install iscogen is installed IN STATA
# options("RStata.StataVersion" = 15)
# RStata::stata("Occupation_Conv.do")

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


save(dat, file = "dat.Rdata")



############################## 
# Descriptive Plots
############################## 

## ---- loadings:d
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/research/Physical/")

load(file = "/Users/hectorbahamonde/research/Physical/dat.Rdata")

p_load(ggplot2,cowplot)

# Attractiveness
##
d.p1.a = ggplot(subset(dat, !is.na(gender)), aes(x = attractiveness, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Attractiveness") 
d.p1.a$labels$fill <- "Candidate Gender"
d.p1.a$labels$colour <- "Candidate Gender"
##
d.p2.a = ggplot(subset(dat, !is.na(esec.r)), aes(x = attractiveness, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Attractiveness")  
d.p2.a$labels$fill <- "European Socio-Economic\nClassification"
d.p2.a$labels$colour <- "European Socio-Economic\nClassification"
##
d.p3.a = ggplot(subset(dat, !is.na(party)), aes(x = attractiveness, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Attractiveness") 
d.p3.a$labels$fill <- "Political Party"
d.p3.a$labels$colour <- "Political Party"
##
attractiveness.p = cowplot::plot_grid(
  d.p1.a, d.p2.a, d.p3.a, 
  align = "hv",
  axis = "b", 
  ncol = 3
  )
ggsave("attractiveness.pdf", plot = attractiveness.p, 
       width = 1400,
       height = 500,
       units = c("px"),
       dpi = 80
       )


# Physical-Occupation Congruence
##
d.p1.p = ggplot(subset(dat, !is.na(gender)), aes(x = phys_occ_cong, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Physical Occupation-Congruent")
d.p1.p$labels$fill <- "Candidate Gender"
d.p1.p$labels$colour <- "Candidate Gender"
##
d.p2.p = ggplot(subset(dat, !is.na(esec.r)), aes(x = phys_occ_cong, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Physical Occupation-Congruent")
d.p2.p$labels$fill <- "European Socio-Economic\nClassification"
d.p2.p$labels$colour <- "European Socio-Economic\nClassification"
##
d.p3.p = ggplot(subset(dat, !is.na(party)), aes(x = phys_occ_cong, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Physical Occupation-Congruent")
d.p3.p$labels$fill <- "Political Party"
d.p3.p$labels$colour <- "Political Party"
##
phys_occ_cong.p = cowplot::plot_grid(
  d.p1.p, d.p2.p, d.p3.p, 
  align = "hv",
  axis = "b", 
  ncol = 3
)
ggsave("phys_occ_cong.pdf", plot = phys_occ_cong.p, 
       width = 1400,
       height = 500,
       units = c("px"),
       dpi = 80
)

# Masculinity
##
d.p1.m = ggplot(subset(dat, !is.na(gender)), aes(x = masculinity, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Masculinity")
d.p1.m$labels$fill <- "Candidate Gender"
d.p1.m$labels$colour <- "Candidate Gender"
##
d.p2.m = ggplot(subset(dat, !is.na(esec.r)), aes(x = masculinity, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Masculinity")
d.p2.m$labels$fill <- "European Socio-Economic\nClassification"
d.p2.m$labels$colour <- "European Socio-Economic\nClassification"
##
d.p3.m = ggplot(subset(dat, !is.na(party)), aes(x = masculinity, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "none") + labs(y = "Density", x = "Candidate Masculinity")
d.p3.m$labels$fill <- "Political Party"
d.p3.m$labels$colour <- "Political Party"
##
masculinity.p = cowplot::plot_grid(
  d.p1.m, d.p2.m, d.p3.m, 
  align = "hv",
  axis = "b", 
  ncol = 3
  )
ggsave("masculinity.pdf", plot = masculinity.p, 
       width = 1400,
       height = 500,
       units = c("px"),
       dpi = 80
)

# Femininity
##
d.p1.f = ggplot(subset(dat, !is.na(gender)), aes(x = femininity, color = gender, fill = gender)) + geom_density(alpha = 0.5) + theme_light() + theme(legend.position = "bottom") + labs(y = "Density", x = "Candidate Femininity")
d.p1.f$labels$fill <- "Candidate Gender"
d.p1.f$labels$colour <- "Candidate Gender"
##
d.p2.f = ggplot(subset(dat, !is.na(esec.r)), aes(x = femininity, color = esec.r, fill = esec.r)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom", legend.title.align=0.5) + labs(y = "Density", x = "Candidate Femininity") 
d.p2.f$labels$fill <- "European Socio-Economic\nClassification"
d.p2.f$labels$colour <- "European Socio-Economic\nClassification"
##
d.p3.f = ggplot(subset(dat, !is.na(party)), aes(x = femininity, color = party, fill = party)) + geom_density(alpha = 0.3) + theme_light() + theme(legend.position = "bottom") + labs(y = "Density", x = "Candidate Femininity")
d.p3.f$labels$fill <- "Political Party"
d.p3.f$labels$colour <- "Political Party"
##
femininity.p = cowplot::plot_grid(
  d.p1.f, d.p2.f, d.p3.f, 
  align = "hv",
  axis = "b", 
  ncol = 3
  )
ggsave("femininity.pdf", plot = femininity.p, 
       width = 1400,
       height = 500,
       units = c("px"),
       dpi = 80
       )
# mega plot
total.plot.p = cowplot::plot_grid(
  d.p1.p,d.p2.p,d.p3.p,
  d.p1.a,d.p2.a,d.p3.a,
  d.p1.m,d.p2.m,d.p3.m,
  d.p1.f,d.p2.f,d.p3.f,
  align = "hv",
  axis = "b", 
  ncol = 3,
  nrow = 4,
  labels = c('A', 'B', 'C','D', 'E', 'F','G', 'H', 'I','J', 'K', 'L')
)
ggsave("densities.pdf", plot = total.plot.p, 
       width = 1400,
       height = 1400,
       units = c("px"),
       dpi = 80
)

ggsave("densities.jpeg", plot = total.plot.p, 
       width = 1400,
       height = 1400,
       units = c("px"),
       dpi = 80,
       device = "jpeg"
)
## ----


############################## 
# Models
############################## 

## ---- models:d
# base models
main.model.formula = as.formula(turnout ~ phys_occ_cong*esec.r + party + age + city)

# Poisson
options(scipen=999)
m0 = glm(main.model.formula, family="poisson", data=dat)
m1.m = glm(main.model.formula, family="poisson", data=dat[dat$gender=="Man",])
m1.w = glm(main.model.formula, family="poisson", data=dat[dat$gender=="Woman",])


############################## 
# Predicted Probabilities Plot
############################## 
p_load(sjPlot,ggplot2,cowplot)

# Poisson
p1.poisson = plot_model(m0, type = "int", show.legend = F, title = "Combined Data") + theme_sjplot() + labs(y = "Candidate Turnout", x = "Candidate Physical Occupation-Congruent") 
p2.poisson = plot_model(m1.m, type = "int", show.legend = TRUE, title = "Man Data") + theme_sjplot() + theme(legend.position = "bottom",legend.title.align=0.5) + labs(y = "Candidate Turnout", x = "Candidate Physical Occupation-Congruent") 
p2.poisson$labels$colour <- "European Socio-Economic Classification"
p3.poisson = plot_model(m1.w, type = "int", show.legend = F, title = "Woman Data") + theme_sjplot() + labs(y = "Candidate Turnout", x = "Candidate Physical Occupation-Congruent") 

#p_load(gridExtra)
#grid.arrange(p1.poisson, p2.poisson, p3.poisson, nrow = 1, ncol= 3)

pred.prob.p = cowplot::plot_grid(p1.poisson,p2.poisson,p3.poisson,
                   align = "hv",
                   axis = "b", 
                   ncol = 3,
                   nrow = 1
                   )
ggsave("pred_prob_plot.pdf", plot = pred.prob.p, 
       width = 1200,
       height = 600,
       units = c("px"),
       dpi = 80
)

ggsave("pred_prob_plot.jpeg", plot = pred.prob.p, 
       width = 1200,
       height = 600,
       units = c("px"),
       dpi = 80,
       device = "jpeg"
)
############################## 
# robustness checks
############################## 

m2 = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + city, family="poisson", data=dat)
m3 = glm(turnout ~ phys_occ_cong*esec.r + party + age + masculinity + city, family="poisson", data=dat)
m4 = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + femininity + city, family="poisson", data=dat)

m2.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + city, family="poisson", data=dat[dat$gender=="Man",])
m2.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + city, family="poisson", data=dat[dat$gender=="Woman",])

m3.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + masculinity + city, family="poisson", data=dat[dat$gender=="Man",])
m3.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + femininity + city, family="poisson", data=dat[dat$gender=="Woman",])

m4.m = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + masculinity + city, family="poisson", data=dat[dat$gender=="Man",])
m4.w = glm(turnout ~ phys_occ_cong*esec.r + party + age + attractiveness + femininity + city, family="poisson", data=dat[dat$gender=="Woman",])
## ----


# individual predictions: Predicted Probabilities
# p_load(ggeffects)
# ggpredict(m1.w, terms = "phys_occ_cong", condition = c(esec.r="Working Class"))
# ggpredict(m1.m, terms = "phys_occ_cong", condition = c(esec.r="Working Class"))



# Quasi Poisson
# Quasi-Poisson regression is useful since it has a variable dispersion parameter, so that it can model over-dispersed data.   It may be better than negative binomial regression in some circumstances (Verhoef and Boveng. 2007).
# https://rcompanion.org/handbook/J_01.html

options(scipen=999)
m0.quasi.poisson = glm(main.model.formula, family="quasipoisson", data=dat)
m1.m.quasi.poisson = glm(main.model.formula, family="quasipoisson", data=dat[dat$gender=="Man",])
m1.w.quasi.poisson = glm(main.model.formula, family="quasipoisson", data=dat[dat$gender=="Woman",])

p_load(effects)
plot(effect("phys_occ_cong*esec.r", m0.quasi.poisson))
plot(effect("phys_occ_cong*esec.r", m1.m.quasi.poisson))
plot(effect("phys_occ_cong*esec.r", m1.w.quasi.poisson))

# p_load(DAMisc)
# install.packages("DAMisc")
# library(DAMisc)
# m0.plot <- intQualQuant(m0, c("esec.r","phys_occ_cong"), type="slopes", n=25, plot=T);update(m0.plot, layout=c(3,1), as.table=TRUE)
# m1.m.plot <- intQualQuant(m1.m, c("esec.r","phys_occ_cong"), type="slopes", n=25, plot=T);update(m1.m.plot, layout=c(3,1), as.table=TRUE)
# m1.w.plot <- intQualQuant(m1.w, c("esec.r","phys_occ_cong"), type="slopes", n=25, plot=T);update(m1.w.plot, layout=c(3,1), as.table=TRUE)

# p_load(ggeffects,ggplot2)
# e1 = ggpredict(m1.w, terms=c("phys_occ_cong", "esec.r"))
# ggplot(e1) + geom_pointrange(aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high)) + facet_wrap(~group, ncol=3) 

## overdispersion test
# m0 = glm(main.model.formula, family="poisson", data=dat)
# m1.m = glm(main.model.formula, family="poisson", data=dat[dat$gender=="Man",])
# m1.w = glm(main.model.formula, family="poisson", data=dat[dat$gender=="Woman",])
##
# p_load(AER)
# options(scipen=999)
# dispersiontest(m0)
# dispersiontest(m1.m)
# dispersiontest(m1.w)
# null: equidispersion
# alternative: overdispersion
# sign equals overdispersion
##
# other package also suggests overdispersion
# p_load(performance)
# check_overdispersion(m0)
# check_overdispersion(m1.m)
# check_overdispersion(m1.w)


# Negative Binomial
# p_load(MASS)
# m0.nb <- glm.nb(turnout ~ phys_occ_cong*esec.r + party + age + city + city, data = dat)
# m1.m.nb <- glm.nb(turnout ~ phys_occ_cong*esec.r + party + age + city + city, data=dat[dat$gender=="Man",])
# m1.w.nb <- glm.nb(turnout ~ phys_occ_cong*esec.r + party + age + city + city, data=dat[dat$gender=="Woman",])


## ---- table:d
p_load(texreg)
reg.table = texreg( # screenreg texreg
  list(m0, m1.m, m1.w, m2, m3, m4, m2.m, m2.w, m3.m, m3.w, m4.m, m4.w),
  custom.header = list("1" = 1,
                       "2" = 2,
                       "3" = 3, 
                       "4" = 4,  
                       "5" = 5, 
                       "6" = 6, 
                       "7" = 7,
                       "8" = 8,
                       "9" = 9, 
                       "10" = 10, 
                       "11" = 11, 
                       "12" = 12),
  custom.model.names = c(
    # m0, m1.m, m1.w
    "Full", "Man", "Woman", 
    # m2, m3, m4 
    "Full","Full","Full",  
    # m2.m, m2.w,
    "Man", "Woman", 
    # m3.m, m3.w
    "Man", "Woman", 
    # m4.m, m4.w
    "Man", "Woman"),
  #custom.coef.names = NULL,
  omit.coef = "(city)|(party)",
  custom.coef.names = c("Intercept",
                        "Physical Occupation-Congruent",
                        "Middle Class",
                        "Working Class",
                        "Age",
                        "Physical Occupation-Congruent $\\times$ Middle Class",
                        "Physical Occupation-Congruent $\\times$ Working Class",
                        "Attractiveness",
                        "Masculinity",
                        "Femininity"),
  # custom.header = list( "Poisson" = 1),
  stars = c(0.001, 0.01, 0.05, 0.1),
  include.adjrs = FALSE,
  symbol = "\\cdot",
  label = "reg:t",
  caption = "Statistical Model (OLS): Amount of Vote-Buying Offer.",
  float.pos="H",
  use.packages = FALSE,
  threeparttable = TRUE,
  custom.note = "\\item %stars. \\item Dependent variable is Turnout. City fixed effects and party variables omitted. The reference category in the ESEC variable is 'Upper Class.' Given the simetry of the derivatives, changing the reference category does not alter the interpretation of the results. Functional form is Poisson regression for all models."
)
## ----


## Neg Binomial
# p1.neg.bin = plot_model(m0.nb, type = "int", show.legend = F, title = "Combined Data") + theme_sjplot()
# p2.neg.bin = plot_model(m1.m.nb, type = "int", show.legend = TRUE, title = "Man Data") + theme_sjplot() + theme(legend.position = "bottom")
# p3.neg.bin = plot_model(m1.w.nb, type = "int", show.legend = F, title = "Woman Data") + theme_sjplot()
##
# p_load(gridExtra)
# grid.arrange(p1.neg.bin, p2.neg.bin, p3.neg.bin, nrow = 1, ncol= 3)

## OLS
# m0.ols = lm(main.model.formula, data=dat)
# m1.m.ols = lm(main.model.formula, data=dat[dat$gender=="Man",])
# m1.w.ols = lm(main.model.formula, data=dat[dat$gender=="Woman",])
##
# p_load(sjPlot,ggplot2)
##
# plot_model(m0.nb, type = "int", show.legend = F, title = "Combined Data") + theme_sjplot()
# plot_model(m1.m.nb, type = "int", show.legend = TRUE, title = "Man Data") + theme_sjplot() + theme(legend.position = "bottom")
# plot_model(m1.w.nb, type = "int", show.legend = F, title = "Woman Data") + theme_sjplot()
##
# p_load(gridExtra)
# grid.arrange(m0.ols, m1.m.ols, m1.w.ols, nrow = 1, ncol= 3)