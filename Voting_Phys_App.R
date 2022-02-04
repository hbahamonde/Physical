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

p_load(rio,tibble)
isco.d <- rio::import("https://raw.githubusercontent.com/hbahamonde/Physical/main/data/isco_data.csv")
p_load(dplyr,tidyverse)
isco.d = isco.d %>% 
  group_by(ISCO_code) %>% 
  mutate(ISCO_job_desc_collaps = paste0(ISCO_job_description, collapse = " ")) %>% 
  distinct(ISCO_code, ISCO_job_desc_collaps)
isco.d$ISCO_code = ifelse(isco.d$ISCO_code<1000,0, isco.d$ISCO_code)
isco.d$ISCO_group = as.factor(gsub("(^\\d{1}).*", "\\1", isco.d$ISCO_code))



isco.d$ISCO_group = recode_factor(isco.d$ISCO_group, 
                                  `0` = "Army", 
                                  `1` = "Managers", 
                                  `2` = "Professional", 
                                  `3` = "Technicians",
                                  `4` = "Clerical",
                                  `5` = "Service",
                                  `6` = "Skilled Agricultural",
                                  `7` = "Craft",
                                  `8` = "Machine Operators",
                                  `9` = "Elementary Occupations")
# https://en.wikipedia.org/wiki/International_Standard_Classification_of_Occupations
# http://www.ilo.org/public/english/bureau/stat/isco/docs/index08-draft.xlsx
phys.cong.d = merge(phys.cong.d,isco.d, by = "ISCO_code")

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
                            ISCO_group,
                            ISCO_job_desc_collaps,
                            everything()
                            )
# table(dat$municipality.y==dat$municipality) # municipalities match 100%
## I was getting duplicated municipality columns because of the NA's. Delete redundant columns.
dat = dat %>% select(c(-municipality.x,-municipality.y))

## I was getting udplicated rows because the occupation character vector was different. Will select distinct based on the evaluation columns.
p_load(dplyr)
dat = dat %>% dplyr::distinct(city,party,firstname,lastname,phys_occ_cong,femininity,masculinity,attractiveness, fem_dom_job, .keep_all = TRUE)

############################## 
# Models

#####
# m0
#####

options(scipen=999)
m0 = glm(turnout ~ phys_occ_cong*gender + party + city, family="poisson", data=dat)

p_load(effects)
plot(predictorEffects(m0))
# At low levels of congruence: women base their vote more on job-phys cong.
# At high levels of congruence: men base their vote more on job-phys cong.
# Interpretation: women are punished less for the lack of job-phys cong. (Women will be forgiven more?).
# Banducci2008: "female counterparts are described as warm, compassionate, people-oriented, gentle, kind, passive, caring, and sensitive"

#####
# m1
#####

m1 = glm(turnout ~ phys_occ_cong + attractiveness*gender + party + city, family="poisson", data=dat)

# p_load(effects)
# plot(predictorEffects(m0))
p_load(sjPlot, sjmisc, ggplot2)
plot_model(m0, type = "int") 
## Congruence matters, but ONLY AT HIGHER LEVELS OF ATTRACTIVENESS
##############################
# m1

options(scipen=999)
m1 = glm(turnout ~ attractiveness*gender + party + city, family="poisson", data=dat)

p_load(effects)
plot(predictorEffects(m1))
# Attractiveness matters, but it does so more for man than woman.
## A. At similar levels of "ugliness" woman do better.
## B. At similar levels of "beauty" man do better.

# p_load(sjPlot, sjmisc, ggplot2)
# plot_model(m1, type = "int")

#####
# m2
#####

##############################
# m2

# ARE RIGHT-WING CANDIDATES MORE ATTRACTIVE? Well, kind of, but not really.
m2.a = lm(attractiveness  ~ ideology, dat)
#summary(m2.a)
p_load(effects)
plot(predictorEffects(m2.a))
# RIGHT-WING CANDIDATES ARE *NOT* MORE ATTRACTIVE THAN CANDIDATES ON THE LEFT (CONTRARY TO Berggren2017).
## Really what's happening is that the effect vary by gender.  Woman in politics are always considered more attractive than men, but more so "centered" woman.
m2.b = lm(attractiveness  ~ ideology*gender, dat)
#summary(m2.b)
#p_load(effects)
#plot(predictorEffects(m2.b))
p_load(sjPlot, sjmisc, ggplot2)
plot_model(m2.b, type = "int")

##############################
# m3
m3 = glm(turnout ~ attractiveness*ideology*gender + city, family="poisson", data=dat)
# p_load(effects,sjmisc)
# plot(predictorEffects(m3))

sjPlot::plot_model(
  m3,
  type = "int",
  colors = "bw"
) %>% 
  purrr::map(function(plot) {
    # You can also use scale_color_manual/scale_fill_manual or other variants here
    plot <- plot + scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1")
    if (!is.null(plot[["labels"]][["linetype"]])) {
      plot[["labels"]][["colour"]] <- plot[["labels"]][["linetype"]]
      plot[["labels"]][["fill"]] <- plot[["labels"]][["linetype"]]
    } else {
      plot[["labels"]][["colour"]] <- plot[["labels"]][["shape"]]
      plot[["labels"]][["fill"]] <- plot[["labels"]][["shape"]]
    }
    plot[["guides"]][["colour"]] <- NULL
    plot[["guides"]][["fill"]] <- NULL
    return(plot)
  })

# While woman in politics are considered more attractive (as per m2.b), that doesn't translate into more votes. Attractive man do better electorally than attractive woman, particularly center-left candidates. 

##############################
# m4
m4 = glm(turnout ~ phys_occ_cong*ISCO_group + party + age + attractiveness + city, family="poisson", data=dat)

p_load(sjPlot, sjmisc, ggplot2)
plot_model(m4, type = "int")
# Congruence matters, but it matters more if you're a boss ("Managers") or an Army person ("Army"). Actually, the less you look like a "Machine Operator," the better.
# This is kinda the "discrimination" model.





##############################
# m5
m5.m = glm(turnout ~ phys_occ_cong*masculinity + party + age + city, family="poisson", data=dat)

p_load(sjPlot, sjmisc, ggplot2)
plot_model(m5.m, type = "int") 


m5.f = glm(turnout ~ phys_occ_cong*femininity + party + age + city, family="poisson", data=dat)

p_load(sjPlot, sjmisc, ggplot2)
plot_model(m5.f, type = "int") 

