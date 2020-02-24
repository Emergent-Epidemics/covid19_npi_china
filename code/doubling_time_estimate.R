#Wuhan Cordon Sanitaire
#SV Scarpino
#Feb. 23 2020

###########
#libraries#
###########
library(lme4)

###############
#Global Params#
###############


######
#Data#
######
dat.combine <- readRDS("../data/1582501807.52655_full_data.RData")

#######
#Model#
#######
provinces <- unique(dat.combine$PROV)
cases_reg <- c()
prov_reg <- c()
time_reg <- c()
for(i in provinces){
  use.full.i <- which(dat.combine$PROV == i)
  
  cases.full.i <- dat.combine$CASES_now[use.full.i]

  dates.full.i <- as.numeric(dat.combine$DATE[use.full.i] - min(dat.combine$DATE[use.full.i], na.rm = TRUE), unit = "days")
  
  first.cases.i <- which(cases.full.i != 0)[1]
  stop.cases.i <- which.max(cases.full.i)

  cases.i <- cases.full.i[first.cases.i:stop.cases.i]
  dates.i <- dates.full.i[first.cases.i:stop.cases.i]
  order.i <- order(dates.i, decreasing = FALSE)
  cases.i <- cases.i[order.i]
  dates.i <- dates.i[order.i]
  
  cases_reg <- c(cases_reg, cases.i)
  time_reg <- c(time_reg, dates.i)
  prov_reg <- c(prov_reg, rep(i, length(cases.i)))
}

mod <- lmer(log(cases_reg+1) ~ time_reg + (time_reg|prov_reg))
hist(log(2)/(ranef(mod)$prov_reg$time_reg+fixef(mod)["time_reg"]))
summary(log(2)/(ranef(mod)$prov_reg$time_reg+fixef(mod)["time_reg"]))
