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
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

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
doubling <- log(2)/(ranef(mod)$prov_reg$time_reg+fixef(mod)["time_reg"])
hist(doubling)
summary(doubling)

use_mob <- which(dat.combine$DATE == as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")))
mt_mob <- match(row.names(ranef(mod)$prov_reg), dat.combine$PROV[use_mob])
rm_hub <- which(row.names(ranef(mod)$prov_reg) == "Hubei")
mod <- lm(doubling[-rm_hub] ~ dat.combine$MOB[use_mob][mt_mob][-rm_hub])
plot(dat.combine$MOB[use_mob][mt_mob][-rm_hub], doubling[-rm_hub], pch = 16, bty = "n", ylab = "Doulbing time (days)", xlab = "Mobility from Wuhan (log)", main = "Mobility from Wuhan predicts province-level doubling")
abline(mod, lty = 3, col = "red", lwd = 3)
summary(mod)

ord <- order(abs(mod$residuals), decreasing = TRUE)
dat.combine$PROV[use_mob][mt_mob][-rm_hub][ord]

ord2 <- order(doubling[-rm_hub], decreasing = FALSE)
data.frame(dat.combine$PROV[use_mob][mt_mob][-rm_hub][ord2],doubling[-rm_hub][ord2])