#Wuhan Cordon Sanitaire
#SV Scarpino
#Feb. 23 2020

###########
#libraries#
###########
library(ggplot2)
library(dplyr)
library(pheatmap)
library(wesanderson)
library(RVAideMemoire)
library(lme4)
library(glmulti)
library(leaps)
library(MASS)
library(pscl)
library(dynlm)

###############
#Global Params#
###############
first_date <- as.POSIXct(strptime("2020-01-15", format = "%Y-%m-%d")) #date to start regressions
last_date <- as.POSIXct(strptime("2020-02-10", format = "%Y-%m-%d")) #date to stop regressions
dates <- seq(from = first_date, to = last_date, by = 60*60*24)

######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

#######
#Model#
#######
slopes <- rep(NA, length(dates))
intercepts <- rep(NA, length(dates))
slopes2 <- list()
intercepts2 <- list()
for(i in 1:length(dates)){
  use.i <- which(dat.combine$DATE == dates[i] & dat.combine$PROV != "Hubei")
  mod.i <- try(lm(log(CASES_now + 1) ~ log(CASES_lag4 + 1)*POPS, data = dat.combine[use.i,]), silent = TRUE)
  if(is(mod.i)[1] == "try-error"){
    next
  }
  
  mod2.i <- try(lm(log(CASES_now + 1) ~ log(CASES_lag7 +1):PROV, data = dat.combine[use.i,]), silent = TRUE)
  if(is(mod2.i)[1] == "try-error"){
    next
  }
  slopes[i] <- coefficients(mod.i)[2]
  intercepts[i] <- coefficients(mod.i)[1]
  
  slopes2[[i]] <- coefficients(mod2.i)
  intercepts2[[i]] <- coefficients(mod2.i)
}

boxplot(lapply(intercepts2, function(x) log(exp(x))), names = format(dates, "%d-%b"), las = 2, col = "gray", range = 0, ylab = "Province-level relative growth rates (log scale)", bty = "n")
abline(h = log(2), lty = 3, lwd = 2, col = "red")

