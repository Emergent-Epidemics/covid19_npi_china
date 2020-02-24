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
mobility_date <-  as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")) #date to use for mobility
focal_date <- as.POSIXct(strptime("2020-02-10", format = "%Y-%m-%d")) #date to calculate cumulative case regressions
dates <- seq(from = first_date, to = last_date, by = 60*60*24)

######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

#######
#Model#
#######
mobility_use <- which(dat.combine$DATE == mobility_date)
mobility <- dat.combine$MOB[mobility_use]
prov_mob <- dat.combine$PROV

pois_r2 <- c()
nb_r2 <- c()
lm_r2 <- c()
rho <- c()
rho_ci <- list()
dates_reg <- c()
for(i in dates){
  use.full.i <- which(dat.combine$DATE == i)
  
  cases.full.i <- dat.combine$CASES_now[use.full.i]
  prov.full.i <- dat.combine$PROV[use.full.i]
  mob.mt.i <- match(prov.full.i, prov_mob)
  mob.full.i <- mobility[mob.mt.i]
  
  cumulative.full.i <- dat.combine$CASES_CUMULATIVE[use.full.i]
  
  data.i <- data.frame(as.numeric(cases.full.i), as.numeric(mob.full.i), as.numeric(cumulative.full.i))
  data.i <- na.omit(data.i)
  colnames(data.i) <- c("cases", "mob", "cumulative")
  glm.pois.i <- try(glm(cases~mob, data = data.i, family = poisson), silent = TRUE)
  glm.nb.i <- try(glm.nb(cases~mob, data = data.i), silent = TRUE)
  lm.i <- try(lm(log(cumulative+1)~mob, data = data.i), silent = TRUE)
  
  ord.case.i <- order(data.i$cumulative, decreasing = TRUE)
  ord.mob.i <- order(data.i$mob, decreasing = TRUE)
  rho.i <- try(cor.test(ord.case.i, ord.mob.i, method = "spearman"))
  
  if(is(glm.pois.i)[1] == "try-error"){
    r2ML.pois.i <- NA
  }else{
    r2ML.pois.i <- pR2(glm.pois.i)
  }
  
  if(is(glm.nb.i)[1] == "try-error"){
    r2ML.nb.i <- NA
  }else{
    r2ML.nb.i <- pR2(glm.nb.i)
  }
  
  if(is(lm.i)[1] == "try-error"){
    r2ML.lm.i <- NA
  }else{
    r2ML.lm.i <- summary(lm.i)$adj.r.squared
  }
  
  if(is(rho.i)[1] == "try-error"){
    rho.est.i <- NA
    rho.ci.i <- NA
  }else{
    rho.est.i <- rho.i$estimate
    if(dates == focal_date){
      rho.ci.i <- spearman.ci(ord.case.i, ord.mob.i, nrep = 1000, conf.level = 0.95)$conf.int
    }else{
      rho.ci.i <- NA
    }
  }
  
  pois_r2 <- c(pois_r2, r2ML.pois.i["r2ML"])
  nb_r2 <- c(nb_r2, r2ML.nb.i["r2ML"])
  lm_r2 <- c(lm_r2, r2ML.lm.i)
  rho <- c(rho, rho.est.i)
  rho_ci[[i]] <- rho.ci.i
  dates_reg <- c(dates_reg, as.character(dates[i]))
}