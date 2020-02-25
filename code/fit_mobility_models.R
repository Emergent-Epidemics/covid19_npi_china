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
library(zoo)

###############
#Global Params#
###############
first_date <- as.POSIXct(strptime("2020-01-05", format = "%Y-%m-%d")) #date to start regressions
last_date <- as.POSIXct(strptime("2020-02-04", format = "%Y-%m-%d")) #date to stop regressions
mobility_date <-  as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")) #date to use for mobility
focal_date <- as.POSIXct(strptime("2020-02-10", format = "%Y-%m-%d")) #date to calculate cumulative case regressions
r2_plot_date <- as.POSIXct(strptime("2020-01-31", format = "%Y-%m-%d")) #date to calculate cumulative case regressions

dates <- seq(from = first_date, to = last_date, by = 60*60*24)
time_stamp <- as.numeric(Sys.time())
save_new <- TRUE

######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")
rm_hub <- which(dat.combine$PROV == "Hubei")
dat.combine <- dat.combine[-rm_hub,]

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
for(i in 1:length(dates)){
  use.full.i <- which(dat.combine$DATE == dates[i])
  
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
  
  if(save_new == "TRUE" & dates[i] == r2_plot_date & is(lm.i)[1] != "try-error"){
    pdf(paste0(as.character(r2_plot_date), "r2_plot.pdf"))
    plot(data.i$mob, log(data.i$cumulative+1), pch = 16, bty = "n", xlab = "Mobility from Wuhan (log scale)", ylab = "Cumulative cases (log scale)", main = paste0(as.character(r2_plot_date), " R2 = ", round(summary(lm.i)$adj.r.squared, 2)))
    abline(lm.i, col = "red", lty = 3, lwd = 3)
    dev.off()
  }
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

rm_neg <- which(nb_r2 < 0)
nb_r2[rm_neg] <- NA
roll_mean_nb_r2 <- rollapply(nb_r2, width = 3, by = 1, FUN = mean, na.rm = TRUE, align = "left")
plot(dates[-c(1:2)], roll_mean_nb_r2, type = "l")

out <- data.frame(dates[-c(1:2)], roll_mean_nb_r2)
colnames(out) <- c("date", "r2")
if(save_new == TRUE){
  write.csv(out, file = paste0(time_stamp, "roll_mean_nb_r2.csv"), row.names = FALSE, quote = FALSE)
}