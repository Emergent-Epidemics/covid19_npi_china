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
first_date <- as.POSIXct(strptime("2020-01-02", format = "%Y-%m-%d")) #date to start regressions
last_date <- as.POSIXct(strptime("2020-02-14", format = "%Y-%m-%d")) #date to stop regressions
dates <- seq(from = first_date, to = last_date, by = 60*60*24*7)

######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

#######
#Model#
#######
doubling_prov <- list()
doubling_fixed <- list()
mobs <- list()
for(i in 1:(length(dates)-1)){
  use.i <- which(dat.combine$DATE >= dates[i] & dat.combine$DATE < dates[i+1] & dat.combine$PROV != "Hubei")
  
  data.i <- dat.combine[use.i,]
  mob_mean.i <- by(data = data.i$MOB, INDICES = data.i$PROV, FUN = mean, na.rm = TRUE)
  data.i$DATE <- as.numeric(data.i$DATE - dates[i], unit = "days")
  mod3.i <- try(lmer(data = data.i, log(CASES_now + 1) ~ DATE + (DATE|PROV)), silent = TRUE)
  
  if(is(mod3.i)[1] == "try-error"){
    fixed.i <- NA
    doubling.i <- NA
    mob.i <- NA
  }else{
    fixed.i <-   fixef(mod3.i)["DATE"]
    doubling.i <- ranef(mod3.i)$PROV$DATE+fixed.i
    mt.mob.i <- match(row.names(ranef(mod3.i)$PROV), names(mob_mean.i))
    
    if(length(which(row.names(ranef(mod3.i)$PROV) != names(mob_mean.i)[mt.mob.i])) > 0){
      stop()
    }
    mob.i <- mob_mean.i[mt.mob.i]
  }
  
  doubling_prov[[i]] <- doubling.i
  doubling_fixed[[i]] <- fixed.i
  mobs[[i]] <- mob.i
}

rates <- unlist(lapply(doubling_prov, function(x) x))
mobmean <- unlist(lapply(mobs, function(x) x))
prov <- unlist(lapply(mobs, function(x) names(x)))
times <- rep(dates[-length(dates)], times = unlist(lapply(doubling_prov, function(x) length(x))))
fill <- rep(c("1 - before", "1 - before", "1 - before", "2 - after", "2 - after", "2 - after"), times = unlist(lapply(doubling_prov, function(x) length(x))) )
dat.plot <- data.frame(rates, times, fill, mobmean, prov)

quartz(width = 8, height = 6)
ggplot(dat.plot, (aes(x = as.factor(times), y = rates, fill = fill))) + geom_boxplot() + geom_point() + scale_fill_manual(values = c("#1d91c0", "#c7e9b4"), labels = c("Pre-cordon", "Post-cordon")) + xlab("2020") + ylab("COVID19 growth rate (province-level)") + theme(legend.position = c(0.1,0.9), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_hline(yintercept = 0, linetype = "dashed", color = "#b2182b", size = 1) + labs(fill = "")

quartz(width = 8, height = 8)
ggplot(dat.plot, aes(x = mobmean, y = rates)) + geom_point() + facet_wrap(~as.factor(times))+ xlab("Connectivity to Wuhan prior to the cordon sanitaire (log scale)") + ylab("COVID19 growth rate (province-level)") + theme(legend.position = c(0.1,0.9), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 14), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01))  + geom_hline(aes(yintercept = 0), linetype = "dashed", color = "#b2182b", size = 1)
