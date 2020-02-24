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
library(glmnet)

###############
#Global Params#
###############
first_date <- as.POSIXct(strptime("2020-01-15", format = "%Y-%m-%d")) #date to start regressions
last_date <- as.POSIXct(strptime("2020-02-10", format = "%Y-%m-%d")) #date to stop regressions
mobility_date <-  as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")) #date to use for mobility
focal_date <- as.POSIXct(strptime("2020-02-10", format = "%Y-%m-%d")) #date to calculate cumulative case regressions
dates <- seq(from = first_date, to = last_date, by = 60*60*24)
save_new <- FALSE
time_stamp <- as.numeric(Sys.time())

######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

#######
#Model#
#######
rm_hub <- which(dat.combine$PROV == "Hubei")
dat.combine <- dat.combine[-rm_hub,]
models <- list(c("CASES_lag4"), c("CASES_lag4", "TEST"), c("CASES_lag4", "MOB"), c("CASES_lag4", "MOB_IND"), c("CASES_lag4", "TEST", "MOB"), c("CASES_lag4", "MOB_IND", "MOB"), c("CASES_lag4", "MOB_IND", "MOB","TEST"))
BIC <- matrix(NA, ncol = 3, nrow = length(models))
colnames(BIC) <- c("LM","Pois", "NB")
BIC <- as.data.frame(BIC)
AIC <- BIC
R2 <- BIC

Y <- dat.combine$CASES_now
for(i in 1:length(models)){
  pred.i <- dat.combine[,models[[i]]]
  
  if(i == 1){
    pred.i <- data.frame(pred.i)
  }
  pred.lm.i <- pred.i
  pred.lm.i[,1] <- log(pred.lm.i[,1] + 1)
  
  lm.i <- try(lm(log(Y + 1)~.,data = pred.lm.i), silent = TRUE)
  glm.pois.i <- try(glm(Y~.,data = pred.i, family = poisson), silent = TRUE)
  glm.nb.i <- try(glm.nb(Y~.,data = pred.i), silent = TRUE)
  
  if(is(glm.pois.i)[1] == "try-error"){
    r2ML.pois.i <- NA
    bic.pois.i <- NA
    aic.pois.i <- NA
  }else{
    r2ML.pois.i <- pR2(glm.pois.i)
    bic.pois.i <- bic(glm.pois.i)
    aic.pois.i <- aic(glm.pois.i)
  }
  
  if(is(glm.nb.i)[1] == "try-error"){
    r2ML.nb.i <- NA
    bic.nb.i <- NA
    aic.nb.i<- NA
  }else{
    r2ML.nb.i <- pR2(glm.nb.i)
    bic.nb.i <- bic(glm.nb.i)
    aic.nb.i<- aic(glm.nb.i)
  }
  
  if(is(lm.i)[1] == "try-error"){
    r2ML.i <- NA
    bic.i <- NA
    aic.i <- NA
  }else{
    r2ML.i <- summary(lm.i)$adj.r.squared
    bic.i <- bic(lm.i)
    aic.i <- aic(lm.i)
  }
  
  R2[i,] <- c(r2ML.i, r2ML.pois.i["r2ML"], r2ML.nb.i["r2ML"])
  AIC[i,] <- c(aic.i, aic.pois.i, aic.nb.i)
  BIC[i,] <- c(bic.i, bic.pois.i, bic.nb.i)
}

model_names <- unlist(lapply(models, paste, collapse = "-"))
out <- data.frame(AIC, BIC)
colnames(out) <- c(paste0(colnames(AIC), "-AIC"), paste0(colnames(AIC), "-BIC"))
row.names(out) <- model_names
if(save_new == TRUE){
  write.csv(out, file = paste0(time_stamp, "-model_selection_results.csv"))
}