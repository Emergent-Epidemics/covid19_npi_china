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

#glmnet
# A tuning parameter for the lasso/elastic-net model-selection algorithm
# alpha = 1 corresponds to l1/lasso
# alpha = 0.5 corresponds to elastic net
# alpha = 0 gives ridge regression, which will not do any model selection, merely shrinkage
alpha <- 0.5
D <- dat.combine[,models[[length(models)]]]
D$MOB_IND <- as.numeric(D$MOB_IND)
D$TEST <- as.numeric(D$TEST)
D <- cbind(Y, D)
D <- as.matrix(D)
D <- na.omit(D)
Y_cv <- D[,"Y"]
D <- D[,-which(colnames(D) == "Y")]

#pick lambda by loo cross-validation
mycv <- cv.glmnet(D, Y_cv, nfolds=length(Y_cv), parallel=TRUE, family='poisson', alpha=alpha, keep=TRUE, standardize=TRUE)
lambda0 <- mycv$lambda.min

# Fit the model
glm.pois <- glmnet(D, Y_cv, family='poisson', lambda = lambda0 + 1, alpha=alpha,  maxit=1000000, standardize=TRUE)

keep.params.ij <- as.numeric(glm.pois$beta !=0)
use.params.ij <- which(keep.params.ij == 1)

#fit negative binomial after selecting coefficients
pred_loo_negbinom <- rep(NA, length(Y_cv))
for(i in 1:length(Y_cv)){
  nb_oos.i <- glm.nb(Y_cv[-i] ~ D[-i, use.params.ij])
  pred_loo_negbinom[i] <- exp(nb_oos.i$coefficients[1] + D[i, use.params.ij] %*% nb_oos.i$coefficients[-1])
}

sum(dpois(Y_cv, pred_loo_negbinom, log=TRUE) #ll nb
sum(dpois(Y_cv, pred_loo_negbinom, log=TRUE)[which(dpois(Y_cv, pred_loo_negbinom, log=TRUE) > -1000)]) #ll nb with outliers removed
sum(dpois(Y_cv, exp(mycv$fit.preval[,which.min(mycv$cvm)]), log=TRUE)) #ll pois
