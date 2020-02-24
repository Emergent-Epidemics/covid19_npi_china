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
last_date <- as.POSIXct(strptime("2020-02-18", format = "%Y-%m-%d")) #date when line list is most up-to-date
first_date <- as.POSIXct(strptime("2020-01-01", format = "%Y-%m-%d")) #date to start recording cases
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
models <- list(c("CASES_lag4"), c("CASES_lag4", "MOB"))
model_names <- unlist(lapply(models, paste, collapse = "-"))

results <- list()
bic_pois <- c()
bic_pois_mod <- c()
bic_pois_date <- c()
for(t in 1:length(dates)){
  use.i <- which(dat.combine$DATE == dates[t])
  dat.combine.i <- dat.combine[use.i,]
  
  BIC <- matrix(NA, ncol = 3, nrow = length(models))
  colnames(BIC) <- c("LM","Pois", "NB")
  BIC <- as.data.frame(BIC)
  AIC <- BIC
  R2 <- BIC
  
  Y <- dat.combine.i$CASES_now
  for(i in 1:length(models)){
    pred.i <- dat.combine.i[,models[[i]]]
    
    if(length(models[[i]]) == 1){
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
  use.bic.i <- which.min(BIC[,2])
  if(use.bic.i != 1 & BIC[1,2] - BIC[use.bic.i,2] < 4){
    use.bic.i <- 1
  }
  
  bic_out.i <- BIC[use.bic.i,2]
  bic_pois <- c(bic_pois, bic_out.i)
  bic_pois_mod <- c(bic_pois_mod, model_names[use.bic.i])
  bic_pois_date <- c(bic_pois_date, rep(as.character(dates[t]), length(use.bic.i)))
  
  out <- data.frame(AIC, BIC)
  colnames(out) <- c(paste0(colnames(AIC), "-AIC"), paste0(colnames(AIC), "-BIC"))
  row.names(out) <- model_names
  results[[t]] <- out
}

if(save_new == TRUE){
  saveRDS(results, file = paste0(time_stamp, "_model_selection_scenarios.RData"))
}

dat.plot <- data.frame(bic_pois, bic_pois_mod, bic_pois_date)
dat.plot$bic_pois_date <- as.POSIXct(strptime(dat.plot$bic_pois_date, format = "%Y-%m-%d"))

quartz(width = 8, height = 5)
ggplot(dat.plot, aes(bic_pois_date, fill = bic_pois_mod)) + geom_bar() + scale_fill_manual(values = c("#0B775E", "#35274A", "#E1BD6D"), guide_legend(title = "Model"), labels = c("Cases", "Cases + Mobility")) + xlab("Date") + ylab("Relative BIC Improvement") + theme(legend.position = c(0.11,0.87), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#EABE9495", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 14), axis.title = element_text(colour = "black", size = 15), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#E1BD6D",size = 1.5) + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#E1BD6D", size = 1.5) + scale_x_datetime(expand = c(0.01,0.01))