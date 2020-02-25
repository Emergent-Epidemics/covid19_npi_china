#Wuhan Cordon Sanitaire
#SV Scarpino
#Feb. 23 2020

###########
#libraries#
###########
library(ggplot2)

###############
#Global Params#
###############


######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

##########
#Plotting#
##########
quartz(width = 10, height = 10)
ggplot(dat.combine, aes(DATE, log(CASES_now + 1), color = PROV)) + geom_line() + facet_wrap(~PROV) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#000000") + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")+ ylab("Daily COVID-19 cases (log scale)") + xlab("Date") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")

quartz(width = 10, height = 10)
ggplot(dat.combine, aes(DATE, log(CASES_CUMULATIVE/exp(POPS)), color = PROV)) + geom_line() + facet_wrap(~PROV) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#000000") + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")+ ylab("Daily COVID-19 cases (log scale)") + xlab("Date") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")