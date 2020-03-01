#Wuhan Cordon Sanitaire
#SV Scarpino
#Feb. 23 2020

###########
#libraries#
###########
library(ggplot2)
library(wesanderson)

###############
#Global Params#
###############
date_cut <- as.POSIXct(strptime("2020-01-01", format = "%Y-%m-%d"))

######
#Data#
######
dat.combine <- readRDS("../data/1582546769.28308_full_data.RData")

##########
#Plotting#
##########
pal <-wes_palette(name = "Darjeeling1", length(unique(dat.combine$PROV)), type = "continuous")

quartz(width = 10, height = 10)
ggplot(dat.combine, aes(DATE, log(CASES_now + 1), color = PROV)) + geom_line(size = 1.1) + facet_wrap(~PROV) + scale_color_manual(values = pal) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#000000") + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")+ ylab("Daily COVID-19 cases (log scale)") + xlab("2020") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")

quartz(width = 10, height = 10)
ggplot(dat.combine, aes(DATE, log(CASES_CUMULATIVE/exp(POPS)), color = PROV)) + geom_line(size = 1.1) + facet_wrap(~PROV) + scale_color_manual(values = pal) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#000000") + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")+ ylab("Daily COVID-19 cases (log scale)") + xlab("2020") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")

use_provinces <- c("Guangdong", "Fujian", "Hunan")
dat.plot.use <- which(! dat.combine$PROV %in% use_provinces & dat.combine$DATE > date_cut)
dat.plot <- dat.combine[dat.plot.use,]

pal2 <- c("#4d4d4d", wes_palette(name = "Darjeeling1", 5, type = "discrete")[c(2, 5, 1, 3, 4, 5)])

quartz(width = 10, height = 6)
ggplot(dat.plot, aes(DATE, log(CASES_now + 1), fill = PROV)) + geom_bar(stat = "identity") + scale_fill_manual(values = pal2) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#e34a33", size = 1) + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#e34a33", size = 1)+ ylab("Daily COVID-19 cases (log scale)") + xlab("2020") + theme(legend.position = c(0.1,0.7), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(fill = "Province")

rm_hub <- which(dat.combine$PROV == "Hubei" | dat.combine$DATE < date_cut)
dat.combine.no.hub <- dat.combine[-rm_hub,]
quartz(width = 10, height = 6)
ggplot(dat.combine.no.hub, aes(DATE, CASES_now)) + stat_summary(fun.y = 'sum', geom = 'bar') + ylab("Daily COVID-19 cases") + xlab("2020") + theme(legend.position = c(0.1,0.7), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) +  geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#e34a33", size = 1)


quartz(width = 10, height = 6)
ggplot(dat.combine, aes(DATE, CASES_now)) + geom_bar(stat = "identity") + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#e34a33", size = 1) + ylab("Daily COVID-19 cases") + xlab("2020") + theme(legend.position = c(0.1,0.7), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + labs(fill = "Province") + annotate(geom="text", x=as.POSIXct(strptime("2020-01-26", format = "%Y-%m-%d"))+10000, y=1150, label="Wuhan cordon", color="#e34a33") 

quartz(width = 10, height = 10)
ggplot(dat.combine.no.hub, aes(DATE, CASES_now, color = PROV)) + geom_line(size = 1.1) + facet_wrap(~PROV) + scale_color_manual(values = pal) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#000000") + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")+ ylab("Daily COVID-19 cases (log scale)") + xlab("2020") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 16), panel.grid.minor = element_line(colour = "#00000000",linetype = 3), panel.grid.major = element_line(colour = "#00000000", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01)) + geom_vline(xintercept = as.POSIXct(strptime("2020-01-22", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")
