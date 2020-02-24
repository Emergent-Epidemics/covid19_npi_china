
quartz(width = 10, height = 10)
ggplot(dat.combine, aes(DATE, log(CASES_now+1), color = PROV)) + geom_line() + facet_wrap(~PROV) + theme(legend.position = "none") + geom_vline(xintercept = as.POSIXct(strptime("2020-01-23", format = "%Y-%m-%d")), linetype="dashed", color = "#000000") + geom_vline(xintercept = as.POSIXct(strptime("2020-02-06", format = "%Y-%m-%d")), linetype="dashed", color = "#000000")
