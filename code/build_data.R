#Wuhan Cordon Sanitaire
#SV Scarpino
#Feb. 23 2020

###########
#libraries#
###########
library(dplyr)
library(googlesheets4)
library(googledrive)

###############
#Global Params#
###############
save_new <- FALSE #to save new combined data sets
time_stamp <- as.numeric(Sys.time())
last_date <- as.POSIXct(strptime("2020-02-18", format = "%Y-%m-%d")) #date when line list is most up-to-date
first_date <- as.POSIXct(strptime("2019-12-01", format = "%Y-%m-%d")) #date to start recording cases
test_start <- as.POSIXct(strptime("2020-01-19", format = "%Y-%m-%d")) #date when PCR tests were first widely available
incubation_end <- as.POSIXct(strptime("2020-01-27", format = "%Y-%m-%d")) #this is the mean incubation period out from Jan 22nd (last day before shutdown)

######
#Data#
######
#Line list data
source("../../Emergent_Epidemics_Lab_nCoV2019/Emergent_Epidemics_Lab_nCoV2019/scripts/update_data.R")
full_data <- update_data(savefile = FALSE)

#fix_macau
full_data$province[which(full_data$province == "Macao")] <- "Macau"
#fix_Zheijiang
full_data$province[which(full_data$province == "Zheijiang")] <- "Zhejiang"

#Population data
pops <- read.table("../data/china_population.txt", sep = "\t", header = TRUE)

#Mobility data
mobility_data <- sheets_get(ss = "1ov7Z2IjEPRB41rmRe4oTF1nI6L3EB9_Bn64AJIkOX6g") %>%
  read_sheet(sheet = "Target Province", skip = 1)

province_data_names <- sheets_get(ss = "1wQVypefm946ch4XDp37uZ-wartW4V7ILdg-qYiDXUHM")
rm_announcement <- which(province_data_names$sheets$name == "Announcement")
if(length(rm_announcement) > 0){
  pr_names <- province_data_names$sheets$name[-rm_announcement]
}else{
  pr_names <- province_data_names$sheets$name
}

#JHU province data
counter <- 1
for(i in pr_names){
  sheets.i <- sheets_get(ss = "1wQVypefm946ch4XDp37uZ-wartW4V7ILdg-qYiDXUHM") %>%
    read_sheet(sheet = i)
  use.i <- which(colnames(sheets.i) %in% c("Province/State", "Country", "Country/Region", "Last Update", "Date last updated", "Confirmed"))
  sheets.i <- sheets.i[,use.i]
  colnames(sheets.i) <- c("Province/State", "Country/Region", "Last Update", "Confirmed")
  if(counter == 1){
    province_data <- sheets.i
  }else{
    province_data <- rbind(province_data, sheets.i)
  }
  counter <- counter + 1
  
  if(counter %in% c(30, 60, 90, 120)){
    message("pausing to not overload api quota")
    Sys.sleep(time = 400)
  }
}

#model of onset date
rm_date_future <- which(full_data$date_onset_symptoms > Sys.time())
if(length(rm_date_future) > 0){
  full_data$date_onset_symptoms[rm_date_future] <- NA
}

onset_norm <- as.numeric(last_date - full_data$date_onset_symptoms, unit = "days")
conf_norm <- as.numeric(last_date - full_data$date_confirmation, unit = "days")
day_mod <- lm(onset_norm ~ conf_norm)
full_data$onset_pred <- as.Date(-predict(day_mod, full_data$date_confirmation), origin = last_date)

dates_use <- apply(full_data[,c("onset_pred", "date_onset_symptoms", "date_admission_hospital", "date_confirmation")], 1, min, na.rm = TRUE)
full_data$min_dat <- as.POSIXct(strptime(dates_use, format = "%Y-%m-%d"))

#filtering for date range and only China
use <- which(full_data$min_dat <= last_date & full_data$min_dat > first_date & toupper(full_data$country) == "CHINA")
data_plot <- full_data[use, ]

#indicator for Hubei
data_plot$hubei <- rep("Hubei", nrow(data_plot))
data_plot$hubei[which(toupper(data_plot$province) != "HUBEI")] <- "Outside Hubei"
data_plot$one <- rep(1, nrow(data_plot))

#indicator for Wuhan travel
data_plot$wuhan_travel <- rep("No Wuhan travel", nrow(data_plot))
data_plot$wuhan_travel[grep("Wuhan", data_plot$travel_history_location, ignore.case = TRUE)] <- "Wuhan travel"
data_plot$wuhan_travel[which(is.na(data_plot$lives_in_Wuhan) == FALSE & data_plot$lives_in_Wuhan != "0" & data_plot$lives_in_Wuhan != "N/A" & data_plot$lives_in_Wuhan != "NULL" & data_plot$lives_in_Wuhan != "used to be")] <-  "Wuhan travel"
data_plot$wuhan_travel[which(data_plot$travel_history_location %in% c("Traveled extensively"))] <-  "Wuhan travel"
data_plot$wuhan_travel[which(is.na(data_plot$travel_history_location) == "TRUE" & is.na(data_plot$lives_in_Wuhan) == TRUE)] <- "Missing"

#build aggregated line list
line_list <- data_plot %>% group_by(min_dat, province, wuhan_travel) %>% summarise(sum = sum(one))
line_list$province <- as.factor(line_list$province)

#building combined data set
dates <- seq(from = first_date, to = last_date, by = 60*60*24)
prop_cols_short <- c(rep(88, 36), seq(4, 100, by = 4)) #88 is Jan. 22nd
prop_cols <- c(prop_cols_short, rep(88, length(dates)-length(prop_cols_short)))

cases <- matrix(NA, ncol = length(unique(line_list$province)), nrow = length(dates))
colnames(cases) <- unique(line_list$province)
row.names(cases) <- as.character(dates)
cases <- as.data.frame(cases)

hubei_cases <- matrix(NA, ncol = 1, nrow = length(dates))
colnames(hubei_cases) <- "Hubei"
row.names(hubei_cases) <- as.character(dates)
hubei_cases <- as.data.frame(hubei_cases)

migration <- matrix(NA, ncol = length(unique(line_list$province)), nrow = length(dates))
colnames(migration) <- unique(line_list$province)
row.names(migration) <- as.character(dates)
migration <- as.data.frame(migration)

PROV <- c()
CASES_now <- c()
CASES_lag7 <- c()
CASES_lag4 <- c()
CASES_lag7_hub <- c()
CASES_lag4_hub <- c()
MOB <- c()
TEST <- c()
DATE <- c()
MOB_IND <- c()
POPS <- c()
POPS <- c()
CASES_travel_now <- c()
CASES_lag1_hub <- c()
CASES_CUMULATIVE <- c()
CASES_CUMULATIVE_jhu <- c()

for(i in 1:length(dates)){
  names.i <- unlist(lapply(mobility_data[,prop_cols[i]-1], as.character))
  
  #get hubei cases
  use_hub.i <- which(line_list$min_dat == dates[i] & line_list$province == "Hubei")
  by_hub_sum.i <- by(data = line_list$sum[use_hub.i], INDICES = line_list$province[use_hub.i], FUN = mean, na.rm = TRUE) 
  hubei_cases[i,] <- as.numeric(by_hub_sum.i[["Hubei"]])
  if(is.na(hubei_cases[i,]) == TRUE){
    hubei_cases[i,] <- 0
  }
  
  #new cases line list
  use_sum.i <- which(line_list$min_dat == dates[i])
  by_max_sum.i <- by(data = line_list$sum[use_sum.i], INDICES = line_list$province[use_sum.i], FUN = max, na.rm = TRUE)  
  by_max_sum.i[which(is.na(by_max_sum.i) == TRUE)] <- 0
  mt_sum.i <- match(names(by_max_sum.i), names.i)
  mob_sum.i <- unlist(lapply(mobility_data[mt_sum.i,prop_cols[i]], as.numeric))
  migration[i,names(by_max_sum.i)] <- mob_sum.i
  cases[i,names(by_max_sum.i)] <- by_max_sum.i
  
  #new travel cases
  use_travel.i <- which(line_list$min_dat == dates[i] & line_list$wuhan_travel == "Wuhan travel" & line_list$province != "Hubei")
  by_max_travel.i <- by(data = line_list$sum[use_travel.i], INDICES = line_list$province[use_travel.i], FUN = max, na.rm = TRUE) 
  by_max_travel.i[which(is.na(by_max_travel.i) == TRUE)] <- 0
  
  #cumulative cases (line list)
  mt.line.i <- match(names(by_max_sum.i), colnames(cases))
  cases_cumulative.i <- colSums(cases[1:i,mt.line.i], na.rm = TRUE)
  cases_cumulative.i["Hubei"] <- sum(hubei_cases[1:i, "Hubei"], na.rm = TRUE)
  
  #cumulative cases (jhu)
  use_sum_jhu.i <- which(substr(province_data$`Last Update`, 1, 10) == dates[i])
  if(length(use_sum_jhu.i) == 0){
    cases_cumulative.jhu.i <- rep(NA, length(names(by_max_sum.i)))
  }else{
    by_max_jhu.i <- by(data = province_data$Confirmed[use_sum_jhu.i], INDICES = province_data$`Province/State`[use_sum_jhu.i], FUN = mean, na.rm = TRUE) 
    mt_jhu.i <- match(names(by_max_sum.i), names(by_max_jhu.i))
    cases_cumulative.jhu.i <- by_max_jhu.i[mt_jhu.i]
  }
 
  if(length(by_max_travel.i) != length(by_max_sum.i)){
    stop()
  }
  
  if(length(which(names(by_max_travel.i) != names(by_max_sum.i))) > 0){
    stop()
  }
  
  if(dates[i] > test_start){
    test.i <- rep("Yes", length(by_max_sum.i))
  }else{
    test.i <- rep("No", length(by_max_sum.i))
  }
  
  if(dates[i] > incubation_end){
    mobind.i <- rep("Yes", length(by_max_sum.i))
  }else{
    mobind.i <- rep("No", length(by_max_sum.i))
  }
  
  if(i > 4){
    cases.lag4.i <- cases[i-4,mt.line.i]
    cases.lag4.hub.i <- rep(hubei_cases[i-4,"Hubei"], length(by_max_sum.i))
  }else{
    cases.lag4.i <- rep(NA, length(by_max_sum.i))
    cases.lag4.hub.i <- rep(NA, length(by_max_sum.i))
  }
  
  if(i > 7){
    cases.lag.i <- cases[i-7,mt.line.i]
    cases.lag.hub.i <- rep(hubei_cases[i-7,"Hubei"], length(by_max_sum.i))
  }else{
    cases.lag.i <- rep(NA, length(by_max_sum.i))
    cases.lag.hub.i <- rep(NA, length(by_max_sum.i))
  }
  
  if(i > 1){
    cases.lag.hub.1.i <- rep(hubei_cases[i-1,"Hubei"], length(by_max_sum.i))
  }else{
    cases.lag.hub.1.i <- rep(NA, length(by_max_sum.i))
  }
  
  popmatch.i <- match(names(by_max_sum.i), pops$Province)
  pops.i <- pops$X2017[popmatch.i]
  
  if(length(cases.lag.hub.i) != length(cases.lag.hub.1.i)){
    stop()
  }
  
  if(length(names(by_max_sum.i)) != length(as.numeric(cases.lag.i)) | length(names(by_max_sum.i)) != length(mob_sum.i) | length(names(by_max_sum.i)) != length(test.i) | length(cases_cumulative.i) != length(as.numeric(cases.lag.i)) | length(cases_cumulative.jhu.i) != length(cases_cumulative.i)){
    stop()
  }
  
  CASES_CUMULATIVE_jhu <- c(CASES_CUMULATIVE_jhu, cases_cumulative.jhu.i)
  CASES_CUMULATIVE <- c(CASES_CUMULATIVE, cases_cumulative.i)
  PROV <- c(PROV, names(by_max_sum.i))
  CASES_travel_now <- c(CASES_travel_now, as.numeric(by_max_travel.i))
  CASES_now <- c(CASES_now, as.numeric(by_max_sum.i))
  CASES_lag4 <- c(CASES_lag4, as.numeric(cases.lag4.i))
  CASES_lag7 <- c(CASES_lag7, as.numeric(cases.lag.i))
  MOB <- c(MOB,mob_sum.i)
  MOB_IND <- c(MOB_IND, mobind.i)
  TEST <- c(TEST, test.i)
  DATE <- c(DATE, rep(as.character(dates[i]), length(by_max_sum.i)))
  POPS <- c(POPS, pops.i)
  CASES_lag4_hub <- c(CASES_lag4_hub, cases.lag.hub.i)
  CASES_lag7_hub <- c(CASES_lag7_hub, cases.lag.hub.i)
  CASES_lag1_hub <- c(CASES_lag1_hub, cases.lag.hub.1.i)
}

MOB <- log(MOB)
POPS <- log(POPS)
dat.combine <- data.frame(DATE, CASES_CUMULATIVE, CASES_CUMULATIVE_jhu, CASES_lag6, CASES_travel_now, CASES_now, TEST, PROV, MOB, MOB_IND, POPS, CASES_lag6_hub, CASES_lag1_hub)
dat.combine$DATE <- as.POSIXct(strptime(as.character(dat.combine$DATE), format = "%Y-%m-%d"))

if(save_new == TRUE){
  saveRDS(dat.combine, file = paste0("../data/", time_stamp, "_full_data.RData"))
}