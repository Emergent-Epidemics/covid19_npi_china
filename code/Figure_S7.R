# Figure S7

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(dplyr)
library(stringr)

df <- read.csv('../data/data_figure_s7.csv', stringsAsFactors = F)


#Check no NA's or empty here
table(df$wuhan.0._not_wuhan.1.)

str(df)
df$age
df$age_numerical_only<- as.numeric(as.character(df$age))

df <- df %>%
  mutate_if(is.character, str_trim)

#ggplot(df, aes(age_numerical_only, fill = as.factor(wuhan.0._not_wuhan.1.)))+geom_histogram(binwidth = 5)+theme_bw()


#Should check NA's aren't being produced, and date format is correct
df$date_confirmation<- as.Date(df$date_confirmation, format='%d.%m.%Y')
df$date_admission_hospital<- as.Date(df$date_admission_hospital, format='%d.%m.%Y')
df$date_onset_symptoms<- as.Date(df$date_onset_symptoms, format='%d.%m.%Y')
onset_admission_interval <- df$date_admission_hospital-df$date_onset_symptoms
df$onset_admission_interval <- df$date_admission_hospital-df$date_onset_symptoms


# travel
age_early_travel <- subset(df, travel_history_location_binary == 1 & date_confirmation <= as.Date("2020-01-31"))
median(age_early_travel$age_numerical_only, na.rm=T)
quantile(age_early_travel$age_numerical_only, probs = c(0.25, 0.75), na.rm= T) # quartile

age_late_travel <- subset(df, travel_history_location_binary == 1 & date_confirmation > as.Date("2020-01-31"))
median(age_late_travel$age_numerical_only, na.rm=T)
quantile(age_late_travel$age_numerical_only, probs = c(0.25, 0.75), na.rm= T) # quartile

# not travel
age_early <- subset(df, travel_history_location_binary == 0 & date_confirmation <= as.Date("2020-01-31"))
median(age_early$age_numerical_only, na.rm=T)
quantile(age_early$age_numerical_only, probs = c(0.25, 0.75), na.rm= T) # quartile

age_late <- subset(df, travel_history_location_binary == 0 & date_confirmation > as.Date("2020-01-31"))
median(age_late$age_numerical_only, na.rm=T)
quantile(age_late$age_numerical_only, probs = c(0.25, 0.75), na.rm= T) # quartile


# statistical test
early <- as.data.frame(age_early$age_numerical_only)
early <- cbind(early, 'early')
colnames(early) <- c('age', 'class')

late <- as.data.frame(age_late$age_numerical_only)
late <- cbind(late, 'late')
colnames(late) <- c('age', 'class')

tmp <- rbind(late, early)
tmp <- na.omit(tmp)
t.test(age ~ class, data = tmp)

early_travel <- as.data.frame(age_early_travel$age_numerical_only)
early_travel <- cbind(early_travel, 'early_travel')
colnames(early_travel) <- c('age', 'class')

late_travel <- as.data.frame(age_late_travel$age_numerical_only)
late_travel <- cbind(late_travel, 'late_travel')
colnames(late_travel) <- c('age', 'class')

# put them all together and plot Figure S7
tmp <- rbind(late_travel, early_travel, late, early)

p <- ggplot(tmp, aes(x=class, y=age, fill = class)) + 
  #geom_violin(trim=FALSE)+
  #geom_dotplot(binaxis='y', stackdir='center',
  #            position=position_dodge(1))+
  geom_boxplot(width=0.1)

p + scale_fill_brewer(palette="Dark2") + theme_minimal() + geom_boxplot(width=0.1) 

