# Figure 4

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggplot2)
library(ggalt)
theme_set(theme_classic())
#devtools::install_github("hrbrmstr/ggalt")

df <- read.csv('../data/data_figure_4d.csv', stringsAsFactors = F)

####

p_2d <- ggplot(df) + geom_segment(aes(x='early', xend='late', y=early, yend=late), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d"))



### statistical test
df <- read.csv('../data/f4_chi_square.csv')
df <- df[-1]
chisq.test(df)

##

# Figure 2c
df <- read.csv('../Data/data_figure_4c.csv', stringsAsFactors = F)

####
p_2c <- ggplot(df) + geom_segment(aes(x='early', xend='late', y=early, yend=late), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d"))