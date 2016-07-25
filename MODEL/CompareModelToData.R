c <rm (list = ls(all = TRUE)) # Clean everything
options(warn=-1) # Boot.ci() keeps whining about needing variances for t-intervals. But we don't use those.

# Load packages
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)

# Set your working directory!!!


######
# Load data
######

mhdata <- read.csv("Human_Model_Data.csv")
#This has the probabilities that a word (S,V,O) is included in various conditions; for humans we also
#have the 95% boostrapped confidence intervals; see main data analysis for those values and the m files for the 
#model derivations.

#melt this into a nicer shape! (row = Condition + target word + human, model judgments)

confints <- mhdata %>%
  dplyr::select(one_of("Condition", "Word", "conf.Upper","conf.Lower")) %>%
  filter(!is.na(conf.Upper))
names(confints) <- c("Condition", "Word", "Human.conf.Upper","Human.conf.Lower")

mhdata <- mhdata %>%
  dplyr::select(one_of(c("Source","Word","Condition","p.Include"))) %>%
  spread(Source, p.Include) %>%
  merge(confints, by=c("Condition","Word"))

#########
# Get the model correlations!
#########
rcorr(as.matrix(select(mhdata,one_of(c("Human","Model.v5")))))
rcorr(as.matrix(select(mhdata,one_of(c("Human","Model.v50")))))

########
# GRAPHS
########

library(RColorBrewer)
my.cols <- brewer.pal(9, "Purples")
my.cols <- rev(my.cols[4:9])

#pretty labels
trials <- list("1_6","2_5","3_4","4_3","5_2","6_1")
trial_label <- list("1 agent, 6 patients", "2 agents, 5 patients","3 agents, 4 patients","4 agents, 3 patients","5 agents, 2 patients","6 agents, 1 patient")
mhdata$trialLabel <- ""
for (i in 1:6){
  mhdata[(mhdata$Condition == trials[[i]]),]$trialLabel <- trial_label[[i]] 
}

mhdata$Word <- factor(mhdata$Word, levels = c("Subject", "Object","Verb"))

ggplot(data=mhdata, aes(x=Word, y=Model.v5, fill=trialLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  xlab('') +
  ylab('p(include word), k=5') +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  scale_fill_manual(name="", values=my.cols) +
  theme(text = element_text(family="Times", size=rel(4))) +
  theme(legend.text = element_text(family="Times", size=rel(4))) +
  theme(axis.text = element_text(family="Times", size=rel(0.9)))

ggsave(filename="v5.jpg", width=10, height=6)

ggplot(data=mhdata, aes(x=Word, y=Model.v50, fill=trialLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  xlab('') +
  ylab('p(include word), k=50') +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  scale_fill_manual(name="", values=my.cols) +
  theme(text = element_text(family="Times", size=rel(4))) +
  theme(legend.text = element_text(family="Times", size=rel(4))) +
  theme(axis.text = element_text(family="Times", size=rel(0.9)))
  
  
ggsave(filename="v50.jpg", width=10, height=6)
