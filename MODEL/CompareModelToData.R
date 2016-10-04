c <rm (list = ls(all = TRUE)) # Clean everything
options(warn=-1) # Boot.ci() keeps whining about needing variances for t-intervals. But we don't use those.

# Load packages
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(stringr)

# Set your working directory!!!


######
# Load data
######

hdata <- read.csv("Human_Split_Data.csv")
mdata <- read.csv("Models_from_splitdata.csv")
#This has the probabilities that a word (S,V,O) is included in various conditions; for humans we also
#have the 95% boostrapped confidence intervals; see main data analysis for those values and the m files for the 
#model derivations.

#melt this into a nicer shape! (row = Condition + target word + data half, then human, model judgments, matched crossways!)
hdata$CompSet <- ""
hdata[hdata$Source == "Human.1",]$CompSet <- "A"
hdata[hdata$Source == "Human.2",]$CompSet <- "B"

mdata$CompSet <- ""
mdata[str_detect(mdata$Source, "1"),]$CompSet <- "B"
mdata[str_detect(mdata$Source, "2"),]$CompSet <- "A"

alldata <- rbind(hdata, mdata)
alldata$Source2 <- ""
alldata[str_detect(alldata$Source, "Human"),]$Source2 <- "Human"
alldata[str_detect(alldata$Source, "Dummy"),]$Source2 <- "Dummy"
alldata[str_detect(alldata$Source, "IfBase"),]$Source2 <- "IfBase"
alldata[str_detect(alldata$Source, "IfNoBase"),]$Source2 <- "IfNoBase"
alldata[str_detect(alldata$Source, "SucceedOrFail"),]$Source2 <- "SucceedOrFail"
alldata$Source <- alldata$Source2

alldata <- alldata %>%
  dplyr::select(one_of(c("Source","Word","Condition","CompSet", "pInclude"))) %>%
  spread(Source, pInclude) 
#########
# Get the model correlations!
#########

setA <- filter(alldata, CompSet == "A")
setB <- filter(alldata, CompSet == "B")
rcorr(setA$Human, setA$Dummy)
rcorr(setA$Human, setA$SucceedOrFail)
rcorr(setA$Human, setA$IfNoBase)
rcorr(setA$Human, setA$IfBase)

rcorr(setB$Human, setB$Dummy)
rcorr(setB$Human, setB$SucceedOrFail)
rcorr(setB$Human, setB$IfNoBase)
rcorr(setB$Human, setB$IfBase)

#(all the data, for all comparisons, easiest for reporting in paper
#(this is ok because same amt data/model))
rcorr(alldata$Human, alldata$Dummy)
rcorr(alldata$Human, alldata$SucceedOrFail)
rcorr(alldata$Human, alldata$IfNoBase)
rcorr(alldata$Human, alldata$IfBase)


########
# GRAPHS
########

#Average all the predictions between halves

toGraph <- alldata %>%
  group_by(Word, Condition) %>%
  summarise_each(funs(mean))

toGraph[toGraph$SucceedOrFail == 0,]$SucceedOrFail <- 0.01 #Make the bars show up!

library(RColorBrewer)
my.cols <- brewer.pal(9, "Purples")
my.cols <- rev(my.cols[4:9])

#pretty labels
trials <- list("1_6","2_5","3_4","4_3","5_2","6_1")
trial_label <- list("1 agent, 6 patients", "2 agents, 5 patients","3 agents, 4 patients","4 agents, 3 patients","5 agents, 2 patients","6 agents, 1 patient")
toGraph$trialLabel <- ""
for (i in 1:6){
  toGraph[(toGraph$Condition == trials[[i]]),]$trialLabel <- trial_label[[i]] 
}

toGraph$Word <- factor(toGraph$Word, levels = c("Subject", "Object","Verb"))
toGraph <- gather(toGraph, "Model", "pInclude", one_of(c("Dummy", "SucceedOrFail","IfNoBase","IfBase")))

toGraph$modelLabel = ""
toGraph[toGraph$Model == "Dummy",]$modelLabel <- "Baserate only"
toGraph[toGraph$Model == "SucceedOrFail",]$modelLabel <- "Succeed/fail"
toGraph[toGraph$Model == "IfNoBase",]$modelLabel <- "Informative"
toGraph[toGraph$Model == "IfBase",]$modelLabel <- "Informative + Baserate"
toGraph$modelLabel <- factor(toGraph$modelLabel, levels = c("Succeed/fail","Informative", "Baserate only", "Informative + Baserate"))

ggplot(data=toGraph, aes(x=Word, y=pInclude, fill=trialLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  xlab('') +
  ylab('p(include word)') +
  theme(legend.key = element_blank()) +
  theme(strip.background = element_blank()) +
  theme_bw() +
  scale_fill_manual(name="", values=my.cols) +
  theme(text = element_text(family="Times", size=rel(4))) +
  theme(legend.text = element_text(family="Times", size=rel(4))) +
  theme(axis.text = element_text(family="Times", size=rel(0.9))) +
  facet_grid(. ~ modelLabel) +
  theme(strip.text = element_text(family="Times", size=rel(0.9))) 

ggsave(filename="allmodels.jpg", width=10, height=4)
