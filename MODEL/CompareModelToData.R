c <rm (list = ls(all = TRUE)) # Clean everything
options(warn=-1) # Boot.ci() keeps whining about needing variances for t-intervals. But we don't use those.

# Load packages
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(stringr)
library(RColorBrewer)

# Set your working directory!!!


######
# Load data
######

hdata <- read.csv("humandata.csv")
model.files <- list.files(pattern = '*[A-B].csv')
mdata <- data.frame(NULL)

for(f in model.files) {
  tmp <- read.csv(f, header=F)
  filena <- strsplit(f,c('.'), fixed=TRUE)[[1]][1]
  tmp$Source <- substr(filena, 0, nchar(filena)-1)
  tmp$half <- substr(filena, nchar(filena), nchar(filena))
  tmp$condition <- c('1_6','2_5','3_4','4_3','5_2', '6_1')
  mdata <- rbind(mdata, tmp)
}

#Those are the probabilities that a particular 2 word solution is used by a human or model
#(Note that for model comparisons we just use the trials where people produced exactly 2 of [S,V,O])

#melt mdata & hdata to match
hdata <- hdata %>%
  select(-X) %>%
  mutate(Source = "human")

hdata$half <- as.character(hdata$half)
hdata$halftoCompare <- ""
hdata[hdata$half == 'r1',]$halftoCompare <- "B" #humans and models reverse paired!
hdata[hdata$half == 'r2',]$halftoCompare <- "A"

mdata$halftoCompare <- mdata$half
  
names(mdata) <- c("SV","VO","SO", "Source","half","condition", "halftoCompare")
mdata <- mdata %>%
  gather("element","p_include", 1:3) 

alldata <- rbind(hdata,mdata)

#alternate form for comparing 'include word' probs
worddata <- alldata %>%
  select(-half) %>%
  spread(element, p_include) %>%
  mutate(Agent = (SO + SV)/(SV + VO + SO)) %>%
  mutate(Patient = (VO + SO)/(SV + VO + SO)) %>%
  mutate(Verb = (VO + SV)/(SV + VO + SO)) %>%
  select(-one_of('SO','SV','VO')) %>%
  gather('Word', 'p_include', c(4:6)) %>%
  spread(Source, p_include)

#For comparing utts
alldata <- alldata %>%
  select(-half) %>%
  spread(Source, p_include)
  
#########
# Get the model correlations!
#########
#(all the data, for all comparisons, easiest for reporting in paper
#(this is ok because same amt data/eachmodel))
#Note, this is by SV, VO, SO responses, again using only the humandata from 'good' answers
rcorr(alldata$human, alldata$dummycost)
rcorr(alldata$human, alldata$succeedorfail_nobase)
rcorr(alldata$human, alldata$succeedorfail)
rcorr(alldata$human, alldata$informative_nobaserate)
rcorr(alldata$human, alldata$informative_baserate)

#We could try it with transformed 'p(include word)' probs too
rcorr(worddata$human, worddata$dummycost)
rcorr(worddata$human, worddata$succeedorfail_nobase)
rcorr(worddata$human, worddata$succeedorfail)
rcorr(worddata$human, worddata$informative_nobaserate)
rcorr(worddata$human, worddata$informative_baserate)


########
# GRAPHS
########

#Average all the predictions between halves, then renormalize to 
#'likelihood utterance includes X' format we use for the full human dataaset

toGraph <- alldata %>%
  group_by(condition, element) %>%
  summarise_each(funs(mean)) %>%
  gather("model", "prediction", 4:9) %>%
  spread("element", "prediction") %>%
  select(-halftoCompare) %>%
  mutate(Agent = (SO + SV)/(SV + VO + SO)) %>%
  mutate(Patient = (VO + SO)/(SV + VO + SO)) %>%
  mutate(Verb = (VO + SV)/(SV + VO + SO)) #%>%
  #filter(model != 'human') #leave humans off this graph

### To make the graphs pretty
toGraph[toGraph$Agent == 0,]$Agent <- 0.01 #Make the bars show up!
toGraph[toGraph$Patient == 0,]$Patient <- 0.01

my.cols <- brewer.pal(9, "Purples")
my.cols <- rev(my.cols[4:9])

#pretty labels
trials <- list("1_6","2_5","3_4","4_3","5_2","6_1")
trial_label <- list("1 agent, 6 patients", "2 agents, 5 patients","3 agents, 4 patients","4 agents, 3 patients","5 agents, 2 patients","6 agents, 1 patient")
toGraph$trialLabel <- ""
for (i in 1:6){
  toGraph[(toGraph$condition == trials[[i]]),]$trialLabel <- trial_label[[i]] 
}

toGraph <- toGraph %>% 
  gather('Word','prob', 6:8)

toGraph$modelLabel = ""
toGraph[toGraph$model == "dummycost",]$modelLabel <- "Cost only"
toGraph[toGraph$model == "succeedorfail_nobase",]$modelLabel <- "Succeed/Fail"
toGraph[toGraph$model == "succeedorfail",]$modelLabel <- "Succeed/Fail & Cost"
toGraph[toGraph$model == "informative_nobaserate",]$modelLabel <- "Rational Speaker"
toGraph[toGraph$model == "informative_baserate",]$modelLabel <- "Rational Speaker & Cost"
toGraph[toGraph$model == "human",]$modelLabel <- "Human Performance"
toGraph$modelLabel <- as.factor(toGraph$modelLabel)
toGraph$modelLabel <- factor(toGraph$modelLabel,levels = c("Cost only","Succeed/Fail", "Succeed/Fail & Cost", "Rational Speaker", "Rational Speaker & Cost", "Human Performance"))       

#Version without cost models for new paper draft
toGraph2 <- filter(toGraph, model %in% c("dummycost", "succeedorfail_nobase", "informative_nobaserate", "human"))
toGraph <- toGraph2
ggplot(data=toGraph, aes(x=Word, y=prob, fill=trialLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  xlab('') +
  ylab('p(include word)') +
  theme(strip.background = element_blank()) +
  theme_bw() +
  #theme(legend.key = element_blank()) +
  theme(legend.position = 'none') +
  scale_fill_manual(name="", values=my.cols) +
  theme(text = element_text(family="Times", size=rel(4))) +
  #theme(legend.text = element_text(family="Times", size=rel(4))) +
  theme(axis.text = element_text(family="Times", size=rel(0.9))) +
  facet_wrap( ~ modelLabel, ncol=2) +
  theme(strip.text = element_text(family="Times", size=rel(0.9))) 

ggsave(filename="threemodels_nolegend.jpg", width=11, height=8)

#Why is the correlation between succeedorfail as high as it is? Melissa suspects simpsons paradoxes

toGraph <- toGraph %>%
  #filter(model == 'informative_nobaserate' | model == 'human') %>%
  select(-one_of(c('SV', 'VO', 'SO'))) %>%
  unite(model, modelLabel) %>%
  spread(model, prob) %>%
  select(-dummycost) %>%
  select(-succeedorfail_nobase) %>%
  gather('WhichModel', 'modelprob',c(5,6,7))

toGraph$humanLabel = "Human Performance"
toGraph$modelLabel = ""
toGraph[toGraph$WhichModel == "succeedorfail",]$modelLabel <- "Succeed/Fail & Cost"
toGraph[toGraph$WhichModel == "informative_nobaserate",]$modelLabel <- "Rational Speaker"
toGraph[toGraph$WhichModel == "informative_baserate",]$modelLabel <- "Rational Speaker & Cost"
toGraph$modelLabel <- as.factor(toGraph$modelLabel)
toGraph$modelLabel <- factor(toGraph$modelLabel,levels = c("Succeed/Fail & Cost","Rational Speaker",  "Rational Speaker & Cost"))       


ggplot(data=toGraph, aes(x=human, y=modelprob)) + 
  geom_point(position=position_dodge(), stat="identity", aes(color=Word)) +
  geom_smooth(method="lm", color='gray', se=FALSE) +
  geom_smooth(method="lm", aes(color=Word), se=FALSE) +
  coord_cartesian(ylim=c(0,1)) +
  coord_cartesian(xlim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5)) +
  scale_x_continuous(breaks = seq(0, 1, 0.5)) +
  facet_grid(. ~ modelLabel) +
  xlab('Human performance: p(include word)') +
  ylab('Model performance: p(include word)') +
  theme(strip.background = element_blank()) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  #scale_color_manual(values=c('red','blue','green'))
  theme(text = element_text(family="Times", size=rel(4))) +
  theme(legend.text = element_text(family="Times", size=rel(4))) +
  theme(axis.text = element_text(family="Times", size=rel(0.9))) +
  theme(strip.text = element_text(family="Times", size=rel(0.9))) +
  theme(legend.position = "bottom")

ggsave(filename="fitlines.jpg", width=9, height=4)

