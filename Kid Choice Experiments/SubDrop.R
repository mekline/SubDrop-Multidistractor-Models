#Analysis of the SubjectDrop study!

#setwd(mydir)

#Reading in all libraries that we'll use
library(irr)
library(stringr)
library(languageR)
library(lme4)
library(multcomp)
library(binom)
library(dplyr)
library(lsr)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
sum.na.rm <- function(x) { sum(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))

#Get directory of this file
directory = getwd()

#Initialize dataset
subtable = data.frame(NULL)

#Load csv with Alldata into variable
subtable = read.csv(paste0(directory, "/SubDropSpeakers_Data_32115_thesis.csv"), header = TRUE, stringsAsFactors = FALSE)


#Fix some NA columns
subtable[is.na(subtable$Strict.include),]$Strict.include <- 0
subtable$Kid.Response.A...Prag.Choice. <- as.character (subtable$Kid.Response.A...Prag.Choice.)
subtable$Kid.Response.B...Prag.Choice. <- as.character (subtable$Kid.Response.B...Prag.Choice.)

subtable[is.na(subtable)] <- 0
#subtable[is.na(subtable$Include.subject),]$Include.subject <- 0

####################################
#Pick subset of data to analyze

#Drop non-included kids!
subtable <- subtable[subtable$Include.subject. == "1",]
#New - choose stricter inclusion criteria.., following new paradigm rules
subtable <- subtable[subtable$Strict.include == 1,]

#Choose ParentSecret on older children
subtable <- subtable[subtable$Experiment == "ParentSecret",]
subtable <- subtable[subtable$Age.Years > 4,]



#Look at n kids in sub-experiments (this is good for checking updates on n subjects needed per condition)
#How many kids of each Age, Experiment, Condition?

subtable$Age.Years <- as.numeric(as.character(subtable$Age.Years))
subtable$Gender <- subtable$Gender..Guessed.from.Name.Appearance.

with(subtable, tapply(as.numeric(as.character(Strict.include)), list(Condition, Age.Years), sum.na.rm), drop=TRUE)





#############################################
# Recode variables

#SD: 'subject drop' is the 'correct answer', other name for this condition is 'two fruits'
#OD: aka 'two animals'

#Note- we tried 2-trial and 4-trial versions of the task.  With within-subj 4-trial version, we saw big carryover effects. So, analyze just 1st 2 trials
#from all versions togther.

subtable$oldCond <- subtable$Condition
subtable[subtable$Condition == "SDOD",]$Condition <- "SD"
subtable[subtable$Condition == "SDSD",]$Condition <- "SD"
subtable[subtable$Condition == "ODSD",]$Condition <- "OD"
subtable[subtable$Condition == "ODOD",]$Condition <- "OD"

subtable$isPragChoiceA <- "NA"
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "monkey eat",]$isPragChoiceA <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "eat orange",]$isPragChoiceA <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "monkey eat",]$isPragChoiceA <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "eat orange",]$isPragChoiceA <- 0

subtable$isPragChoiceB <- "NA"
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "girl pet",]$isPragChoiceB <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet dog",]$isPragChoiceB <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "girl pet",]$isPragChoiceB <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "pet dog",]$isPragChoiceB <- 0

subtable$isPragChoiceA <- as.numeric(as.character(subtable$isPragChoiceA))
subtable$isPragChoiceB <- as.numeric(as.character(subtable$isPragChoiceB))

#Express this as # chose 'correct'
subtable$pragChoiceScore <- subtable$isPragChoiceA + subtable$isPragChoiceB

#...Or as # chose to drop the object
subtable$choseObjectDrop <- subtable$pragChoiceScore
subtable[subtable$Condition == "SD",]$choseObjectDrop <- 2-subtable[subtable$Condition == "SD",]$pragChoiceScore


####################################
#Descriptive stats for graph (Developmental, small sample, so we'll present hist. of kids choosing each asnwer, rather than proportion scores)

table(subtable$Condition, subtable$choseObjectDrop)
table(subtable$Condition, subtable$pragChoiceScore)


####################################
# Analysis

#First make sure factors are coded correctly, and melt the dataset

subtable$Condition <- as.factor(subtable$Condition)
subtable$Subject <- as.factor(subtable$Subject..)
subtable$choseObjectDrop <- as.factor(subtable$choseObjectDrop)
subtable$pragChoice_1 <- subtable$isPragChoiceA
subtable$pragChoice_2 <- subtable$isPragChoiceB

sub.long = wideToLong(subtable,within="trial", sep='_')
sub.long$choseObjectDrop <- sub.long$pragChoice
sub.long[sub.long$Condition == "SD",]$choseObjectDrop <- 1-sub.long[sub.long$Condition == "SD",]$pragChoice

# Logistic Regression model.  No (Condition|Subject) random effect because condition was varied between subjects
#full_maximal_model <- lmer(choseObjectDrop ~ Condition + (Condition|trial) + (1|Subject), data=sub.long, family="binomial")
#Doesn't converge! Remove random effects
full_maximal_model <- lmer(choseObjectDrop ~ Condition + (1|trial) + (1|Subject), data=sub.long, family="binomial")

#compare to model w/o fixed effect
no_fixed <- lmer(choseObjectDrop ~ 1 + (1|trial) + (1|Subject), data=sub.long, family="binomial")
anova(full_maximal_model, no_fixed)

