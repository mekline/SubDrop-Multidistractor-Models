#Analysis of the SubjectDrop study!

#setwd(mydir)

#Reading in all libraries that we (might) use
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
subtable = read.csv(paste0(directory, "/SubDrop_Data.csv"), header = TRUE, stringsAsFactors = FALSE)


#Fix some badly formatted columns
subtable$Kid.Response.A...Prag.Choice. <- as.character (subtable$Kid.Response.A...Prag.Choice.)
subtable$Kid.Response.B...Prag.Choice. <- as.character (subtable$Kid.Response.B...Prag.Choice.)
subtable$Gender <- subtable$Gender..Guessed.from.Name.Appearance.

subtable[is.na(subtable)] <- 0

#Fix age calculations!
subtable$Age.Years <- as.numeric(as.character(subtable$Age.Years))
subtable$Days.Old <- as.numeric(as.character(subtable$Days.Old))

####################################
#Pick subset of data to analyze

#Drop non-included kids!

#Right here would be the place to print a table of exclusion reasons.

#New - chose stricter inclusion criteria.., following new paradigm rules (= inconsistent implementation in 1st run)
subtable <- subtable[subtable$Final.Include == 1,]

#Choose ParentSecret and ParentSecretControl study versions
subtable <- subtable[subtable$Experiment == "ParentSecret" | subtable$Experiment == "ParentSecretControl" | subtable$Experiment == "ParentSecretControl2" ,]


#############################################
# Recode condition variables

#SD: 'subject drop' is the 'correct answer', other name for this condition is 'two fruits'
#OD: aka 'two animals'

#Note- we tried 2-trial and 4-trial versions of the task.  With within-subj 4-trial version, we saw big carryover effects. So, analyze just 1st 2 trials
#from all versions togther.

subtable$oldCond <- subtable$Condition
subtable[subtable$Condition == "SDOD",]$Condition <- "SD"
subtable[subtable$Condition == "SDSD",]$Condition <- "SD"
subtable[subtable$Condition == "ODSD",]$Condition <- "OD"
subtable[subtable$Condition == "ODOD",]$Condition <- "OD"

## Code Correctness!  For main experiment, correctness = chose the pragmatic one; For cont, correctness = chose the correct one! (this is the same! wrong answers differ though)

subtable$isPragChoiceA <- "NA"
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "eat orange",]$isPragChoiceA <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "monkey eat",]$isPragChoiceA <- 1
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "monkey eat",]$isPragChoiceA <- 0
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "eat orange",]$isPragChoiceA <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "eat banana",]$isPragChoiceA <- 0
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "duck eat",]$isPragChoiceA <- 0

subtable$isPragChoiceB <- "NA"
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet dog",]$isPragChoiceB <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "girl pet",]$isPragChoiceB <- 1
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "girl pet",]$isPragChoiceB <- 0
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "pet dog",]$isPragChoiceB <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet cat",]$isPragChoiceB <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet kitty",]$isPragChoiceB <- 0 #lexical alternative!
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "boy pet",]$isPragChoiceB <- 0

#A few kis didn't answer on one trial and will need to be manually dropped
subtable <- subtable[subtable$isPragChoiceA != "NA",]
subtable <- subtable[subtable$isPragChoiceB != "NA",]
subtable$isPragChoiceA <- as.numeric(as.character(subtable$isPragChoiceA))
subtable$isPragChoiceB <- as.numeric(as.character(subtable$isPragChoiceB))



#Express this as # chose 'correct' across experiment
subtable$pragChoiceScore <- subtable$isPragChoiceA + subtable$isPragChoiceB

#...Or as # chose to drop the object
subtable$choseObjectDrop <- subtable$pragChoiceScore
subtable[subtable$Condition == "SD",]$choseObjectDrop <- 2-subtable[subtable$Condition == "SD",]$pragChoiceScore


####################################
#Descriptive stats for graphing (Developmental, small sample, so we'll present hist. of kids choosing each asnwer, rather than proportion scores)

#Time to split up the kids into Main and Control experiments 

maintable <- subtable[subtable$Experiment == "ParentSecret",] 
conttable <- subtable[subtable$Experiment == "ParentSecretControl" | subtable$Experiment == "ParentSecretControl2",] 

#Toss older/younger accidental participant from conttable, it's just for 3-4yos
conttable <- conttable[conttable$Age.Years < 5,]
conttable <- conttable[conttable$Age.Years > 2,]


#Look at n kids in sub-experiments (this is good for checking updates on n subjects needed per condition)
#How many kids of each Age, Experiment, Condition?

with(maintable, tapply(as.numeric(as.character(Final.Include)), list(Condition, Age.Years), sum.na.rm), drop=TRUE)
with(conttable, tapply(as.numeric(as.character(Final.Include)), list(Condition, Age.Years), sum.na.rm), drop=TRUE)

#Make sure factors are coded correctly, and melt the dataset for logistic analyses

maintable$Condition <- as.factor(maintable$Condition)
maintable$Subject <- as.factor(maintable$Subject..)
maintable$choseObjectDrop <- as.factor(maintable$choseObjectDrop)
maintable$pragChoice_1 <- maintable$isPragChoiceA
maintable$pragChoice_2 <- maintable$isPragChoiceB

#Get the objective coding scheme back :)
main.long = wideToLong(maintable,within="trial", sep='_')
main.long$choseObjectDrop <- main.long$pragChoice
main.long[main.long$Condition == "SD",]$choseObjectDrop <- 1-main.long[main.long$Condition == "SD",]$pragChoice

conttable$Condition <- as.factor(conttable$Condition)
conttable$Subject <- as.factor(conttable$Subject..)
conttable$choseObjectDrop <- as.factor(conttable$choseObjectDrop)
conttable$pragChoice_1 <- conttable$isPragChoiceA
conttable$pragChoice_2 <- conttable$isPragChoiceB

#Get the objective coding scheme back :)
cont.long = wideToLong(conttable,within="trial", sep='_')
cont.long$choseObjectDrop <- cont.long$pragChoice
cont.long[cont.long$Condition == "SD",]$choseObjectDrop <- 1-cont.long[cont.long$Condition == "SD",]$pragChoice

#For quick-and-dirty graphs
table(maintable$Condition, maintable$choseObjectDrop)
table(maintable$Age.Years, maintable$pragChoiceScore)

table(conttable$Condition, conttable$pragChoiceScore)
table(conttable$Age.Years, conttable$pragChoiceScore)
####################################
# Analysis!

###
#Question #1 - is choice of OD puppet sensitive to condition?
# Logistic Regression model.  No (Condition|Subject) random effect because condition was varied between subjects
full_maximal_model <- lmer(choseObjectDrop ~ Condition + (Condition|trial) + (1|Subject), data=main.long, family="binomial")

#compare to model w/o fixed effect
no_fixed <- lmer(choseObjectDrop ~ 1 + (Condition|trial) + (1|Subject), data=main.long, family="binomial")
anova(full_maximal_model, no_fixed)

#Answer #1 marginal (with all ages included)

###
#Question 2: Does age influence choice patterns?

#Scale age (z score), to avoid convergence problems (this worked on some datasets)
main.long$Scaled.Days.Old <- scale(main.long$Days.Old)

# Logistic Regression model.  No (Condition|Subject) random effect because condition was varied between subjects
full_maximal_age_model <- lmer(choseObjectDrop ~ Condition*Scaled.Days.Old + (Condition|trial) + (1|Subject), data=main.long, family="binomial")
#If that doesn't converge, as it doesn't as of 10/10, remove random effects...
nonmax_age_model <- lmer(choseObjectDrop ~ Condition*Scaled.Days.Old + (1|trial) + (1|Subject), data=main.long, family="binomial")

#Compare to a model without conditionxage interaction, and with same random effects structure as above
no_age <- lmer(choseObjectDrop ~ Condition+Scaled.Days.Old + (1|trial) + (1|Subject), data=main.long, family="binomial")
anova(nonmax_age_model, no_age)

#Clearer to understand: just run correctness age model instead?

#Answer #2 yes there is an effect of age!

#Question 3 What if we consider only older kids? 4yos and up? (Justification - see conttable analyses)

older.long <- main.long[main.long$Age.Years > 4,] 
# Logistic Regression model.  No (Condition|Subject) random effect because condition was varied between subjects
full_maximal_model_older <- lmer(choseObjectDrop ~ Condition + (Condition|trial) + (1|Subject), data=older.long, family="binomial")

#compare to model w/o fixed effect
no_fixed_older <- lmer(choseObjectDrop ~ 1 + (Condition|trial) + (1|Subject), data=older.long, family="binomial")
anova(full_maximal_model_older, no_fixed_older)

#Answer #3 yup


#Question #4 Can the littles do it at all? Here we just want to ask if the choices are on average different from 50% (Remember that 'conditions' here are just two kinds of true/false!)
#(MK goes away and decides the most reasonable way to do this...)

#START HERE XXXXX

#TO INCLUDE: Fisher tests of correctness levels of 3s and 4s in the 2 different experiments. Do 4yos answers informativity qs look like 4yso answering correctness qs?

######Graphs


#Make a pretty dot graph I hope?
Scores <- aggregate(main.long$pragChoice, list(main.long$Subject), sum)
names(Scores) <- c("Subject","Score")
Ages <- main.long[ !duplicated(main.long$Subject), c("Subject","Days.Old")]

foo <- merge(Scores,Ages)

foo$JitScore <- jitter(foo$Score)

plot( foo$Days.Old, foo$JitScore)

