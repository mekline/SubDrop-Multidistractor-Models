#Analysis of the SubjectDrop study!

#Reading in all libraries that we'll use
library(irr)
library(stringr)
library(languageR)
library(lme4)
library(multcomp)
library(binom)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
sum.na.rm <- function(x) { sum(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))

#Get directory of this file
directory = getwd()

#Initialize dataset
subtable = data.frame(NULL)

#Load csv with Alldata into variable
subtable = read.csv(paste0(directory, "/SubDropSpeakers_Data.csv"), header = TRUE, stringsAsFactors = FALSE)


#Fix some NA columns
subtable[is.na(subtable$Strict.include),]$Strict.include <- 0
subtable$Kid.Response.A...Prag.Choice. <- as.character (subtable$Kid.Response.A...Prag.Choice.)
subtable$Kid.Response.B...Prag.Choice. <- as.character (subtable$Kid.Response.B...Prag.Choice.)

subtable[is.na(subtable)] <- 0
#subtable[is.na(subtable$Include.subject),]$Include.subject <- 0

#Drop non-included kids!
subtable <- subtable[subtable$Include.subject. == "1",]

#Drop early version of KidSecret!
subtable <- subtable[subtable$Experiment != "KidSecret",]

#How many kids of each Age, Experiment, Condition?
with(subtable, tapply(as.numeric(as.character(Include.subject.)), list(Experiment, Condition, Age.Years), sum.na.rm), drop=TRUE)

#Get info for individual sub-experiments (good for updating 'subjects needed' on ongoing exps)
subtable$Age.Years <- as.numeric(as.character(subtable$Age.Years))
subtable$Gender <- subtable$Gender..Guessed.from.Name.Appearance.
PSecret <- subtable[subtable$Experiment == "ParentSecret",]
KSecret <- subtable[subtable$Experiment == "KidSecret-New",]

Parent56 <- PSecret[PSecret$Age.Years >4,]
Parent34 <- PSecret[PSecret$Age.Years < 5,]
Kid56 <- KSecret[KSecret$Age.Years>4,]

with(Parent34, tapply(as.numeric(as.character(Include.subject.)), list(Condition, Age.Years, Gender), sum.na.rm), drop=TRUE)
with(Kid56, tapply(as.numeric(as.character(Include.subject.)), list(Condition, Age.Years, Gender), sum.na.rm), drop=TRUE)




#Add new columns to encode 'correctness!'
#SD: 'subject drop' is the 'correct answer', other name for this condition is 'two fruits'
#OD: aka 'two animals'

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

subtable$pragChoiceScore <- subtable$isPragChoiceA + subtable$isPragChoiceB

subtable$choseObjectDrop <- subtable$pragChoiceScore
subtable[subtable$Condition == "SD",]$choseObjectDrop <- 2-subtable[subtable$Condition == "SD",]$pragChoiceScore

#Just ParentSecret!

subtable <- subtable[subtable$Experiment == "ParentSecret",]
#Just the big kids
subtable <- subtable[subtable$Age.Years > 4,]

#OK, do some descriptives!

table(subtable$Condition, subtable$choseObjectDrop)
table(subtable$Condition, subtable$pragChoiceScore)

#Can we do a logistic regression on this one? I don't see why not!

#First make some necessary things factors, and melt the dataset

subtable$Condition <- as.factor(subtable$Condition)
subtable$Subject <- as.factor(subtable$Subject..)
subtable$choseObjectDrop <- as.factor(subtable$choseObjectDrop)
subtable$pragChoice_1 <- subtable$isPragChoiceA
subtable$pragChoice_2 <- subtable$isPragChoiceB

sub.long = wideToLong(subtable,within="trial", sep='_')
sub.long$choseObjectDrop <- sub.long$pragChoice
sub.long[sub.long$Condition == "SD",]$choseObjectDrop <- 1-sub.long[sub.long$Condition == "SD",]$pragChoice


full_maximal_model <- lmer(choseObjectDrop ~ Condition + (Condition|trial), data=sub.long, family="binomial")
summary(full_maximal_model)

no_fixed <- lmer(choseObjectDrop ~ 1 + (Condition|trial), data=sub.long, family="binomial")
anova(full_maximal_model, no_fixed)
