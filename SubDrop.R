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
subtable = read.csv(paste0(directory, "/SubDrop Speakers 2014 data.csv"), header = TRUE)

#Drop non-included kids!
subtable <- subtable[subtable$Include.subject. == "1",]

#How many kids of each Age, Experiment, Condition?
with(subtable, tapply(as.numeric(as.character(Include.subject.)), list(Experiment, Condition, Age.Years), sum.na.rm), drop=TRUE)



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

#Just the big kids
subtable <- subtable[subtable$Age.Years > 4,]

#OK, do some calcs!

table(subtable$Condition, subtable$choseObjectDrop)
table(subtable$Condition, subtable$pragChoiceScore)

