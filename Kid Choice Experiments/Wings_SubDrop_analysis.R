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

####### How many questions did the children get right? ######
# Recode condition variables

#SD: 'subject drop' is the 'correct answer'
#OD: 'object drop' is the 'correct answer'

subtable$oldCond <- subtable$Condition
subtable[subtable$Condition == "SDOD",]$Condition <- "SD"
subtable[subtable$Condition == "ODSD",]$Condition <- "OD"

####Did they get it right? Coding: 1 for right, 0 for wrong.
#Stories 1 and 2 (can be OD or SD)
subtable$isPragChoiceA <- "NA"
#Right
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "eat orange",]$isPragChoiceA <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "monkey eat",]$isPragChoiceA <- 1
#Wrong
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "monkey eat",]$isPragChoiceA <- 0
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "eat orange",]$isPragChoiceA <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.A...Prag.Choice. == "eat banana",]$isPragChoiceA <- 0
subtable[subtable$Condition == "OD" & subtable$Kid.Response.A...Prag.Choice. == "duck eat",]$isPragChoiceA <- 0

#Stories 3 and 4 (can be OD or SD)
subtable$isPragChoiceB <- "NA"
#Right
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet dog",]$isPragChoiceB <- 1
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "girl pet",]$isPragChoiceB <- 1
#Wrong
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "girl pet",]$isPragChoiceB <- 0
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "pet dog",]$isPragChoiceB <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet cat",]$isPragChoiceB <- 0
subtable[subtable$Condition == "SD" & subtable$Kid.Response.B...Prag.Choice. == "pet kitty",]$isPragChoiceB <- 0 #lexical alternative!
subtable[subtable$Condition == "OD" & subtable$Kid.Response.B...Prag.Choice. == "boy pet",]$isPragChoiceB <- 0

#Total number correct
subtable$pragChoiceScore <- subtable$isPragChoiceA + subtable$isPragChoiceB

