###############################################################
## Library for tedlab: linguistic acceptability rating surveys on Mechanical Turk
## October 2009 Steve Piantadosi
## July, August, September 2010 Ted Gibson
###############################################################

#setwd(mydir)

library(languageR)
library(lme4)
library(stringr)
source("tedlab-misc.R")
mean.na.rm <- function(x) { mean(x,na.rm=T) }

turk.files <- list.files('batch')
willow.files <- list.files('log')
verb.files <- list.files('ratings')
		
###############################################################
## Read in the data files from turk
list.number <- 1;  # keep track of the list number
turkdata <- data.frame(NULL)
for(f in turk.files) {
	tmp <- read.csv(paste('batch/',f, sep=''), header=T)
	tmp$turkfile <- f
	turkdata <- rbind(turkdata, tmp)

	list.number <- list.number + 1
}

dropped.columns <- c("HITId", "HITTypeId","Title", "Description", "Keywords", "Reward", "CreationTime", "MaxAssignments", 	"RequesterAnnotation", "AssignmentDurationInSeconds", "AutoApprovalDelayInSeconds", "Expiration", "NumberOfSimilarHITs", 	"LifetimeInSeconds", "AssignmentId", "AcceptTime", "AutoApprovalTime", "ApprovalTime", "RejectionTime", 	"RequesterFeedback", "AssignmentStatus","LifetimeApprovalRate", "Last30DaysApprovalRate","Last7DaysApprovalRate" )

turkdata <- turkdata[, setdiff(names(turkdata), dropped.columns)]

###############################################################
## Read in the data files from willow
willowdata <- data.frame(NULL)
for (w in willow.files) {
	tmp <- read.csv(paste('log/',w, sep=''), header=T)
	tmp$willowfile <- w
	tmp$willowcode <- paste(unlist(str_extract_all(w,"[0-9+]")),collapse='')
	willowdata <- rbind(willowdata, tmp)
}

#Remove extra header lines...
willowdata <- willowdata[willowdata$paycode != "paycode",]
willowdata$Paycode <- willowdata$paycode

###############################################################
## Read in the data files from verb ratings!!
verbdata <- data.frame(NULL)
for (w in verb.files) {
	tmp <- read.csv(paste('ratings/',w, sep=''), header=T)
	tmp$verbfile <- w
	tmp$verbcode <- paste(unlist(str_extract_all(w,"[0-9+]")),collapse='')
	verbdata <- rbind(verbdata, tmp)
}

#Remove extra header lines...
verbdata <- verbdata[verbdata$paycode != "paycode",]
verbdata$Paycode <- verbdata$paycode

###############################################################
## Merge the information from turk and willow!  

#How many do we start with?
length(unique(willowdata$Paycode)) #112
length(unique(verbdata$Paycode)) #102
length(unique(turkdata$WorkerId)) #100

#Save and standardize
turkdata$Paycode <- turkdata$Answer.payCode
turkdata$Answer.payCode <- NA
turkdata <- turkdata[,setdiff(names(turkdata), c("Answer.payCode"))]
turkdata$HasTurk <- TRUE
willowdata$HasWillow <- TRUE

mydata <- NULL
mydata <- merge(willowdata, turkdata, by=c("Paycode"), all.x=TRUE, all.y=TRUE)
mydata <- merge(mydata, verbdata, by=c("Paycode", "stimNo", "paycode", "verb"), all.x=TRUE, all.y=TRUE)

#If your line doesn't have Willow, you are a useless unmatched Turk entry!

#Check who these jokers are!  Will want to make double sure they have not taken the survey twice...
mydata[is.na(mydata$HasWillow),]$Paycode
mydata[is.na(mydata$HasWillow),]$WorkerId




#################################################################
## Filter participants first for payment then for analysis

## We filter for:
## Completed a previous survey
## Completed fewer than 3/4 of trials
## Nonnative English Speaker, or not from USA
## Had a video problem
## And save WorkerID code lists for the future...


#If your line doesn't have Turk, we'll give you a free pass on language, country, and video presentation
mydata[is.na(mydata$HasTurk),]$Answer.English <- "yes"
mydata[is.na(mydata$HasTurk),]$Answer.country <- "USA"

#################################################################
## Recode and clean data & conditions

#Check for number of legal responses given in a single session!!

mydata[is.na(mydata$mistakeFlag),]$mistakeFlag <- 'bad input'
mydata[mydata$mistakeFlag == '',]$mistakeFlag <- 'bad input' #Fix up missing values

mydata$Error <- 0
mydata[mydata$mistakeFlag == 'bad input',]$Error <- 1


participant.responsecount <- aggregate(mydata$Error, by=list(mydata$Paycode), sum)
names(participant.responsecount) <- c("Paycode", "gotError")
participant.responsecount$LegalAnswers <- 12-participant.responsecount$gotError
mydata <- merge(mydata, participant.responsecount, by=c("Paycode"), all.x=TRUE)

###############################################
## Throw out people who took a Snazzy Potato task before!

previousers <- read.csv('snazzy potato 11-20.txt', header=F)
names(previousers) <- c("WorkerId")
thistime <- unique(mydata$WorkerId)
previous <- intersect(previousers$WorkerId,thistime)
#
##Grr!  Go make sure to not pay those guys!....
#
#
mydata$tookPreviously <- mydata$WorkerId %in% previous
#
mydata <- mydata[mydata$tookPreviously == FALSE,]


#################################################################
## Drop for analysis
#(Remember, Willow-onliers got a free pass on the first 3 here...)

length(unique(mydata$Paycode))

#Print out worker IDs for Snazzy Potato list

unique(mydata$WorkerId)

mydata <- mydata[mydata$Answer.country == "USA" &
		mydata$Answer.English == "yes" &
		mydata$VideoProblem == "FALSE" &
		mydata$LegalAnswers > 8,] #You did at least 3/4 of the trials
		
#Throw out people who only had turk data (no code given...)
mydata <- mydata[!is.na(mydata$HasWillow),]

length(unique(mydata$Paycode)) #91



#################################################################
## Time to analyze the data!


#######
#Code variables
#######

#Code whether each argument is included in the answer!

mydata$mentionSubject <- FALSE
mydata[mydata$word1_CODED == 'AGENT',]$mentionSubject <- TRUE
mydata[mydata$word2_CODED == 'AGENT',]$mentionSubject <- TRUE

mydata$mentionVerb <- FALSE
mydata[mydata$word1_CODED == 'VERB',]$mentionVerb <- TRUE
mydata[mydata$word2_CODED == 'VERB',]$mentionVerb <- TRUE

mydata$mentionObject <- FALSE
mydata[mydata$word1_CODED == 'OBJECT',]$mentionObject <- TRUE
mydata[mydata$word2_CODED == 'OBJECT',]$mentionObject <- TRUE

#Code whether the SV or VO solution was reached
mydata$mentionSV <- FALSE
mydata[mydata$mentionSubject == TRUE & mydata$mentionVerb == TRUE,]$mentionSV <- TRUE

mydata$mentionVO <- FALSE
mydata[mydata$mentionObject == TRUE & mydata$mentionVerb == TRUE,]$mentionVO <- TRUE


################################################

#######
#Descriptives
#######


#Look at overall results
with(mydata, tapply(mentionSubject, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mydata, tapply(mentionObject, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mydata, tapply(mentionVerb, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)

#Get some bootstrapped confidence intervals for these numbers
########
#Lump by subject to get frequency scores!

mentionSubject <- aggregate(mydata$mentionSubject, by = list(mydata$Paycode, mydata$trialVersion), sum)
names(mentionSubject) <- c("Paycode", "trialVersion", "mentionSubject")
mentionObject <- aggregate(mydata$mentionObject, by = list(mydata$Paycode, mydata$trialVersion), sum)
names(mentionObject) <- c("Paycode", "trialVersion", "mentionObject")
mentionVerb <- aggregate(mydata$mentionVerb, by = list(mydata$Paycode, mydata$trialVersion), sum)
names(mentionVerb) <- c("Paycode", "trialVersion", "mentionVerb")

mentionSV <- aggregate(mydata$mentionSV, by = list(mydata$Paycode, mydata$trialVersion), sum)
names(mentionSV) <- c("Paycode", "trialVersion", "mentionSV")

mentionVO <- aggregate(mydata$mentionVO, by = list(mydata$Paycode, mydata$trialVersion), sum)
names(mentionVO) <- c("Paycode", "trialVersion", "mentionVO")

with(mentionSubject, tapply(mentionSubject, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mentionObject, tapply(mentionObject, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mentionVerb, tapply(mentionVerb, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)

sum.na.rm <- function(x) { sum(x,na.rm=T) }
my.sd <- function(x) {sd(x)/sqrt(length(x))}

with(mentionSubject, tapply(mentionSubject, list(trialVersion), my.sd), drop=TRUE)
with(mentionObject, tapply(mentionObject, list(trialVersion), my.sd), drop=TRUE)
with(mentionVerb, tapply(mentionVerb, list(trialVersion), my.sd), drop=TRUE)

with(mentionSV, tapply(mentionSV, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mentionVO, tapply(mentionVO, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)

with(mentionSV, tapply(mentionSV, list(trialVersion), my.sd), drop=TRUE)
with(mentionVO, tapply(mentionVO, list(trialVersion), my.sd), drop=TRUE)



################################################

#######
#Run models!!
#######

#Make a variable for the inputs - number of subjects (There may be a better ordinal solution that I am not using?)
mydata$nSubj <- as.numeric(mydata$trialVersion) #This works because trialVersion is coded as 6_1 (6 sub 1 ob), 5_2 (5sub 2 ob) etc.

#Make sure that item and subject codings are factors...
mydata$paycode <- as.factor(mydata$paycode)
mydata$verb <- as.factor(mydata$verb)

#Here's a good important test!  Try taking out the extreme levels of nSubj to see if everything holds on messy cases!!!!
mydata <- mydata[mydata$nSubj < 6 & mydata$nSubj > 1,]

#######
#We can test a few different outcome measures, e.g., whether Subject was mentioned

#The nsubj manipulation is within-subject AND within-item, so the full random slopes model is:
full_maximal_model <- lmer(mentionSubject ~ nSubj + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
summary(full_maximal_model)

#Comparison model with just random slopes:
random_slope_model <- lmer(mentionSubject ~1 + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")

#Likelihood ratio test:
anova(full_maximal_model, random_slope_model)

#######
#Or ask whether object was mentioned

full_maximal_model <- lmer(mentionObject ~ nSubj + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
summary(full_maximal_model)
random_slope_model <- lmer(mentionObject ~1 + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
anova(full_maximal_model, random_slope_model)



##################################
#Also look at Subject drop main effect! (People drop subjects more often than objects)

#Just do something really simple - for each subject, give them a subject and object score, and pair-t-test them!

SubjectScore <- aggregate(mydata$mentionSubject, by = list(mydata$Paycode), sum)
names(SubjectScore) <- c("Paycode", "SubjectScore")
ObjectScore <- aggregate(mydata$mentionObject, by = list(mydata$Paycode), sum)
names(ObjectScore) <- c("Paycode", "ObjectScore")

ArgScores <- merge(SubjectScore, ObjectScore, by=c("Paycode"))

t.test(ArgScores$SubjectScore, ArgScores$ObjectScore, paired=TRUE)



########################

#######
#Double checking: ask whether SV solution was used

full_maximal_model <- lmer(mentionSV ~ nSubj + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
random_slope_model <- lmer(mentionSV ~1 + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
anova(full_maximal_model, random_slope_model)

#######
#Or whether VO solution was used 

full_maximal_model <- lmer(mentionVO ~ nSubj + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
random_slope_model <- lmer(mentionVO ~1 + (1+ nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
anova(full_maximal_model, random_slope_model)



##################################3
#Also look at verb surprisals!!

#First look at the data - for each verb, how often included? what average surprisal?

surprisal <- aggregate(mydata$surpriseRating, by = list(mydata$verb), mean.na.rm)
names(surprisal) <- c("verb", "surprisal")

marginalVerbMention <- aggregate(mydata$mentionVerb, by = list(mydata$verb), mean.na.rm)
names(marginalVerbMention) <- c("verb", "mention")

surpriseMatrix <- merge(surprisal, marginalVerbMention, by=c("verb"))

#Take out an outlier...

surpriseMatrix <- surpriseMatrix[surpriseMatrix$surprisal < 2,]

z <- lm(mention ~ surprisal, data = surpriseMatrix)
plot(surpriseMatrix$surprisal, surpriseMatrix$mention)
abline(z)

