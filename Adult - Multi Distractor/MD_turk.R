###############################################################
## Analysis of multi-distractor 'SubDrop' experiment
###############################################################

#setwd(mydir)

library(languageR)
library(lme4)
library(stringr)
library(binom)
library(bootstrap)
library(dplyr)
library(tidyr)
library(ggplot2)
source("tedlab-misc.R")
mean.na.rm <- function(x) { mean(x,na.rm=T) }

turk.files <- list.files('batch')
willow.files <- list.files('log')
#verb.files <- list.files('ratings')
		
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
## Merge the information from turk and willow!  

#How many do we start with?
length(unique(willowdata$Paycode)) #112
#length(unique(verbdata$Paycode)) #102
length(unique(turkdata$WorkerId)) #100

#Save and standardize
turkdata$Paycode <- turkdata$Answer.payCode
turkdata$Answer.payCode <- NA
turkdata <- turkdata[,setdiff(names(turkdata), c("Answer.payCode"))]
turkdata$HasTurk <- TRUE
willowdata$HasWillow <- TRUE

mydata <- NULL
mydata <- merge(willowdata, turkdata, by=c("Paycode"), all.x=TRUE, all.y=TRUE)
#mydata <- merge(mydata, verbdata, by=c("Paycode", "stimNo", "paycode", "verb"), all.x=TRUE, all.y=TRUE)

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


#(If your line doesn't have Turk, we'll give you a free pass on language, country, and video presentation)
mydata[is.na(mydata$HasTurk),]$Answer.English <- "yes"
mydata[is.na(mydata$HasTurk),]$Answer.country <- "USA"


## Recode and clean data & conditions

#Check for number of legal responses given in a single session!!
#mistakeFlag - on your first attempt, you put more than 1 word in a text box and were told to correct it. We exclude these.
mydata[is.na(mydata$mistakeFlag),]$mistakeFlag <- 'bad input'
mydata[mydata$mistakeFlag == '',]$mistakeFlag <- 'bad input' #Fix up missing values

mydata$Error <- 0
mydata[mydata$mistakeFlag == 'bad input',]$Error <- 1


participant.responsecount <- aggregate(mydata$Error, by=list(mydata$Paycode), sum)
names(participant.responsecount) <- c("Paycode", "gotError")
participant.responsecount$LegalAnswers <- 12-participant.responsecount$gotError
mydata <- merge(mydata, participant.responsecount, by=c("Paycode"), all.x=TRUE)

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



## Drop for analysis
#(Remember, Willow-onliers got a free pass on the first 3 here...)

length(unique(mydata$Paycode))

#Print out worker IDs for Snazzy Potato list

unique(mydata$WorkerId)

mydata <- mydata[mydata$tookPreviously == FALSE,]
mydata <- mydata[mydata$Answer.country == "USA" &
		mydata$Answer.English == "yes" &
		mydata$VideoProblem == "FALSE" &
		mydata$LegalAnswers > 8,] #You did at least 3/4 of the trials (i.e. you entered some two-word answer that trial on your first try)
		
#Throw out people who only had turk data (no code given...)
mydata <- mydata[!is.na(mydata$HasWillow),]

length(unique(mydata$Paycode)) #91



#################################################################
## Time to analyze the data!


#######
#Code variables
#######

#Since last time, I have re-coded some elements that were previously marked as other/vague which
#actually DO disambiguate an element in a particular context (e.g. 'woman swing' when there is just
#one female possible in the context display). I marked the kinds of these (nearly all previously
#marked as 'other', except for a (very) small number of coding mistakes discovered) to allow for
#clearer reporting.  The possible codes for each are listed in the coding doc in this folder.

table(mydata$word1_CODED_PRAGMATIC)
table(mydata$word2_CODED_PRAGMATIC)

#How many helpful answers did we initially miss?
nrow(mydata[mydata$word1_CODED != 'AGENT' & mydata$word1_CODED_PRAGMATIC == 'AGENT',])
nrow(mydata[mydata$word1_CODED != 'OBJECT' & mydata$word1_CODED_PRAGMATIC == 'OBJECT',])
nrow(mydata[mydata$word2_CODED != 'AGENT' & mydata$word2_CODED_PRAGMATIC == 'AGENT',])
nrow(mydata[mydata$word2_CODED != 'OBJECT' & mydata$word2_CODED_PRAGMATIC == 'OBJECT',])
#Thirteen! (out of 1092)


#Code whether each argument is included in the answer!

mydata$mentionSubject <- FALSE
mydata[mydata$word1_CODED_PRAGMATIC == 'AGENT',]$mentionSubject <- TRUE
mydata[mydata$word2_CODED_PRAGMATIC == 'AGENT',]$mentionSubject <- TRUE

mydata$mentionVerb <- FALSE
mydata[mydata$word1_CODED_PRAGMATIC == 'VERB',]$mentionVerb <- TRUE
mydata[mydata$word2_CODED_PRAGMATIC == 'VERB',]$mentionVerb <- TRUE

mydata$mentionObject <- FALSE
mydata[mydata$word1_CODED_PRAGMATIC == 'OBJECT',]$mentionObject <- TRUE
mydata[mydata$word2_CODED_PRAGMATIC == 'OBJECT',]$mentionObject <- TRUE

mydata$mentionOther <- FALSE
mydata[mydata$word1_CODED_PRAGMATIC != 'AGENT' & mydata$word1_CODED_PRAGMATIC != 'OBJECT' & mydata$word1_CODED_PRAGMATIC != 'VERB',]$mentionOther <- TRUE
mydata[mydata$word2_CODED_PRAGMATIC != 'AGENT' & mydata$word2_CODED_PRAGMATIC != 'OBJECT' & mydata$word2_CODED_PRAGMATIC != 'VERB',]$mentionOther <- TRUE


#Code whether the exact SV or VO solution was reached
mydata$mentionSV <- FALSE
mydata[mydata$mentionSubject == TRUE & mydata$mentionVerb == TRUE,]$mentionSV <- TRUE

mydata$mentionVO <- FALSE
mydata[mydata$mentionObject == TRUE & mydata$mentionVerb == TRUE,]$mentionVO <- TRUE


################################################

#######
#Descriptives
#######

#How often did people go off piste with response?
mean(mydata$mentionOther)


#Look at overall results
with(mydata, tapply(mentionSubject, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mydata, tapply(mentionObject, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mydata, tapply(mentionVerb, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)

#Get some bootstrapped confidence intervals for these numbers
#TERRIBLE CODE ALERT there is definitely some better way to do this. 
mentionSb_1.boot.mean = bootstrap(mydata[mydata$trialVersion=="1_6",]$mentionSubject, 1000, mean)
quantile(mentionSb_1.boot.mean$thetastar, c(0.025, 0.975))
mentionSb_2.boot.mean = bootstrap(mydata[mydata$trialVersion=="2_5",]$mentionSubject, 1000, mean)
quantile(mentionSb_2.boot.mean$thetastar, c(0.025, 0.975))
mentionSb_3.boot.mean = bootstrap(mydata[mydata$trialVersion=="3_4",]$mentionSubject, 1000, mean)
quantile(mentionSb_3.boot.mean$thetastar, c(0.025, 0.975))
mentionSb_4.boot.mean = bootstrap(mydata[mydata$trialVersion=="4_3",]$mentionSubject, 1000, mean)
quantile(mentionSb_4.boot.mean$thetastar, c(0.025, 0.975))
mentionSb_5.boot.mean = bootstrap(mydata[mydata$trialVersion=="5_2",]$mentionSubject, 1000, mean)
quantile(mentionSb_5.boot.mean$thetastar, c(0.025, 0.975))
mentionSb_6.boot.mean = bootstrap(mydata[mydata$trialVersion=="6_1",]$mentionSubject, 1000, mean)
quantile(mentionSb_6.boot.mean$thetastar, c(0.025, 0.975))

mentionOb_1.boot.mean = bootstrap(mydata[mydata$trialVersion=="1_6",]$mentionObject, 1000, mean)
quantile(mentionOb_1.boot.mean$thetastar, c(0.025, 0.975))
mentionOb_2.boot.mean = bootstrap(mydata[mydata$trialVersion=="2_5",]$mentionObject, 1000, mean)
quantile(mentionOb_2.boot.mean$thetastar, c(0.025, 0.975))
mentionOb_3.boot.mean = bootstrap(mydata[mydata$trialVersion=="3_4",]$mentionObject, 1000, mean)
quantile(mentionOb_3.boot.mean$thetastar, c(0.025, 0.975))
mentionOb_4.boot.mean = bootstrap(mydata[mydata$trialVersion=="4_3",]$mentionObject, 1000, mean)
quantile(mentionOb_4.boot.mean$thetastar, c(0.025, 0.975))
mentionOb_5.boot.mean = bootstrap(mydata[mydata$trialVersion=="5_2",]$mentionObject, 1000, mean)
quantile(mentionOb_5.boot.mean$thetastar, c(0.025, 0.975))
mentionOb_6.boot.mean = bootstrap(mydata[mydata$trialVersion=="6_1",]$mentionObject, 1000, mean)
quantile(mentionOb_6.boot.mean$thetastar, c(0.025, 0.975))

mentionVb_1.boot.mean = bootstrap(mydata[mydata$trialVersion=="1_6",]$mentionVerb, 1000, mean)
quantile(mentionVb_1.boot.mean$thetastar, c(0.025, 0.975))
mentionVb_2.boot.mean = bootstrap(mydata[mydata$trialVersion=="2_5",]$mentionVerb, 1000, mean)
quantile(mentionVb_2.boot.mean$thetastar, c(0.025, 0.975))
mentionVb_3.boot.mean = bootstrap(mydata[mydata$trialVersion=="3_4",]$mentionVerb, 1000, mean)
quantile(mentionVb_3.boot.mean$thetastar, c(0.025, 0.975))
mentionVb_4.boot.mean = bootstrap(mydata[mydata$trialVersion=="4_3",]$mentionVerb, 1000, mean)
quantile(mentionVb_4.boot.mean$thetastar, c(0.025, 0.975))
mentionVb_5.boot.mean = bootstrap(mydata[mydata$trialVersion=="5_2",]$mentionVerb, 1000, mean)
quantile(mentionVb_5.boot.mean$thetastar, c(0.025, 0.975))
mentionVb_6.boot.mean = bootstrap(mydata[mydata$trialVersion=="6_1",]$mentionVerb, 1000, mean)
quantile(mentionVb_6.boot.mean$thetastar, c(0.025, 0.975))


########
#Lump by subject to get frequency scores! Just for fun There are 2 trials per version (1_6, etc)

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

#(Check with exact SV VO solutions too)
with(mentionSV, tapply(mentionSV, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)
with(mentionVO, tapply(mentionVO, list(trialVersion), mean, na.rm=TRUE), drop=TRUE)

with(mentionSV, tapply(mentionSV, list(trialVersion), my.sd), drop=TRUE)
with(mentionVO, tapply(mentionVO, list(trialVersion), my.sd), drop=TRUE)

########
# Right here, calculate empirical percentages in a random split-half, for the modeling comparisons

set.seed(223344)
mydata$randomHalf <- runif(nrow(mydata),0,1) > 0.5
r1 <- mydata[mydata$randomHalf,]
r2 <- mydata[!(mydata$randomHalf),]

aggregate(r1$mentionSubject, by = list(r1$trialVersion), mean)
aggregate(r1$mentionObject, by = list(r1$trialVersion), mean)
aggregate(r1$mentionVerb, by = list(r1$trialVersion), mean)
aggregate(r2$mentionSubject, by = list(r2$trialVersion), mean)
aggregate(r2$mentionObject, by = list(r2$trialVersion), mean)
aggregate(r2$mentionVerb, by = list(r2$trialVersion), mean)

################################################

#######
#Run models!!
#######

#Make a variable for the inputs - number of subjects
mydata$nSubj <- as.numeric(mydata$trialVersion) #This works because trialVersion is coded as 6_1 (6 sub 1 ob), 5_2 (5sub 2 ob) etc.

#Make sure that item and subject codings are factors...
mydata$paycode <- as.factor(mydata$paycode)
mydata$verb <- as.factor(mydata$verb)

#Here's a good important test!  Try taking out the extreme levels of nSubj to see if everything below holds on non-deterministic cases!!!!
#mydata <- mydata[mydata$nSubj < 6 & mydata$nSubj > 1,]

#######
#We can test a few different outcome measures, e.g., whether Subject was mentioned

#The nsubj manipulation is within-subject AND within-item, so the full random slopes model is:
full_maximal_model <- lmer(mentionSubject ~ nSubj + (nSubj|paycode) +(nSubj|verb), data=mydata, family="binomial")
#summary(full_maximal_model)

#Comparison model with just random slopes:
random_slope_model <- lmer(mentionSubject ~1 + (nSubj|paycode) +(nSubj|verb), data=mydata, family="binomial")

#Likelihood ratio test:
anova(full_maximal_model, random_slope_model)

#######
#Or ask whether object was mentioned

full_maximal_model <- lmer(mentionObject ~ nSubj + (nSubj|paycode) +(nSubj|verb), data=mydata, family="binomial")
#summary(full_maximal_model)
random_slope_model <- lmer(mentionObject ~1 + (nSubj|paycode) +(nSubj|verb), data=mydata, family="binomial")
anova(full_maximal_model, random_slope_model)



##################################
#Also look at Subject drop main effect! (People drop subjects more often than objects)

#Overall, did people tend to mention subjects, or objects?
mean(mydata$mentionSubject)
mean(mydata$mentionObject)

#Test the distribution of answers that mentioned JUST ONE of those two
mydata$Only <- "Neither"
mydata[(mydata$word1_CODED == "AGENT" | mydata$word2_CODED == "AGENT") & !(mydata$word1_CODED == "OBJECT" | mydata$word2_CODED == "OBJECT"),]$Only <- "SubOnly"
mydata[!(mydata$word1_CODED == "AGENT" | mydata$word2_CODED == "AGENT") & (mydata$word1_CODED == "OBJECT" | mydata$word2_CODED == "OBJECT"),]$Only <- "SubOnly"

#How many had both?
mean(mydata$Only == "Neither")

diffdata <- mydata[mydata$Only != "Neither",]
diffdata$wasOb <- diffdata$Only == "ObOnly"

#Test!
binom.test(sum(diffdata$wasOb), nrow(diffdata))

###############
# GRAPHS
###############

#Reorganize the data
mylong <- mydata %>% 
  gather("whichElement","isMentioned", mentionObject, mentionSubject, mentionVerb) %>%
  group_by(trialVersion, whichElement) %>%
  summarise(mentionMean = mean(isMentioned))



errorbarsV <- list(mentionVb_1.boot.mean,mentionVb_2.boot.mean,mentionVb_3.boot.mean,mentionVb_4.boot.mean,mentionVb_5.boot.mean,mentionVb_6.boot.mean)
errorbarsS <- list(mentionSb_1.boot.mean,mentionSb_2.boot.mean,mentionSb_3.boot.mean,mentionSb_4.boot.mean,mentionSb_5.boot.mean,mentionSb_6.boot.mean)
errorbarsO <- list(mentionOb_1.boot.mean,mentionOb_2.boot.mean,mentionOb_3.boot.mean,mentionOb_4.boot.mean,mentionOb_5.boot.mean,mentionOb_6.boot.mean)
trials <- list("1_6","2_5","3_4","4_3","5_2","6_1")
trial_label <- list("1 agent, 6 patients", "2 agents, 5 patients","3 agents, 4 patients","4 agents, 3 patients","5 agents, 2 patients","6 agents, 1 patient")
mylong$error_high <- 0
mylong$error_low <- 0
mylong$condLabel <- ""
mylong$trialLabel <- ""
mylong[mylong$whichElement == "mentionSubject",]$condLabel <- "Subject"
mylong[mylong$whichElement == "mentionObject",]$condLabel <- "Object"
mylong[mylong$whichElement == "mentionVerb",]$condLabel <- "Verb"

mylong$condLabel <- factor(mylong$condLabel, levels = c("Subject", "Object","Verb"))
for (i in 1:6) {
  
  vv <- errorbarsV[[i]]
  ss <- errorbarsS[[i]]
  oo <- errorbarsO[[i]]
  mylong[(mylong$trialVersion == trials[[i]]),]$trialLabel <- trial_label[[i]] 

  mylong[(mylong$trialVersion == trials[[i]]) & (mylong$whichElement == "mentionSubject"),]$error_high <- quantile(ss$thetastar, c(0.025, 0.975))[2]
  mylong[(mylong$trialVersion == trials[[i]]) & (mylong$whichElement == "mentionSubject"),]$error_low <- quantile(ss$thetastar, c(0.025, 0.975))[1]
  mylong[(mylong$trialVersion == trials[[i]]) & (mylong$whichElement == "mentionObject"),]$error_high <- quantile(oo$thetastar, c(0.025, 0.975))[2]
  mylong[(mylong$trialVersion == trials[[i]]) & (mylong$whichElement == "mentionObject"),]$error_low <- quantile(oo$thetastar, c(0.025, 0.975))[1]
  mylong[(mylong$trialVersion == trials[[i]]) & (mylong$whichElement == "mentionVerb"),]$error_high <- quantile(vv$thetastar, c(0.025, 0.975))[2]
  mylong[(mylong$trialVersion == trials[[i]]) & (mylong$whichElement == "mentionVerb"),]$error_low <- quantile(vv$thetastar, c(0.025, 0.975))[1]

 
}

library(RColorBrewer)
my.cols <- brewer.pal(9, "Purples")
my.cols <- rev(my.cols[4:9])


ggplot(data=mylong, aes(x=condLabel, y=mentionMean, fill=trialLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=error_high, ymax=error_low), colour="black", width=.1, position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.5))+
  xlab('') +
  ylab('proportion of trials mentioning word') +
  scale_fill_manual(name="", values=my.cols) +
  theme(legend.key = element_blank()) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(text = element_text(family="Times", size=rel(4))) +
  theme(legend.text = element_text(family="Times", size=rel(4))) +
  theme(axis.text = element_text(family="Times", size=rel(0.9)))




ggsave(filename="humanPerformance.jpg", width=10, height=6)




########################



##### Extra analyses
##################################3

#######
#Double checking: ask whether SV solution was used

full_maximal_model <- lmer(mentionSV ~ nSubj + (nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
random_slope_model <- lmer(mentionSV ~1 + (nSubj|paycode) +(1+ nSubj|verb), data=mydata, family="binomial")
anova(full_maximal_model, random_slope_model)

#######
#Or whether VO solution was used 

full_maximal_model <- lmer(mentionVO ~ nSubj + (nSubj|paycode) +(nSubj|verb), data=mydata, family="binomial")
random_slope_model <- lmer(mentionVO ~1 + (nSubj|paycode) +(nSubj|verb), data=mydata, family="binomial")
anova(full_maximal_model, random_slope_model)



#We looked at verb surprisals, but they weren't very informative. (We jsut asked people how surprising the verb was given the agent & patient sets, e.g. THROW fruit vs. EAT fruit)

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

