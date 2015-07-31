## Subject Drop Rscript ##

###Merging####
#Import my subdrop data#
SubDrop <-read.csv(file.choose(), header=TRUE)
#Import Isabella's blind coding#
BlindData <-read.csv(file.choose(), header=TRUE)
#Merge both data sets (mine and blind coders)#
TotalData <- merge(SubDrop, BlindData, by=c("Subject..")) 
#Match Kid Response (mine) versus Kid Response (blind coder)
TotalData$match = (as.character(TotalData$Kid.Response.A...Prag.Choice..x) == as.character(TotalData$Kid.Response.A...Prag.Choice..y))
#Find where the data doesn't match#
nomatchdata <- TotalData[TotalData$match == FALSE,]
#Exclude those subjects from mydata#
myData[-c(1, 9, 16, 17, 19, 25, 29, 32)]

#Calculations/Statistics#

#Import excel spreadsheet#
mydata <-read.csv(file.choose(), header=TRUE)
#Desktop->Subject-Drop->MelSubDrop
mydata
#See how many rows and columns mydata has
dim(mydata)
#See summary of mydata
summary(mydata)
#Calculate mean of "Correct" column
mean(mydata$Correct)
#Calculate median of "Correct" column
median(mydata$correct)
#Calculate variance of "Correct" column
var(mydata$correct)
#Calculate standard deviation of "Correct" column
sd(mydata$correct)   
#Or calculate standard deviation as written below
sqrt(var(mydata$correct))
#Calculate minimum
min(mydata$correct)
#Calculate maximum
max(mydata$correct)
#Calculate correlation between correct answers and age
cor(mydata$Correct, mydata$Age.Years)
#Spearmen correlation
cor(mydata$Correct, mydata$Age,Years, method="spearman")
#Frequency table
#Data from only three years old
threeYOdata <- mydata[mydata$Age == 3,]
#Mean of "Correct" column of Three years old
mean(threeYOdata$Correct)
#Median of "Correct" column of Three years old
median(threeYOdata$Correct)
#Variance of "Correct" column of three year olds
var(threeYOdata$Correct)
#Standard deviation of "Correct" column of three years old
sd(threeYOdata$Correct)
#Minimum of "Correct" column of three year olds
min(threeYOdata$Correct)
#Maximum of "Correct" column of three year olds
max(threeYOdata$Correct)
#Data from only four year olds#
fourYOdata <- mydata[mydata$Age == 4,]
#Mean of "Correct" column of four years old
mean(fourYOdata$Correct)
#Median of "Correct" column of four years old
median(fourYOdata$Correct)
#Variance of "Correct" column of four year olds
var(fourYOdata$Correct)
#Standard deviation of "Correct" column of four years old
sd(fourYOdata$Correct)
#Minimum of "Correct" column of four year olds
min(fourYOdata$Correct)
#Maximum of "Correct" column of four year olds
max(fourYOdata$Correct)
#Create vector for "Condition" and "Correct" columns
mydata_cc <-mydata[, 4:5]
#Correct SDOD answers of all participants
x <- mydata[mydata$Condition == "SDOD",]$Correct
#Mean of Correct SDOD answers of all participants
mean(x)
#Correct ODSD answers of all participants
y <- mydata[mydata$Condition =="ODSD",]$Correct
#Mean of Correct ODSD answers of all participants
mean(y)
#Open data only for three year olds
threeYOdata
#Create vector for "Condition" and "Correct" columns of three year olds
threeYO_cc <-threeYOdata[, 4:5]
#Correct SDOD answers of three year olds
a <-threeYO_cc[threeYO_cc$Condition =="SDOD",]$Correct
#Mean of correct SDOD answers of three year olds
mean(a)
#Correct ODSD answers of three year olds
#Mean of correct ODSD answers of three year olds
mean(b)
#Open data only for four year olds
fourYOdata
#Create vector for "Condition" and "Correct" columns of four year olds
fourYO_cc <-fourYOdata[, 4:5]
#Correct SDOD answers of four year olds
c <-fourYO_cc[fourYO_cc$Condition =="SDOD",]$Correct
#Mean of correct SDOD answers of four year olds
mean(c)
#Correct ODSD answers of four year olds
d <-fourYO_cc[fourYO_cc$Condition =="ODSD",]$Correct
#Mean of correct SDOD answers of four year olds
mean(d)
#Calculate median of both conditions of three year olds
median(a)
median(b)
#Calculate median of both conditions of four year olds
median(c)
median(d)
#Open "Condition" and "Correct" column for three year olds
threeYO_cc
#Frequency table for Corrct answers in SDOD condition of three year olds
FreqA <- as.data.frame(table(a))
FreqA <- transform(FreqA, cumFreq = cumsum(Freq), relative = prop.table(Freq))
#Frequency table for Corrct answers in ODSD condition of three year olds
FreqB <- as.data.frame(table(b))
FreqB <- transform(FreqB, cumFreq = cumsum(Freq), relative = prop.table(Freq))
#Open "Condition" and "Correct" column for four year olds
fourYO_cc
#Frequency table for Corrct answers in SDOD condition of four year olds
FreqC <- as.data.frame(table(c))
FreqC <- transform(FreqC, cumFreq = cumsum(Freq), relative = prop.table(Freq))
#Frequency table for Corrct answers in ODSD condition of four year olds
FreqD <- as.data.frame(table(d))
FreqD <- transform(FreqD, cumFreq = cumsum(Freq), relative = prop.table(Freq))
#For Standard Error open data of three year olds of SDOD only#
a
#Calculate standard error#
SE <- sd(a)/sqrt(length(a))



