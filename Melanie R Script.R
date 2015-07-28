## Subject Drop Rscript ##
#Import excel spreadsheet#
mydata <-read.csv(file.choose(), header=TRUE)
#Desktop->Subject-Drop->MelSubDrop
mydata
#See how many rows and columns mydata has#
dim(mydata)
#See summary of mydata#
summary(mydata)
#Calculate mean of "Correct" column#
mean(mydata$correct)
#Calculate median of "Correct" column#
median(mydata$correct)
#Calculate variance of "Correct" column#
var(mydata$correct)
#Calculate standard deviation of "Correct" column#
sd(mydata$correct)   
#Or calculate standard deviation as written below#
sqrt(var(mydata$correct))
#Calculate minimum#
min(mydata$correct)
#Calculate maximum#
max(mydata$correct)
#Calculate correlation between correct answers and age#
cor(mydata$Correct, mydata$Age.Years)
#Spearmen correlation#
cor(mydata$Correct, mydata$Age,Years, method="spearman")
#Data from only three years old#
threeYOdata <-mydata[mydata$Age.Years ==3,]
#Mean of "Correct" column of Three years old#
mean(threeYOdata$Correct)
#Median of "Correct" column of Three years old#
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
fourYOdata <-mydata[mydata$Age.Years ==4,]
#Mean of "Correct" column of four years old#
mean(fourYOdata$Correct)
#Median of "Correct" column of four years old#
median(fourYOdata$Correct)
#Variance of "Correct" column of four year olds
var(fourYOdata$Correct)
#Standard deviation of "Correct" column of four years old
sd(fourYOdata$Correct)
#Minimum of "Correct" column of four year olds
min(fourYOdata$Correct)
#Maximum of "Correct" column of four year olds
max(fourYOdata$Correct)
