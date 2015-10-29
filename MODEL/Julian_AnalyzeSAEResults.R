rm (list = ls(all = TRUE)) # Clean everything
options(warn=-1) # Boot.ci() keeps waning about needing variances for t-intervals. But we don't use those.

# Load packages
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(boot)
library(magrittr)
library(nlme)

# Set working directory
setwd("~/Documents/Projects/SamplingAndEfficiency/Big World/Experiment1/Experiment1Results/")

# Part 1: Load data -----------------------------------------------------

# Load data and remove columms you don't need
models<-read.csv("../Model_Predictions/final_data_formatted.csv") %>% select(-WorldName,-NPickUp,-CanHold1AtATime) %>% tbl_df
# TrueValue column is stored as strings. Convert it to factors
models$TrueValue<-factor(models$TrueValue)

models %<>% filter(Model!="IP_new",Model!="OneStep",Model!="IP_new3")

models<-select(models,-(p_val:Cost),-MAP)
levels(models$TrueValue)<-c("N","P") # Change the factor's levels

# Z-score models
models <- plyr::ddply(models,c("Model"),function(x){
  return(data.frame(Quantity = x$Quantity,
                    Location = x$Location,
                    TrueValue = x$TrueValue,
                    WorldType = x$WorldType,
                    InferredVal = scale(x$InferredVal)[,1],
                    SD = scale(x$SD)[,1],
                    VariedCollection = x$VariedCollection))
})

# Load experiment results
human<-read.csv("SAE.csv") %>% tbl_df
# Get preference judgments for object 1 (fix scale depending on stimuli type the participant saw)
human$Preference=(1-human$Preference)*(1-human$StimType)+(human$Preference)*(human$StimType)

# Z-score each participant's answers and remove SubjectId
human<-plyr::ddply(human,c("SubjectId","ExpCondition"),function(x){
  return(data.frame(Preference=scale(x$Preference)[,1],
                Confidence=scale(x$Confidence)[,1],
                TrialName=x$TrialName))
}) %>% tbl_df

# TrialName column has the image file. Extract the stimuli condition from the string
human<-separate(human,TrialName,c("Trash","TrialName"),sep=4) %>% # split "img-" to a different column
  select(-Trash) %>% # And delete that column
  separate(TrialName,c("Quantity","Location"),sep=1) %>% # Take the first character and save into Quantity column
  separate(Location,c("Location","Rest"),sep=1) %>% # Take second character and save into location column
  separate(Rest,c("TrueValue","Trash"),sep=1) %>% select(-Trash) # Save last character and delete ".gif"
# Convert all characters into factors and add correct levels
human$Quantity<-factor(human$Quantity)
levels(human$Quantity)<-c("Even","Plentiful","Scarce")
human$Location<-factor(human$Location)
levels(human$Location)<-c("Closer","Farther","Random")
# Rename columns so we can merge them with model predictions
human<-dplyr::rename(human,HumanPref=Preference, HumanConf=Confidence, WorldType=ExpCondition)

RawData <- dplyr::full_join(human,models) %>% tbl_df

getCI<-function(inputdata){
  samples<-boot(inputdata,function(x,id){return(mean(x[id,]$HumanPref))},10000)
  ci<-boot.ci(samples,type="basic")
  return(c(ci$basic[4],ci$basic[5]))
}
set.seed(2837495)
CIs<-plyr::ddply(human,c("Quantity","Location","TrueValue","WorldType"),getCI)
CIs %<>% dplyr::rename(Lower=V1,Upper=V2)

# Now get the average human judgment
human<-human %>% dplyr::group_by(Quantity,Location,TrueValue,WorldType) %>%
  dplyr::summarise(HumanPref=mean(HumanPref),HumanConf=mean(HumanConf))

# Invert confidence judgments so 0 means extremely confident and 1 means not confident at all.
human$HumanConf=human$HumanConf*(-1)

# Create new data frame with human and model results
AllData<-dplyr::full_join(human,models,by=c("Quantity","Location","TrueValue","WorldType")) %>% tbl_df

# Part 2: Reward inference correlations ----------------------

# Get the model correlations
AllData %>% dplyr::group_by(Model) %>% dplyr::summarise(cor=cor(HumanPref,InferredVal))

# Get correlation of a subset of the data (Used for bootstrapping)
getCor<-function(x,id){return(cor(x[id,]$HumanPref,x[id,]$InferredVal))}

# Bootstrap a data frame using getCor() and return the 95% CI
GetCI<-function(dat){
  bootst=boot(dat,statistic=getCor,R=10000)
  return(boot.ci(bootst)$basic[4:5])
}

# Set seed so we can replicate exact decimal values we write on paper
set.seed(3985837)
# Run GetCI() on all models. This will take a while.
plyr::ddply(AllData,c("Model"),GetCI) %>% dplyr::rename(LowerBound=V1,UpperBound=V2)

# Get 95% CIs split by TrueValue as well

AllData %>% dplyr::group_by(Model,VariedCollection) %>% dplyr::summarise(cor=cor(HumanPref,InferredVal)) %>%
  spread(VariedCollection,cor)

# Set seed so we can replicate exact decimal values we write on paper
set.seed(3985837)
# Run GetCI() on all models. This will take a while.
AllData %>% filter(Model!="Emperical") %>%
plyr::ddply(c("Model","TrueValue"),GetCI) %>% dplyr::rename(LowerBound=V1,UpperBound=V2)

# Now do it for empirical distribution!
AllData %>% filter(Model=="Emperical",TrueValue=="N") %>%
plyr::ddply(c("Model","TrueValue"),GetCI) %>% dplyr::rename(LowerBound=V1,UpperBound=V2)

# USING TRUEVALUE TO SPLIT
# Part                 Model TrueValue LowerBound UpperBound
#1 BayesWithProportions         N  0.8196759  0.9730392
#2 BayesWithProportions         P  0.2138609  0.8132382
#3                   IP         N  0.9004398  1.0009533
#4                   IP         P  0.8120891  0.9642074
#5              IP_new2         N  0.8970516  0.9979311
#6              IP_new2         P  0.7380918  0.9651386
#7           NaiveBayes         N  0.9042623  0.9692413
#8           NaiveBayes         P -0.1080940  0.6908710
# 1 Emperical         N  0.9172222   0.997189

# Get RSS -------------------------------------
plyr::ddply(AllData,c("Model"),function(x){
  return(summary(lm(HumanPref ~ InferredVal, data=x))$sigma)
})
# Bootstrap it
getrss<-function(x,id){return(summary(lm(HumanPref ~ InferredVal, data=x[id,]))$sigma)}
GetRSSCI<-function(dat){
  bootst=boot(dat,statistic=getrss,R=10000)
  return(boot.ci(bootst)$basic[4:5])
}
set.seed(293847)
plyr::ddply(AllData,c("Model"),GetRSSCI)
#1 BayesWithProportions 0.3315533 0.4680910
#2            Emperical 0.3133025 0.4431017
#3                   IP 0.1936274 0.2782838
#4              IP_new2 0.2338836 0.3209798
#5           NaiveBayes 0.3050024 0.4533960

# Bootstrap RSS differences.
Model1<-"IP"
Model2<-"NaiveBayes"
GetRSSDif<-function(x,y){
  r1<-summary(lm(HumanPref ~ InferredVal,data=x))$sigma
  r2<-summary(lm(HumanPref ~ InferredVal,data=y))$sigma
  return(r1-r2)
}
BootRssDif<-function(x,id){
  dat=x[id,]
  M1 = filter(dat,Model==Model1)
  M2 = filter(dat,Model==Model2)
  return(GetRSSDif(M1,M2))
}
set.seed(2547283)
bootst=boot(AllData,statistic=BootRssDif,R=10000)
boot.ci(bootst)$basic[4:5]

# IP - IP_new2: -0.10267187  0.02053375
# IP - Emperical: -0.22026885 -0.06400048
# IP - BayesProp: -0.24610121 -0.08151726
# IP - Naive: -0.23233976 -0.05687414

# Get MSS --------------------------------------

TempDat<-AllData
TempDat %<>% mutate(Error=(HumanPref-InferredVal)^2)
TempDat %>% dplyr::group_by(Model) %>% dplyr::summarise(MSE = mean(Error))

#Model       MSE
#1 BayesWithProportions 0.2520968
#2            Emperical 0.2275173
#3                   IP 0.1153702
#4              IP_new2 0.1424259
#5           NaiveBayes 0.2326334

# Bootstrap
MSEBoot<-function(x,id){return(mean(x[id,]$Error))}
MSECI<-function(x){
  samples <- boot(x,MSEBoot,R=10000)
  return(boot.ci(samples,type="basic")$basic[4:5])
}
set.seed(6850430)
plyr::ddply(TempDat,c("Model"),MSECI)
#Model         V1        V2
#1 BayesWithProportions 0.18062587 0.3173825
#2            Emperical 0.14354348 0.2992873
#3                   IP 0.06945330 0.1548971
#4              IP_new2 0.09405674 0.1854722
#5           NaiveBayes 0.15091356 0.3049770

# Compare all MSEs with IP (best model)
Model1<-"IP"
Model2<-"Emperical"
getMSEDif<-function(x,id){
  dat=x[id,]
  M1 = mean(filter(dat,Model==Model1)$Error)
  M2 = mean(filter(dat,Model==Model2)$Error)
  return(M1-M2)
}
# Make it replicable
set.seed(2938472)
BootDifference <- TempDat %>% filter(Model %in% c(Model1,Model2)) %>%
  boot(statistic=getMSEDif,R=1000)
# Look at confidence intervals
boot.ci(BootDifference,type="basic")
# IP with..
# IP_new2: (-0.2015, -0.0182 )
# PropB: (-0.2147, -0.0554 )
# Naive: (-0.2079, -0.0273 )
# Emp: (-0.2019, -0.0168 ) 

#3: Correlation differences ---------------------------
# Test if a model has a reliably better correlation than a different model
# Look at the model options
table(AllData$Model)
Model1<-"IP" # Model with 0.03 cost
Model2<-"Emperical"

# Function to correlate a subset of two models
getCorDif<-function(x,id){
  dat=x[id,]
  M1=filter(dat,Model==Model1)
  cor1<-cor(M1$HumanPref,M1$InferredVal)
  M2=filter(dat,Model==Model2)
  cor2<-cor(M2$HumanPref,M2$InferredVal)
  return(cor1-cor2)
}

# Make it replicable
set.seed(2938472)
BootDifference <- AllData %>% filter(Model %in% c(Model1,Model2)) %>%
  boot(statistic=getCorDif,R=1000)
# Look at confidence intervals
boot.ci(BootDifference,type="basic")

# RESULTS
# future-discount vs proportionsampling: ( 0.0083,  0.1478 )
# future-discount vs naivesampling: (-0.0136,  0.1344 )
# future-discount vs empirical: (-0.0047,  0.1257 )
# cost vs proportionsampling: (-0.0101,  0.1338 )
# cost vs naivesampling: (-0.0308,  0.1242 )
# cost vs empirical (-0.0239,  0.1121 )

# Calculate depending on TrueValue (last two columns of table) -----------------------
MainModel <- "IP"
TestModel<- "Emperical"
GetCorDif <- function(x, id){
  dat <- x[id,]
  M1 <- filter(dat,Model==MainModel,TrueValue=="P")
  M2 <- filter(dat,Model==TestModel,TrueValue=="P")
  c1 <- cor(M1$HumanPref,M1$InferredVal)
  c2 <- cor(M2$HumanPref,M2$InferredVal)
  return(c1-c2)
}
set.seed(502983)
bootst <- boot(AllData,GetCorDif,R=10000)
boot.ci(bootst,type="basic")$basic[4:5]

# No Preference. IP with ...
# Cost: -0.04853614  0.07505884
# PropSam: -0.0456732  0.1427387
# NoPropSamp: -0.07208124  0.07723911
# Emp: -0.04854766  0.07544891

# With Preference. IP with ...
# Cost:-0.1120099  0.1752208
# Propsam: 0.0465805 0.6759345
# NoPropSamp: 0.1665825 1.0000952
# Emp: NA

# Part 3b: Figure examples ----------------

# Look at three specific trials
s1<-AllData %>% filter(Quantity=="Even",Location=="Closer",TrueValue=="P", WorldType=="Three") %>% mutate(Type="S1")
s2<-AllData %>% filter(Quantity=="Even",Location=="Random",TrueValue=="P", WorldType=="Three") %>% mutate(Type="S2")
s3<-AllData %>% filter(Quantity=="Even",Location=="Farther",TrueValue=="P", WorldType=="Three") %>% mutate(Type="S3")

rbind(s1,s2,s3) %>%
  select(-SD) %>%
  spread(Model,InferredVal) %>%
  gather(Model,InferredVal,HumanPref,BayesWithProportions:NaiveBayes) %>%
  ggplot(aes(x=Model,y=InferredVal,fill=Type))+geom_bar(stat="identity",position="dodge")+
  theme_linedraw()+
  scale_y_continuous("Inferred preference",limits=c(0,1.2))
#ggsave("Figure3a.pdf")

# Part 4: Confidence correlations ----------------------

# Get model correlations
AllData %>% dplyr::group_by(Model) %>%
  dplyr::summarise(cor=cor(HumanConf,SD))

# Make it replicable
set.seed(3985837)

# Get correlation of a subset of the data (Used for bootstrapping)
getCor<-function(x,id){return(cor(x[id,]$HumanConf,x[id,]$SD))}

# Bootstrap a data frame using getCor() and return the 95% CI
GetCI<-function(dat){
  bootst=boot(dat,statistic=getCor,R=10000)
  return(boot.ci(bootst)$basic[4:5])
}

# Get 95% CI of each correlation
plyr::ddply(filter(AllData,Model!="Emperical"),c("Model"),GetCI) %>% dplyr::rename(LowerBound=V1,UpperBound=V2)

#Model  LowerBound UpperBound
#1 BayesWithProportions  0.03987022  0.5558088
#2                   IP  0.28312091  0.7693876
#3              IP_new2 -0.08764697  0.3873314
#4           NaiveBayes -0.19778026  0.3540742


# Winner is IP. Check which ones are as good as IP.
table(AllData$Model)
Model1<-"IP" # Model with 0.03 cost
Model2<-"NaiveBayes"

# Function to correlate a subset of two models
getCorDif<-function(x,id){
  dat=x[id,]
  M1=filter(dat,Model==Model1)
  cor1<-cor(M1$HumanConf,M1$SD)
  M2=filter(dat,Model==Model2)
  cor2<-cor(M2$HumanConf,M2$SD)
  return(cor1-cor2)
}

# Make it replicable
set.seed(2938472)
BootDifference <- AllData %>% filter(Model %in% c(Model1,Model2)) %>%
  boot(statistic=getCorDif,R=1000)
# Look at confidence intervals
boot.ci(BootDifference,type="basic")
# IP with IP_new2: ( 0.0252,  0.7243 )
# BayeswProp: (-0.1294,  0.5518 ) 
# NaiveBayes: ( 0.0398,  0.8194 )


# REPEAT WITH DECOMPOSITION

AllData %>% dplyr::group_by(Model,TrueValue) %>%
  dplyr::summarise(cor=cor(HumanConf,SD)) %>% spread(TrueValue,cor)

# Make it replicable
set.seed(3985837)

# Get 95% CI of each correlation
plyr::ddply(filter(AllData,Model!="Emperical"),c("Model","TrueValue"),GetCI) %>% dplyr::rename(LowerBound=V1,UpperBound=V2)

#1 BayesWithProportions         N -0.002132156  0.5965169
#2 BayesWithProportions         P -0.116341220  0.5932181
#3                   IP         N -0.329742490  0.5058207
#4                   IP         P  0.723235727  0.9664994
#5              IP_new2         N -0.307084645  0.4482770
#6              IP_new2         P  0.606280103  0.8216575
#7           NaiveBayes         N -0.492953910  0.3096957
#8           NaiveBayes         P -0.365724830  0.4537202

# Winner is BayesWithProportions in NoPref. Is it better than the rest?
Model1<-"IP"
Model2<-"NaiveBayes"

# Function to correlate a subset of two models
getCorDif<-function(x,id){
  dat=x[id,]
  M1=filter(dat,Model==Model1)
  cor1<-cor(M1$HumanConf,M1$SD)
  M2=filter(dat,Model==Model2)
  cor2<-cor(M2$HumanConf,M2$SD)
  return(cor1-cor2)
}

# Make it replicable
set.seed(2938472)
BootDifference <- AllData %>% filter(Model %in% c(Model1,Model2), TrueValue=="P") %>%
  boot(statistic=getCorDif,R=10000)
# Look at confidence intervals
boot.ci(BootDifference,type="basic")
# NP Stimuli (Best was Bayeswprop)
# IP: (-0.2975,  0.7209 )
# IP_new2: (-0.2560,  0.7055 )
# NaiveBayes: (-0.1450,  0.8799 )
#Pref stim (Best was IP)
# IP_new2 (-0.0357,  0.2890 )
# BayesProp ( 0.2075,  0.9679 )
# NaiveBayes ( 0.3486,  1.2111 )

# Repeat excluding the mixed trials
AllData %>% filter(VariedCollection=="N",Model!="Emperical") %>% dplyr::group_by(Model) %>%
  dplyr::summarise(cor=cor(HumanConf,SD))
#Model        cor
#1 BayesWithProportions 0.28419195
#2                   IP 0.82881642
#3              IP_new2 0.72915244
#4           NaiveBayes 0.06822408
# Winner is IP in NoPref. Is it better than the rest?
Model1<-"IP"
Model2<-"NaiveBayes"

# Function to correlate a subset of two models
getCorDif<-function(x,id){
  dat=x[id,]
  M1=filter(dat,Model==Model1)
  cor1<-cor(M1$HumanConf,M1$SD)
  M2=filter(dat,Model==Model2)
  cor2<-cor(M2$HumanConf,M2$SD)
  return(cor1-cor2)
}

# Make it replicable
set.seed(2938472)
BootDifference <- AllData %>% filter(Model %in% c(Model1,Model2), VariedCollection=="N") %>%
  boot(statistic=getCorDif,R=10000)
# Look at confidence intervals
boot.ci(BootDifference,type="basic")
# IP versus..
# IP_new2: (-0.0282,  0.2229 ) 
# BayesWPropr: ( 0.2529,  0.8047 )
# NaiveBayes: ( 0.4658,  1.0704 )


# Part 4: Correlation differences in confidence judgments -----------------

# I think it only makes sense to do this when two models have very similar correlations.

# Part 5: Plots --------------------------------------

# Plot everything at once
# Reorder levels first
AllData$Model = factor(AllData$Model)
#AllData$Model = factor(AllData$Model, levels(AllData$Model)[c(3,5,1,4,2)])
AllData$Model = factor(AllData$Model, levels(AllData$Model)[c(3,4,1,5,2)])
levels(AllData$Model)<-c("Future-discount\ninverse planning","Cost\ninverse planning","Proportion-sensitive\nsampling","Proportion-insensitive\nsampling","Empirical")

AllData %>% ggplot(aes(x=InferredVal,y=HumanPref))+ # X-axis is model prediction, y axis is human judgment
  geom_point()+ # Use points for each stimuli
  facet_wrap(~Model,ncol=3)+ # Break plot into many plots using Model variable. Allow each plot to have different scales, and use 3 columns
  geom_smooth(method="lm")+ # Add a linear regression on top of each plot
  theme_linedraw()+
  scale_x_continuous("Model predictions")+scale_y_continuous("Human judgments")+ggtitle("Experiment results")

AllData %>% filter(Model!="Empirical") %>% ggplot(aes(x=SD,y=HumanConf,color=TrueValue))+ # X-axis is model prediction, y axis is human judgment
  geom_point()+ # Use points for each stimuli
  facet_wrap(~Model,ncol=2)+ # Break plot into many plots using Model variable. Allow each plot to have different scales, and use 3 columns
  geom_smooth(aes(color=NA),method="lm")+ # Add a linear regression on top of each plot
  theme_linedraw()+
  scale_x_continuous("-SD")+scale_y_continuous("Human confidence")+ggtitle("Confidence judgments")+ guides(color=FALSE)


AllData

AllData %>% ggplot(aes(x=InferredVal,y=HumanPref,color=TrueValue))+ # X-axis is model prediction, y axis is human judgment
  geom_point()+ # Use points for each stimuli
  facet_wrap(~Model,ncol=3)+ # Break plot into many plots using Model variable. Allow each plot to have different scales, and use 3 columns
  geom_smooth(method="lm")+ # Add a linear regression on top of each plot
  theme_linedraw()+
  scale_x_continuous("Model predictions")+scale_y_continuous("Human judgments")+ggtitle("Experiment results")


# Plot confidence
AllData %>% ggplot(aes(x=SD,y=HumanConf,color=TrueValue))+geom_point()+facet_wrap(~Model)

AllData %>% filter(Model=="CostIP") %>% ggplot(aes(x=SD,y=HumanConf))+geom_point()+facet_wrap(~WorldType)

AllData %>% filter(Model %in% c("IP","IP_new2")) %>% dplyr::group_by(Quantity,Location,TrueValue,WorldType) %>%
  dplyr::summarise(HumanConf=mean(HumanConf),SD=mean(SD)) %>%
  ggplot(aes(x=SD,y=HumanConf))+geom_point()

# Plot individual trials -----------------------------------

# filter the four trials you'll use.
TempDat<-tbl_df(AllData)
TempDat %<>% add_rownames %>% tbl_df
TempDat %>% filter(Quantity=="Even",TrueValue=="P",WorldType=="Three",Location=="Closer") %>% select(rowname)
TempDat %>% filter(Quantity=="Even",TrueValue=="P",WorldType=="Three",Location=="Farther") %>% select(rowname)
TempDat %>% filter(Quantity=="Plentiful",TrueValue=="P",WorldType=="Three",Location=="Random") %>% select(rowname)
TempDat %>% filter(Quantity=="Scarce",TrueValue=="P",WorldType=="Three",Location=="Random") %>% select(rowname)

indices<-c(26,27,28,29,30,56,57,58,59,60,176,177,178,179,180,266,267,268,269,270)

TempDat %>% filter(rowname %in% indices) %>% select(-SD,-rowname) %>% spread(Model,InferredVal) %>%
  add_rownames %>% tbl_df %>%
  dplyr::rename(Humans=HumanPref) %>% gather(Model,Value,Humans,9:13) %>%
  ggplot(aes(x=Model,y=Value,fill=rowname))+geom_bar(stat="identity",position="dodge")

# two!
TempDat<-tbl_df(AllData)
TempDat %<>% add_rownames %>% tbl_df
TempDat %>% filter(Quantity=="Plentiful",TrueValue=="P",WorldType=="OneThrice",Location=="Closer") %>% select(rowname)
TempDat %>% filter(Quantity=="Plentiful",TrueValue=="P",WorldType=="OneThrice",Location=="Farther") %>% select(rowname)
TempDat %>% filter(Quantity=="Plentiful",TrueValue=="P",WorldType=="OneThrice",Location=="Random") %>% select(rowname)
TempDat %>% filter(Quantity=="Scarce",TrueValue=="P",WorldType=="OneThrice",Location=="Random") %>% select(rowname)

indices<-c(111,112,113,114,115,141,142,143,144,145,171,172,173,174,175,261,262,263,264,265)

TempDat %>% filter(rowname %in% indices) %>% select(-SD,-rowname) %>% spread(Model,InferredVal) %>%
  add_rownames %>% tbl_df %>%
  dplyr::rename(Humans=HumanPref) %>% gather(Model,Value,Humans,9:13) %>%
  ggplot(aes(x=Model,y=Value,fill=rowname))+geom_bar(stat="identity",position="dodge")+theme_linedraw()

indices<-c(111,112,113,114,115,141,142,143,144,145,171,172,173,174,175,261,262,263,264,265)

PlotData <- TempDat %>% filter(rowname %in% indices) %>% select(-SD,-rowname)

test<-dplyr::left_join(PlotData,CIs)
test %>% spread(Model,InferredVal) %>%
  add_rownames %>% tbl_df %>%
  dplyr::rename(Humans=HumanPref) %>% gather(Model,Value,Humans,11:15) %>%
  ggplot(aes(x=rowname,y=Value))+geom_bar(stat="identity",position="dodge")+theme_linedraw()+facet_wrap(~Model)+
  scale_y_continuous("Preference for red\nminerals (z-scored)")+
  scale_x_discrete("Stimuli")+geom_errorbar(aes(ymin=Lower,ymax=Upper,width=0.2))
#ggsave("RevisedFig.pdf")
# Compare model sensitivity ------------------------------------------
AllData %>% dplyr::group_by(Model) %>%
  dplyr::summarise(Predictions = length(table(InferredVal)))

plyr::ddply(AllData,c("Model"),function(x){return(summary(lm(HumanPref ~ InferredVal, data=x))$r.squared)})

iota <- function(x,y){
  tied<-0
  untied<-0
  n <- length(x)
  for (i in 1:(n-1)){
    for (j in (i+1):n){
      if (i!=j){
        if ((x[i]==x[j] & y[i]!=y[j]) | (x[i]!=x[j] & y[i]==y[j])){
          tied = tied+1
        }
        else{
          untied = untied+1
        }
      }
    }
  }
  return((untied-tied)/(n*(n-1))+(1/2))
}

AllData %>% dplyr::group_by(Model) %>% dplyr::summarise(i = iota(InferredVal,HumanPref))

GetI<-function(x,id){
  t <-x[id,]
  return(iota(t$HumanPref,t$InferredVal))
}

set.seed(583723)

plyr::ddply(AllData,c("Model"),function(x){
  res <- boot.ci(boot(x,statistic=GetI,R=10000), type="basic")$basic
  return(data.frame(res[4],res[5]))})

# Part 6: Analyze participants individually ---------------------------------
 
# Reload data first

models<-read.csv("../Model_Predictions/final_data_formatted.csv") %>% tbl_df
models<-select(models,-WorldName,-NPickUp,-CanHold1AtATime)
models$TrueValue<-factor(models$TrueValue)
levels(models$TrueValue)<-c("N","P")

human<-read.csv("SAE.csv") %>% tbl_df
# Match scale to model scale and fix according to stimuli type
human$Preference=(1-human$Preference)*(1-human$StimType)+(human$Preference)*(human$StimType)

human<-plyr::ddply(human,c("SubjectId"),function(x){
  return(data.frame(Preference=scale(x$Preference)[,1],
                    Confidence=scale(x$Confidence)[,1],
                    TrialName=x$TrialName,
                    ExpCondition=x$ExpCondition))
})
human<-tbl_df(human)

human<-separate(human,TrialName,c("Trash","TrialName"),sep=4) %>% select(-Trash)
human<-separate(human,TrialName,c("Quantity","Location"),sep=1)
human<-separate(human,Location,c("Location","Rest"),sep=1)

human<-separate(human,Rest,c("TrueValue","Trash"),sep=1) %>% select(-Trash)
human$Quantity<-factor(human$Quantity)
levels(human$Quantity)<-c("Even","Plentiful","Scarce")
human$Location<-factor(human$Location)
levels(human$Location)<-c("Closer","Farther","Random")

human<-dplyr::rename(human,HumanPref=Preference,HumanConf=Confidence,WorldType=ExpCondition)

# Invert confidence judgments
human$HumanConf=human$HumanConf*(-1)

IndData<-merge(human,models,by=c("Quantity","Location","TrueValue","WorldType")) %>% tbl_df


IndData %>% dplyr::group_by(SubjectId,Model) %>%
  dplyr::summarise(Cor=cor(HumanPref,InferredVal)) %>%
  ggplot(aes(x=SubjectId,y=Cor,group=Model,color=Model))+
  geom_point()+geom_line()+ggtitle("Individual correlations")

IndData %>% dplyr::group_by(SubjectId,Model) %>%
  dplyr::summarise(Cor=cor(HumanPref,InferredVal)) %>%
  spread(Model,Cor) %>%
  mutate(Best=(IP>BayesWithProportions & IP>Emperical & IP>NaiveBayes & IP>OneStep)) %>%
  select(Best) %>%
  table

chisq.test(c(20,10),p=c(0.2,0.8), simulate.p.value=TRUE)


IndData %>% dplyr::group_by(SubjectId,Model) %>%
  dplyr::summarise(Cor=cor(HumanPref,InferredVal)) %>%
  dplyr::group_by(Model) %>%
  dplyr::summarise(MeanCor=mean(Cor))

# Plot aggregate results -------------------------------

AllData %>% filter(Model=="IP") %>% ggplot(aes(x=InferredVal,y=HumanPref))+geom_point()+facet_wrap(~Model,scales="free")+geom_smooth(method="lm")+
  scale_x_continuous("Model predictions")+scale_y_continuous("Average human\njudgment")+ggtitle("Experiment results")
#ggsave("V1.pdf")

AllData %>% filter(Model %in% c("IP","BayesWithProportions")) %>% ggplot(aes(x=InferredVal,y=HumanPref))+geom_point()+facet_wrap(~Model,scales="free")+geom_smooth(method="lm")+
  scale_x_continuous("Model predictions")+scale_y_continuous("Average human\njudgment")+ggtitle("Experiment results")
#ggsave("V2.pdf")

TempDat<-AllData
levels(TempDat$Model)=c("3 Proportion-sensitive sampling","2 Naive model",
                        "1 Inverse planning","4 Proportion-insensitive sampling",
                        "5 One-step planning")
TempDat$Model<-factor(as.character(TempDat$Model))

TempDat %>% ggplot(aes(x=InferredVal,y=HumanPref))+geom_point()+facet_wrap(~Model,ncol=5)+geom_smooth(method="lm")+
  scale_x_continuous("Model predictions")+scale_y_continuous("Average human\njudgment")+ggtitle("Experiment results")
#ggsave("NIPSFig.pdf")

AllData %>% ggplot(aes(x=InferredVal,y=HumanPref))+geom_point()+facet_wrap(~Model,ncol=3)+geom_smooth(method="lm")+
  scale_x_continuous("Model predictions")+scale_y_continuous("Average human\njudgment")+ggtitle("Experiment results")

AllData %>% filter(Model %in% c("Emperical","NaiveBayes","OneStep")) %>% ggplot(aes(x=InferredVal,y=HumanPref))+geom_point()+facet_wrap(~Model,ncol=5)+geom_smooth(method="lm")+
  scale_x_continuous("Model predictions")+scale_y_continuous("Average human\njudgment")+ggtitle("Experiment results")
#ggsave("V3.pdf")
#ggsave("OverallCorrelations.pdf")

AllData %>% dplyr::group_by(Model) %>% dplyr::summarise(cor=cor(HumanPref,InferredVal))

AllData %>% ggplot(aes(x=InferredVal,y=HumanPref))+geom_point()+facet_wrap(WorldType~Model,scales="free")+geom_smooth(method="lm")+
  scale_x_continuous("Human judgment")+scale_y_continuous("Model inference")
#ggsave("CorrelationsByCondition.pdf")

AllData %>% filter(Model!="Emperical") %>% ggplot(aes(x=SD,y=HumanConf))+geom_point()+facet_wrap(~Model,scales="free")+geom_smooth(method="lm")+
  scale_x_continuous("Posterior's\nstandard deviation")+
  scale_y_continuous("Human\nconfidence")+ggtitle("Confidence judgments")
#ggsave("Confidence2.pdf")

AllData %>% filter(Model!="Emperical") %>% ggplot(aes(x=HumanConf,y=HDI5))+geom_point()+facet_wrap(~Model,scales="free")

AllData %>% filter(Model!="Emperical") %>% ggplot(aes(x=HumanConf,y=HDI99))+geom_point()+facet_wrap(~Model,scales="free")

# Get correlations

AllData %>% dplyr::group_by(Model) %>% dplyr::summarise(HDI5=cor(HDI5,HumanConf),HDI99=cor(HDI99,HumanConf),
                                                        SD=cor(SD,HumanConf)) %>%
  gather(Method,Value,HDI5:SD) %>% ggplot(aes(x=Method,y=Value,group=Model,colour=Model))+
  geom_point()+geom_line()+ggtitle("Confidence correlations")


# Plot individual trials ----------------------

StandardData<-AllData
StandardData$HumanPref=StandardData$HumanPref+abs(min(StandardData$HumanPref))
StandardData$HumanPref=StandardData$HumanPref/max(StandardData$HumanPref)

StandardData<-select(StandardData,-HumanConf,-(SD:HDI99))
StdDat_Models<-select(StandardData,-HumanPref)
StdDat_Humans<-StandardData %>% filter(Model=="IP") %>% select(-(InferredVal:Model)) # Could be any model. Just want to remove duplicates
StdDat_Humans<-dplyr::rename(StdDat_Humans,InferredVal=HumanPref)
StdDat_Humans$Model="Human"

StandardData<-rbind(StdDat_Models,StdDat_Humans)
rm(StdDat_Models,StdDat_Humans)

StandardData$Id=paste(StandardData$Quantity,StandardData$Location,StandardData$TrueValue,StandardData$WorldType)

StandardData %>% ggplot(aes(x=Model,y=InferredVal))+geom_bar(stat="identity")+facet_wrap(~Id,scales="free")

StandardData %>% filter(WorldType=="One") %>%
  ggplot(aes(x=Model,y=InferredVal))+geom_bar(stat="identity")+facet_wrap(~Id,scales="free")

StandardData %>% filter(Model %in% c("IP","Human"),WorldType=="OneThrice") %>%
  ggplot(aes(x=Quantity,y=InferredVal,fill=Model))+
  geom_bar(stat="identity",position="dodge")+
  facet_grid(Location~TrueValue)

# * WorldType
# * Model
# Quantity
# * Location
# * Preference (True Value)
StandardData %>% filter(Model %in% c("IP"), TrueValue=="P") %>%
  ggplot(aes(x=WorldType, y=InferredVal, color=Location,group=Location))+geom_line()+
  facet_wrap(Location~Quantity)+ggtitle("Add human points with confidence intervals")


StandardData %>% filter(Model %in% c("IP","Human"), TrueValue=="P") %>%
  ggplot(aes(x=WorldType, y=InferredVal, color=Location,group=Location))+geom_line()+
  facet_wrap(Model~Quantity)+ggtitle("Qualitative model predictions")

StandardData %>% filter(Model %in% c("IP","Human"), TrueValue=="P") %>%
  ggplot(aes(x=WorldType, y=InferredVal, color=Location,shape=Model,group=Location))+
  geom_point(size=5)+geom_line()+
  facet_wrap(~Quantity)+ggtitle("Qualitative model predictions")

StandardData %>% filter(Model %in% c("IP","Human"),WorldType=="OneThrice") %>%
  ggplot(aes(x=Quantity,y=InferredVal,fill=Model))+
  geom_bar(stat="identity",position="dodge")+
  facet_grid(Location~TrueValue)

# Plot trials for Josh talk
StandardData %>% filter(Model %in% c("IP","Human"), WorldType=="Three") %>%
  filter(Quantity=="Plentiful",Location=="Closer",TrueValue=="P") %>%
  ggplot(aes(x=Quantity,y=InferredVal,fill=Model))+geom_bar(stat="identity",position="dodge")+
  scale_y_continuous("Relative preference",limits=c(0,1))
ggsave("Ex1a.pdf")

StandardData %>% filter(Model %in% c("IP","Human"), WorldType=="Three") %>%
  filter(Quantity=="Scarce",Location=="Closer",TrueValue=="P") %>%
  ggplot(aes(x=Quantity,y=InferredVal,fill=Model))+geom_bar(stat="identity",position="dodge")+
  scale_y_continuous("Relative preference",limits=c(0,1))
ggsave("Ex1b.pdf")

# Plot trials for Josh talk
StandardData %>% filter(Model %in% c("IP","Human"), WorldType=="Three") %>%
  filter(Quantity=="Even",Location=="Closer",TrueValue=="P") %>%
  ggplot(aes(x=Quantity,y=InferredVal,fill=Model))+geom_bar(stat="identity",position="dodge")+
  scale_y_continuous("Relative preference",limits=c(0,1))
ggsave("Ex2a.pdf")

StandardData %>% filter(Model %in% c("IP","Human"), WorldType=="Three") %>%
  filter(Quantity=="Even",Location=="Farther",TrueValue=="P") %>%
  ggplot(aes(x=Quantity,y=InferredVal,fill=Model))+geom_bar(stat="identity",position="dodge")+
  scale_y_continuous("Relative preference",limits=c(0,1))
ggsave("Ex2b.pdf")

# Plot specific path predictions --------------------------

AllData # Need to z-score all models
MainJudgments<-AllData %>% select(-HumanConf,-(SD:HDI99)) %>%
  spread(Model,InferredVal)

MainJudgments$BayesWithProportions=scale(MainJudgments$BayesWithProportions)[,1]
MainJudgments$Emperical=scale(MainJudgments$Emperical)[,1]
MainJudgments$IP=scale(MainJudgments$IP)[,1]
MainJudgments$NaiveBayes=scale(MainJudgments$NaiveBayes)[,1]
MainJudgments$OneStep=scale(MainJudgments$OneStep)[,1]

MainJudgments<-MainJudgments %>% gather(Model,Answer,HumanPref:OneStep)

MQuantity="Plentiful"
MLocation="Closer"
MWorldType="Three"

MainJudgments %>% filter(Quantity==MQuantity,
                         Location==MLocation,
                         WorldType==MWorldType,
                         TrueValue=="N") %>%
  ggplot(aes(x=Model,y=Answer))+geom_bar(stat="identity")
