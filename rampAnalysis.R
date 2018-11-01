###Ramp analysis
###Rewritten/organized 9.4.2018
###Updated 10.1.2018

#Things to do
#Get sd bars to work
#In the 'interesting' graphs, plot mean of participants AND each indiv. point
#Is there a way of doing some kind of analysis to see what other groups intN is a part of?
#To figure out when gambles should interrupt
#Poisson distribution 

##Loading packages
#install.packages('mosaic')
#install.packages('plotrix')
#install.packages('plotrix', dependencies=TRUE, repos='http://cran.rstudio.com/')

library(mosaic)
library(plotrix)
library(VennDiagram)
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/reg_fns.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/gamblePlotFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/rtPlotFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/odddsScoreMeanFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/odddsScoreEbFun.R")

##Loading data
#d0<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")
d0<-read.csv(file="C:/Users/Guillaume/Documents/GitHub/rampAnalysis/Totalrampv04.csv",sep=",")
#d0<-read.csv(file="C:/Users//Documents/GitHub/rampAnalysis/Totalrampv04.csv",sep=",")

#d0<-read.csv(file="//files.brown.edu/Home/gpagnier/Documents/GitHub/rampAnalysis/Totalrampv03.csv",sep=",")

#d0<-read.csv(file.choose())
#Cleaning data for totalrampv3
d0<-d0[5836:length(d0$viewTime),]
d0<-subset(d0,!grepl("debug",as.character(d0$uniqueid)))

bonusAmountsTemp=data.frame(matrix(NA, ncol = 2, nrow =1))
bonusAmounts=bonusAmountsTemp
for (i in 1:length(d0$BonusAmount)){
  if(!is.na(d0[i,2])){
    bonusAmountsTemp[1,1]=as.integer(d0[i,"BonusAmount"])
    bonusAmountsTemp[1,2]=as.character(d0[i,"uniqueid"])
    bonusAmounts=rbind(bonusAmounts,bonusAmountsTemp)
  }
}
bonusAmounts=bonusAmounts[-1,]
colnames(bonusAmounts)[1]<-"Amount"
colnames(bonusAmounts)[2]<-"ID"

bonusAmounts<-unique(bonusAmounts)
bonusAmounts
#If you want to see survey results
dsurvey<-d0 %>% 
  group_by(uniqueid) %>% 
  summarise(engagement=unique(engagement)[2],
            difficulty=unique(difficulty)[2],
            length=unique(length)[2],
            design=unique(design)[2],
            gender=unique(gender)[2],
            interest=unique(interest)[2])
#Removing rows that have empty column values (specifically if gender is empty)
dsurvey<-dsurvey[!(is.na(dsurvey$gender)|dsurvey$gender==""),]

#Engagment: how engaging, 1 is not, 10 is very engaging
hist(as.integer(dsurvey$engagement),breaks=50,main="Length: 10 is very engaging")
#difficult: 1 is easy, 10 is hard
hist(as.integer(dsurvey$difficulty),breaks=50,main="Length: 10 is too hard")
#length: 1 is could be longer, 10 is much too long
hist(as.integer(dsurvey$length),breaks=50,ylim=c(0,50),main="Length: 10 is too long")
#design: how engaging, 1 is unplayable, 4 is worked fine
table(as.integer(dsurvey$design))
#Gender: 1 male, 2 is female 4: decline to answer
table(as.integer(dsurvey$gender))
#interest: 1 is no way; 10 is absolutely yes
hist(as.integer(dsurvey$interest),breaks=50,ylim=c(0,100),main="interest; 10 is very interesting")


#Warning! CSV needs to be in exact column order:
#"trialid" #"expTime" "gambleDelay" "gambleRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"

#Need to replace uniqueid with numbers and clean out columns that aren't useful
d<-d0[,c("Trialid","gambleDelay","gambleRT","outcomeRT","response","standardGamble","trialNumber","uniqueid")]
d<-subset(d,!grepl("debug",as.character(d$uniqueid)))
d<-subset(d,d$response!="")
d$engagement<-NULL
d$difficulty<-NULL
d$length<-NULL
d$design<-NULL
d$gender<-NULL
d$interest<-NULL
d$expTime

#Adding survey answers
d2=NULL
for(i in unique(d$uniqueid)){
  d0sub<-filter(d0,uniqueid==i)
  dsub<-filter(d,uniqueid==i)
  dsub$engagement=unique(d0sub$engagement)[2]
  dsub$difficulty=unique(d0sub$difficulty)[2]
  dsub$length=unique(d0sub$length)[2]
  dsub$design=unique(d0sub$design)[2]
  dsub$gender=unique(d0sub$gender)[2]
  dsub$interest=unique(d0sub$interest)[2]
  dsub$expTime=unique(d0sub$expTime)[2]
  d2<-rbind(d2,dsub)
}
d<-d2
#Adding col uniqueID uniqueid with numbers
d$uniqueID=NA
seed=401
d[1,"uniqueID"]<-seed
for (i in 2:nrow(d)){
  if(d[i,"uniqueid"]==d[i-1,"uniqueid"]){
    d[i,"uniqueID"]=d[i-1,"uniqueID"]
  }
  else if (d[i,"uniqueid"]!=d[i-1,"uniqueid"]){
    d[i,"uniqueID"]=(d[i-1,"uniqueID"]+1)
  }
  
}
unique(d$uniqueID)
d$uniqueid=d$uniqueID
d$uniqueID=NULL
#############################################################################################################################
#Now we have a dataframe we can work with
#Replace any empty cell with NA
#Colnames should now be:
#"trialid" #"expTime" "gambleDelay" "gambleRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"

d[d==""] <- NA
d$gambleDelay<-d$gambleDelay/1000
d$binsTime=0;

#Clearing pictures
graphics.off()

##Some basic behavioral metrics and filtering participants and adding gamble delay
#Intitial filtering of participants
removeIds=c(498)
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}
unique(d$uniqueid)

#Where did gambles interrupt
hist(d$gambleDelay,breaks=50,xlim=c(0,8),main="When did gambles interrupt the progress bar?",xlab="Seconds into trial gamble appeared",col='black',ylab="Total number of trials",ylim=c(0,2500))


#How many points do you want on the gambleDelay?
d$binsTme=NULL
bins=3

ninbins=round((length(d$Trialid)/bins))

#This creates a list with what should be in the binTimeCalc
delayValues=split(sort(d$gambleDelay[d$gambleDelay!=0]), ceiling(seq_along(d$gambleDelay[d$gambleDelay!=0])/ninbins))

#This is to manually hardcode the NUMBER of blocks you're using - can not exceed number of bins
a1head<-unlist(lapply(delayValues[1],head,1),use.names=FALSE)
a1tail<-unlist(lapply(delayValues[1],tail,1),use.names=FALSE)
a2head<-unlist(lapply(delayValues[2],head,1),use.names=FALSE)
a2tail<-unlist(lapply(delayValues[2],tail,1),use.names=FALSE)
a3head<-unlist(lapply(delayValues[3],head,1),use.names=FALSE)
a3tail<-unlist(lapply(delayValues[3],tail,1),use.names=FALSE)
a4head<-unlist(lapply(delayValues[4],head,1),use.names=FALSE)
a4tail<-unlist(lapply(delayValues[4],tail,1),use.names=FALSE)
a5head<-unlist(lapply(delayValues[5],head,1),use.names=FALSE)
a5tail<-unlist(lapply(delayValues[5],tail,1),use.names=FALSE)
a6head<-unlist(lapply(delayValues[6],head,1),use.names=FALSE)
a6tail<-unlist(lapply(delayValues[6],tail,1),use.names=FALSE)
a7head<-unlist(lapply(delayValues[7],head,1),use.names=FALSE)
a7tail<-unlist(lapply(delayValues[7],tail,1),use.names=FALSE)
a8head<-unlist(lapply(delayValues[8],head,1),use.names=FALSE)
a8tail<-unlist(lapply(delayValues[8],tail,1),use.names=FALSE)
a9head<-unlist(lapply(delayValues[9],head,1),use.names=FALSE)
a9tail<-unlist(lapply(delayValues[9],tail,1),use.names=FALSE)
a10head<-unlist(lapply(delayValues[10],head,1),use.names=FALSE)
a10tail<-unlist(lapply(delayValues[10],tail,1),use.names=FALSE)

#Manually setting bins
#For totalRampv3
#For 6 bins
a1head<-1.4
a1tail<-1.999999
a2head<-2
a2tail<-2.5
a3head<-3.75
a3tail<-3.999999
a4head<-4
a4tail<-4.5
a5head<-5.8
a5tail<-6.24
a6head<-6.24000000001
a6tail<-6.8

#For 3 bins
a1head<-1.5
a1tail<-2.5
a2head<-3.5
a2tail<-4.5
a3head<-5.75
a3tail<-6.75

#This is the function that creates gambleBins
binTimeCalc<-function(d,row){
  if(d[row,'gambleDelay']==0)
  {return(0)}
  else if (d[row,'gambleDelay']>0&d[row,'gambleDelay']<=a1tail)
  {return(mean(c(a1head,a1tail)))}
  else if (d[row,'gambleDelay']>=a2head&d[row,'gambleDelay']<=a2tail)
  {return(mean(c(a2head,a2tail)))}
  else if (d[row,'gambleDelay']>=a3head&d[row,'gambleDelay']<=a3tail)
  {return(mean(c(a3head,a3tail)))}
  # else if (d[row,'gambleDelay']>=a4head&d[row,'gambleDelay']<=a4tail)
  # {return(mean(c(a4head,a4tail)))}
  # else if (d[row,'gambleDelay']>=a5head&d[row,'gambleDelay']<=a5tail)
  # {return(mean(c(a5head,a5tail)))}
  # else if (d[row,'gambleDelay']>=a6head&d[row,'gambleDelay']<=a6tail)
  # {return(mean(c(a6head,a6tail)))}
  # else if (d[row,'gambleDelay']>=a7head&d[row,'gambleDelay']<=a7tail)
  # {return(mean(c(a7head,a7tail)))}
  # else if (d[row,'gambleDelay']>=a8head&d[row,'gambleDelay']<=a8tail)
  # {return(mean(c(a8head,a8tail)))}
  # else if (d[row,'gambleDelay']>=a9head&d[row,'gambleDelay']<=max(d$gambleDelay))
  # {return(mean(c(a9head,a9tail)))}
  else
  {return(999)}
}

#Actually running the functon
for(i in 1:nrow(d)){
  d[i,'binsTime']=binTimeCalc(d,i)
}

#Creating new df to see how many ended up in each bin/ this is sanity check
dbins<-d %>% 
  group_by(binsTime) %>% 
  summarise(Number=length(response))
dbins

#Remove/coalesce any rows with bins if the numbers are too far apart from one another
# #Right now this takes the last one and adds it to the last 'bin'
# for(i in 1:nrow(d)){
#   if(d[i,"binsTime"]==mean(c(a8head,a8tail))){
#     d[i,"binsTime"]=mean(c(a7head,a7tail))
#     i
#   }
# };
dbins<-d %>% 
  group_by(binsTime) %>% 
  summarise(Number=length(response))
dbins

d<-filter(d,binsTime<999)
###################################################################################
#Adding different factors

#Adding which condition trial was in
#For new data set
d$magCond<-NULL
magCondCalc<-function(d,row){
  if(d[row,'standardGamble']==1|d[row,'standardGamble']==2){
    return("low")
  } else if (d[row,'standardGamble']==3|d[row,'standardGamble']==4){
    return("mid")
  } else if (d[row,'standardGamble']==5|d[row,'standardGamble']==6){
    return("high")
  } else{
    return("null")
  }
}

for(i in 1:nrow(d)){
  d[i,'magCond']=magCondCalc(d,i)
}

d$oddsCond<-NULL
oddsCondCalc<-function(d,row){
  if(d[row,'Trialid']==21|d[row,'Trialid']==22|d[row,'Trialid']==23|d[row,'Trialid']==24|d[row,'Trialid']==25|d[row,'Trialid']==26){
    return("lowp")
  } else if (d[row,'Trialid']==1|d[row,'Trialid']==2|d[row,'Trialid']==3|d[row,'Trialid']==4|d[row,'Trialid']==5|d[row,'Trialid']==6){
    return("midp")
  } else if (d[row,'Trialid']==31|d[row,'Trialid']==32|d[row,'Trialid']==33|d[row,'Trialid']==34|d[row,'Trialid']==35|d[row,'Trialid']==36){
    return("highp")
  } else{
    return("catch")
  }
}

for(i in 1:nrow(d)){
  d[i,'oddsCond']=oddsCondCalc(d,i)
}

#Adding another column 1 if they gambled, 0 if they didn't
#This will be outcome variable in logistic regression
d$gambled=NULL
for (i in 1:length(d$response)){
  if(d$response[i]=='gamble'){
    d$gambled[i]=1
  } else {
    d$gambled[i]=0
  }
}

#This is a column for highest gamble shown (used to calculate RPE2 and RPE3)
d$gambleMaxAmt=NULL
for (i in 1:length(d$response)){
  if(d$oddsCond[i]=='lowp'){
    d$gambleMaxAmt[i]=(d$standardGamble[i]*1.5)
  } else if(d$oddsCond[i]=='midp'){
    d$gambleMaxAmt[i]=(d$standardGamble[i]*2)
  }else if(d$oddsCond[i]=='highp'){
    d$gambleMaxAmt[i]=(d$standardGamble[i]*3)
  } else {
    d$gambleMaxAmt[i]=d$standardGamble[i]
  }
}

#Adding prediction errors:
#Adding prediction errors as possible variable
d2=d[0,]
#rpe 1 2 and 3
d$rpe1=NULL
d$rpe2=NULL
d$rpe3=NULL
d$gamblePrevTrial=NULL
#RT z score
#Should I do z scores on speed or raw RT?
d$gambleRTz<-NULL
d$outcomeRTz<-NULL
acfdf<-matrix(NA, ncol = 133, nrow =0)
Participants<-unique(d$uniqueid)
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  if(length(dsub$gambled)==133){
    acfdf<-rbind(acfdf,dsub$gambled)
  }
}

#Adding vector to filter out fast RTers
fastRTers<-NULL
#Adding RPE as a factor AND normalized RT z score
#rpe1 is the sure thing of trial t- standard gamble of t-1
#rpe2 is the sure thing of t - the average of whatever was chosen in t-1
#rpe3 is the sure thing of t - whatever was chosen in t-1 but the highest gamble option instead of average
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  dsub[1,"rpe1"]=dsub[1,"standardGamble"]
  dsub[1,"rpe2"]=dsub[1,"standardGamble"]
  dsub[1,"rpe3"]=dsub[1,"standardGamble"]
  #This is adding RPE
  for (row in 2:length(dsub$Trialid)){
    #This is essentially calculating the difference between potential reward on trial
    #t - reward on trial t-1
    dsub[row,"rpe1"]=(dsub[row,"standardGamble"]-dsub[(row-1),"standardGamble"])
    #This is rpe2
    #And gamblingPrevTrial
    if(dsub[(row-1),"gambled"]){
      dsub[row,"rpe2"]=(dsub[row,"standardGamble"]-(dsub[(row-1),"gambleMaxAmt"]/2))
      dsub[row,"gamblePrevTrial"]=1
    } else{
      dsub[row,"rpe2"]=(dsub[row,"standardGamble"]-dsub[(row-1),"standardGamble"])
      dsub[row,"gamblePrevTrial"]=0
    }
  }
  #This is rpe3
  for (row in 2:length(dsub$Trialid)){
    if(dsub[(row-1),"gambled"]){
      dsub[row,"rpe3"]=(dsub[row,"standardGamble"]-(dsub[(row-1),"gambleMaxAmt"]))
    } else{
      dsub[row,"rpe3"]=(dsub[row,"standardGamble"]-dsub[(row-1),"standardGamble"])
    }
  }
  #This is calculating normalized z score for dsub
  meanGambleZ=mean(dsub$gambleRT[dsub$gambleRT!=0])
  meanOutcomeZ=mean(dsub$outcomeRT[dsub$outcomeRT!=0])
  sdGambleZ=sd(dsub$gambleRT[dsub$gambleRT!=0])
  sdOutcomeZ=sd(dsub$outcomeRT[dsub$outcomeRT!=0])
  
  for(row in 1:length(dsub$Trialid)){
    if(dsub[row,"gambleRT"]!=0){
      dsub[row,"gambleRTz"]=(dsub[row,"gambleRT"]-meanGambleZ)/sdGambleZ
    } else if(dsub[row,"gambleRT"]==0){
      dsub[row,"gambleRTz"]=0
    }
    if(dsub[row,"outcomeRT"]!=0){
      dsub[row,"outcomeRTz"]=(dsub[row,"outcomeRT"]-meanOutcomeZ)/sdOutcomeZ
    } else if(dsub[row,"outcomeRT"]==0){
      dsub[row,"outcomeRTz"]=0
    }
  }
  
  #Fitering out fast RTers
  
  if(nrow(filter(dsub,outcomeRT>0&outcomeRT<150))>20){
    fastRTers<-c(fastRTers,unique(dsub$uniqueid))
  }
  
  d2=rbind(d2,dsub)
}
d=d2

#Normalized RT log(1/RT) to get speed
d$NgambleRT=0
d$NoutcomeRT=0


#Dividing gambleRT by (1/RT) on new column
for(i in 1:length(d$response)){
  if(d[i,"gambleRT"]!=0){
    d[i,"NgambleRT"]=(1/d[i,"gambleRT"])
  }
}
#Dividing OutcomeRT by 1/RT
for(i in 1:length(d$response)){
  if(d[i,"outcomeRT"]!=0){
    d[i,"NoutcomeRT"]=(1/d[i,"outcomeRT"])
  }
}

#########################################################################################################################################
#This should be ready to run now without any artificial filtering of participants for low RT etc.

nParticipants<- length(unique(d$uniqueid))
nParticipants

# #Check for catch trials
# #75 should gamble; 86 should success/fail; 2 catch trials - moved to 6
# #Check for catch trials
# dcatch<-filter(d,Trialid==75|Trialid==86)[,c("Trialid","response","uniqueid")]
# #dcatch[order(dcatch$Trialid),]
# dcatchGamble<-dcatch[dcatch$Trialid==75,]
# failCatchId<-dcatchGamble[dcatchGamble$response=='success'|dcatchGamble$response=='fail',]$uniqueid
# 
# dcatchSuccess<-dcatch[dcatch$Trialid==86,]
# 
# failCatchId<-unique(c(failCatchId,dcatchSuccess[dcatchSuccess$response=='gamble',]$uniqueid))
# catchSuccessId<-setdiff(Participants,failCatchId)
# 
# #Differential analysis of failCatch
# #Called what they SHOULD do
# dcatchidFinish<-dcatch[dcatch$Trialid==86,] %>% 
#   group_by(uniqueid) %>% 
#   summarise(FailTrials=sum(response=="gamble"|response=='earlyFail'))
# 
# dcatchidGamble<-dcatch[dcatch$Trialid==75,] %>% 
#   group_by(uniqueid) %>% 
#   summarise(FailTrials=sum(response=="fail"|response=='success'|response=='earlyFail'))
# 
# dcatchscore<-data.frame(dcatchidFinish$uniqueid)
# colnames(dcatchscore)[1]='uniqueid'
# dcatchscore$failScore<-dcatchidFinish$FailTrials+dcatchidGamble$FailTrials

###Behavioral analyses
##Reaction time
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
hist(gambleRTs,main="Aggregated gamble RTs",breaks=70)
#Whenever they claimed 'boring' reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
hist(outcomeRTs,main=c("Aggregated outcome RTs; number of trials:",length(outcomeRTs)),breaks=70)

#Printing experiment times in minutes CONFIRM PSITURK TRACKS IN MILLISECONDS
expTimes<-((unique(d$expTime,na.rm=TRUE)/1000)/60)
hist(expTimes,main="Experiment Time (in minutes)",breaks=50,xlim=c(0,120))

#Survey responses
dsurvey<-d %>% 
  group_by(uniqueid) %>% 
  summarise(engagement=unique(engagement),
            difficulty=unique(difficulty),
            length=unique(length),
            design=unique(design),
            gender=unique(gender),
            interest=unique(interest))
dsurvey<-dsurvey[!(is.na(dsurvey$gender)|dsurvey$gender==""),]
#Removing participants who gambled too much/not enough
dgamble0<-filter(d,gambleDelay!=0)

dBehavioralTotal<-dgamble0 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

#head(dBehavioralTotal)
#Overall preference for gambling
hist(dBehavioralTotal$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity(everyone) to gamble; n =",toString(sum(dBehavioralTotal$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

dlowG<-filter(dBehavioralTotal,percentageGambled<6)
noGamblers<-dlowG$uniqueid
dhighg<-filter(dBehavioralTotal,percentageGambled>95)
allGamblers<-dhighg$uniqueid
lowTrials<-filter(dBehavioralTotal,trials<50)$uniqueid
removeIds<-c(noGamblers,allGamblers,lowTrials)

#Removing fastRTers
removeIds<-c(removeIds,fastRTers)
#Removing any subjects from dataset, using unique ids in vector removeIds
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}

#Check for catch trials
#75 should gamble; 86 should success/fail; 2 catch trials - moved to 6
#Check for catch trials
dcatch<-filter(d,Trialid==75|Trialid==86)[,c("Trialid","response","uniqueid")]

#Differential analysis of failCatch
#Called what they SHOULD do
dcatchidFinish<-dcatch[dcatch$Trialid==86,] %>% 
  group_by(uniqueid) %>% 
  summarise(FailTrials=sum(response=="gamble"|response=='earlyFail'))
imp<-filter(dcatchidFinish,FailTrials>0)$uniqueid

dcatchidGamble<-dcatch[dcatch$Trialid==75,] %>% 
  group_by(uniqueid) %>% 
  summarise(FailTrials=sum(response=="fail"|response=='success'|response=='earlyFail'))
l<-filter(dcatchidGamble,FailTrials>0)$uniqueid

dcatchscore<-data.frame(dcatchidFinish$uniqueid)
colnames(dcatchscore)[1]='uniqueid'
dcatchscore$failScore<-dcatchidFinish$FailTrials+dcatchidGamble$FailTrials

failCatchId<-unique(dcatchscore$uniqueid[dcatchscore$failScore>0])
catchSuccessId<-unique(dcatchscore$uniqueid[dcatchscore$failScore==0])

#Breaking down catchTrials by severity
fail1<-unique(dcatchscore$uniqueid[dcatchscore$failScore==1])
fail2<-unique(dcatchscore$uniqueid[dcatchscore$failScore==2])
fail3<-unique(dcatchscore$uniqueid[dcatchscore$failScore==3])
fail4<-unique(dcatchscore$uniqueid[dcatchscore$failScore==4])
fail5<-unique(dcatchscore$uniqueid[dcatchscore$failScore==5])
fail6<-unique(dcatchscore$uniqueid[dcatchscore$failScore==6])

#Now this is refined number of participants
nParticipants<- length(unique(d$uniqueid))
Participants<-unique(d$uniqueid)
nParticipants

#######################################################################################################
#Clearing pictures
graphics.off()

#Behavioral analyses
dgamble<-filter(d,gambleDelay!=0)

dBehavioral<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

hist(dBehavioral$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall propensity to gamble; n =",toString(nrow(dgamble[dgamble$gambleDelay!=0,]))," possible trials;",nParticipants,"subj"),xlab="Percentage of time gambled",col='red')
boxplot(dBehavioral$percentageGambled,ylim=c(0,100),main=paste("Distribution of gambling percentages across everyone; mean G % = ",toString(mean(dBehavioral$percentageGambled))),ylab="Percentage gambled (per participant)")

#Remaking behavioral histograms
#Whenever they gambled
gambleRTs<-dgamble$gambleRT[dgamble$gambleRT!=0]
hist(gambleRTs,main=paste("Aggregated gamble RTs; ",toString(sum(dBehavioral$gambleCount)),"trials gambled"),breaks=70)
c("Numberof trials that they gambled on: ",toString(sum(dBehavioral$gambleCount)))

#Whenever they claimed guaranteed reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
hist(outcomeRTs,main=paste("Aggregated outcome RTs; ",toString(length(outcomeRTs)),"trials accepted sure thing"),breaks=70)
c("Number of trials that they accepted sur thing: ",length(outcomeRTs))



#Number of trials per participant 
dTrials<-d %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/ntrials*100))
#dTrials
hist(dTrials$ntrials,breaks=50,xlim=c(0,140),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")

#Statistics
#Logistic regression models to predict gambled
#Need to figure out which one to use
mlog<-glm(gambled~contOdds,
          data=dgamblelate,family="binomial");
summary(mlog)

mlog2<-glm(gambled~gambleDelay+oddsCond+interest,
           data=dgamble,family="binomial");
summary(mlog2)

d5gambleearly$contOdds<-recode(d5gambleearly$oddsCond,lowp=1,midp=2,highp=3)
d5gamblemid$contOdds<-recode(d5gamblemid$oddsCond,lowp=1,midp=2,highp=3)
d5gamblelate$contOdds<-recode(d5gamblelate$oddsCond,lowp=1,midp=2,highp=3)

dgambleearly$contOdds<-recode(dgambleearly$oddsCond,lowp=1,midp=2,highp=3)
dgamblemid$contOdds<-recode(dgamblemid$oddsCond,lowp=1,midp=2,highp=3)
dgamblelate$contOdds<-recode(dgamblelate$oddsCond,lowp=1,midp=2,highp=3)


d5gambleearly<-filter(d5gamble,gambleDelay<2.5)
d5gamblemid<-filter(d5gamble,gambleDelay>2.5&gambleDelay<5)
d5gamblelate<-filter(d5gamble,gambleDelay>5)

dgambleearly<-filter(dgamble,gambleDelay<2.5)
dgamblemid<-filter(dgamble,gambleDelay>2.5&gambleDelay<5)
dgamblelate<-filter(dgamble,gambleDelay>5)

d5success<-filter(d5,Trialid==75)
gamblePlot(d5success,orig=T,title="shouldGamble")
rtPlot(d5success,type='raw',eb='stderr',ylimit=c(400,1000),title="ShouldGamble")

library(lme4)
 # dgamble[,'oddsCondf'] <- as.factor(dgamble[,'oddsCond'])
 #  mlmerog<-glmer(gambled~scale(gambleDelay)+oddsCondf+(scale(gambleDelay)+oddsCondf|uniqueid),
 #           data=dgamble,family="binomial");
 #  summary(mlmerog)

# d5primegamble[,'oddsCondf'] <- as.factor(d5primegamble[,'oddsCond'])
# d5primegamble[,'gamblePrevTrialf'] <- as.factor(d5primegamble[,'gamblePrevTrial'])
# mlogfc<-glm(gambled~gambleDelay+oddsCondf+gamblePrevTrial,
#               data=d5primegamble,family="binomial");
# summary(mlogfc)
  
##Total data
c("Number of trials that they gambled on: ",length(dgamble$response[dgamble$response=='gamble']))
c("Number of trials that they had the chance to gamble on: ",length(dgamble$response))

#By GambleDelay
d2<-filter(d,gambleDelay!=0) %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))

d2$seconds<-d2$binsTime
d2=filter(d2,binsTime!=0)
d2

d2p<-filter(d,gambleDelay!=0) %>% 
  group_by(binsTime,uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            semRT=sd(setdiff(gambleRT,0))/sqrt(length(setdiff(gambleRT,0))))

d2p$seconds<-d2p$binsTime
d2p=filter(d2p,binsTime!=0)
head(d2p)

d2prime<-d2p %>% 
  group_by(uniqueid) %>% 
  summarise(subjAverage=mean(percentageGambled))

grandmean=mean(d2p$percentageGambled)
head(d2p)


#Interesting plot of gambleDelay vs propensity to gamble. NEED TO ADD SDS HERE
#Andrew1
plot(d2$seconds,d2$percentageGambled,xlim = c(0,8),ylim = c(35,50),
     main=paste("Total group data; Gamble propensity; n =",toString(sum(d2$trials)),
                "trials;",toString(length(Participants)),"participants"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
abline(lm(d2$percentageGambled~d2$seconds))

#Plotting gamble RTs with sd
dRT<-filter(dgamble,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sdRT=std.error(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRTz=median(gambleRTz),
            sdRTz=sd(gambleRTz))
dRT$seconds<-dRT$binsTime
summary(m2RT<-glm(dRT$medianRT~dRT$binsTime))
#This is raw RT
plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=c(650,800),main=paste("Group data; total data; median RT with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dRT$seconds)){
  arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]+(as.numeric(dRT[i,3]))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]-(as.numeric(dRT[i,3]))),length=0.05, angle=90, code=3)
}
rtPlot(d,type="raw",eb='stderr',ylimit=c(600,900))

#This is Speed
plot(dRT$seconds,dRT$medianSpeed,xlim = c(0,8),main=paste("Group data; total data; median Speed with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dRT$seconds)){
  arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianSpeed']+(as.numeric(dRT[i,'sdSpeed']))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianSpeed']-(as.numeric(dRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
}
rtPlot(d,type="raw",eb='stderr',ylimit=c(600,900))

#Formatting to get subj mean and grand mean and making new df d2poster that does the calculation (very elegant)
d2p$binsTime<-as.factor(d2p$binsTime)
d2poster<-d2p
for(i in 1:nrow(d2p)){
  d2poster[i,"percentageGambled"]$percentageGambled=(d2poster[i,"percentageGambled"]$percentageGambled-d2prime[which(d2prime$uniqueid==d2p[i,"uniqueid"]$uniqueid),"subjAverage"]$subjAverage+grandmean)
}
#Making poster graphs of total gamble delay and RT with confidence bands
#POSTER1
 #violin plot
  # p1<-ggplot(d2poster,aes(binsTime,percentageGambled))+geom_violin(data=d2poster)
  # p1
 #Scatterplot with error bars

  # 
  # pGD<-ggplot(d2p,aes(seconds,percentageGambled))+stat_summary(geom="point",fun.y=mean)+
  #   stat_summary(geom="errorbar",fun.data=mean_se)+theme_bw()+
  #   xlim(1,7.5)+xlab("Seconds into trial")+
  #   ggtitle("Total group data; Gamble Propensity; n = 13654 trials; 121 participants")+ylab("Mean Gamble propensity")+
  #   theme(plot.title = element_text(hjust = 0.5,face="bold",size=14))+
  #   geom_smooth(method='lm',se=FALSE,color='black',size=.4)+
  #   theme(axis.title.y = element_text(margin=unit(c(0,4,0,0),"mm")),panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank())
  # pGD  
  # 
plot(d2$seconds,d2$percentageGambled,xlim = c(0,7.5),ylim = c(35,50),
     main=paste("Propensity to gamble vs. gamble interruption time"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19,bty='l')
abline(lm(d2$percentageGambled~d2$seconds))
summary(lm(d2$percentageGambled~d2$seconds))
#Making poster graph of RT of d2p with semRTs
#POSTER2
#Andrew2
pRT<-ggplot(d2p,aes(seconds,medianRT))+stat_summary(geom="point",fun.y=mean)+
              stat_summary(geom="errorbar",fun.data=mean_se)+theme_bw()+
              xlim(1,7.5)+ylim(770,790)+xlab("Seconds into trial")+
              ggtitle("Total group data; Reaction Time; n = 13654 trials; 121 participants")+ylab("Mean RT")+
              theme(plot.title = element_text(hjust = 0.5,face="bold",size=14))+
              geom_smooth(method='lm',se=FALSE,color='black',size=.4)+
              theme(axis.title.y = element_text(margin=unit(c(0,4,0,0),"mm")),panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
pRT


#Another way of getting interesting gamble graph once you have d2p (summarize by uniqueid and bins Time)
dTest<-d2p %>%
  group_by(seconds) %>%
  summarise(meanPercentageGambled=mean(percentageGambled),
            medianPercentageGambled=median(percentageGambled),
            sdPercentageGambled=sd(percentageGambled),
            stdPercentageGambled=std.error(percentageGambled))

plot(dTest$seconds,dTest$medianPercentageGambled,xlim = c(0,7.5),ylim = c(35,50),
     main=paste("Propensity to gamble vs. gamble interruption time"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19,bty='l')
gamblePlot(d5,orig=T,eb='sem',ylimit=c(0,100))

gamblePlot(d,orig=T,eb='sem',ylimit=c(40,55))
gamblePlot(d5,orig=T,eb='sem',ylimit=c(40,55))
gamblePlot(d5prime,orig=T,eb='sem',ylimit=c(35,50))

gamblePlot(d,orig=F,eb='',ylimit=c(40,55),line=T)
gamblePlot(d5,orig=F,eb='',ylimit=c(40,55),line=T)
gamblePlot(d5prime,orig=F,eb='',ylimit=c(35,50),line=T)

rtPlot(d,type="raw",eb="stderr",ylimit=c(600,800),title="All participants n=140",line=T)
rtPlot(d5,type="raw",eb="stderr",ylimit=c(600,800),title="failCatch n=87",line=T)
rtPlot(d5prime,type="raw",eb="stderr",ylimit=c(600,800),,title="successCatch n=53",line=T)


d5<-dgamble[dgamble$uniqueid %in% failCatchId,]
d5prime<-dgamble[dgamble$uniqueid %in% catchSuccessId,]

########################################################################################################################################
##Breaking down by 6 sub conditions - mag/odds

#How does mag affect gamble propensity
#Low mag =Guaranteed amount = $1 or $2
#Mid mag =Guaranteed amount = $3 or $4
#High mag =Guaranteed amount = $5 or $6

#Low mag
dlow<-filter(dgamble,standardGamble==1|standardGamble==2)
c("Number of trials that they gambled on: ",length(dlow$gambleRT[dlow$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dlow$gambleDelay))
#By uniqueId
dBehavioralLow<-dlow %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

#How much did each participant choose to gamble
hist(dBehavioralLow$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low-mag gambles; n =",toString(sum(dBehavioralLow$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')

#RTs histogram
hist(setdiff(dlow$gambleRT,0),xlim=c(0,1300),breaks=50,main=paste("RT low mag gambles; n =",toString(sum(dBehavioralLow$gambleCount)),"gambled trials;",toString(length(dBehavioralLow$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
gamblePlot(dlow,orig=T,ylimit=c(35,50),title="Low Mag")
gamblePlot(dlow,orig=F,eb='sem',ylimit=c(35,50),title="Low Mag")
rtPlot(dlow,type="raw",eb="stderr")

#Plotting RTs with sd
dlowRT<-filter(dlow,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sterrRT=std.error(gambleRT),
            medianSpeed=mean(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

dlowRT$seconds<-dlowRT$binsTime
plot(dlowRT$seconds,dlowRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; Low mag; median RT with stderr; n =",toString(sum(dlow2$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)


for(i in 1:length(dlowRT$seconds)){
  arrows(as.numeric(dlowRT$seconds[i]),as.numeric(dlowRT[i,2]+(as.numeric(dlowRT[i,3]))),as.numeric(dlowRT$seconds[i]),as.numeric(dlowRT[i,2]-(as.numeric(dlowRT[i,3]))),length=0.05, angle=90, code=3)
}

#Speed
#Still need to figure out errorBars
#plot(dlowRT$seconds,dlowRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; Low mag; median Speed with sd; n =",toString(sum(dlow2$gambleCount)),"trials;"),
#     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dlowRT$seconds)){
#  arrows(as.numeric(dlowRT$seconds[i]),as.numeric(dlowRT[i,'medianSpeed']+(as.numeric(dlowRT[i,'sdSpeed']))),as.numeric(dlowRT$seconds[i]),as.numeric(dlowRT[i,'medianSpeed']-(as.numeric(dlowRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
#}


##Mid mag
dmid<-filter(dgamble,standardGamble==3|standardGamble==4)
c("Number of trials that they gambled on: ",length(dmid$gambleRT[dmid$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dmid$gambleDelay))
#By uniqueId
dBehavioralMid<-dmid %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

#How much did each participant choose to gamble
hist(dBehavioralMid$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on mid-mag gambles; n =",toString(sum(dBehavioralMid$trials)),"possible trials;",toString(length(dBehavioralMid$uniqueid)),"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(dmid$gambleRT,0),xlim=c(0,1200),breaks=50,main=paste("RT mid mag gambles; n =",toString(sum(dBehavioralMid$gambleCount)),"gambled trials; ",toString(length(dBehavioralLow$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
gamblePlot(dmid,orig=T,ylimit=c(40,50),title='Mid Mag')
gamblePlot(dmid,orig=F,eb='sem',ylimit=c(40,50),title='Mid Mag')
rtPlot(dmid,type='raw',eb='stderr',title="Mid Mag")
#Plotting RTs with sd
dmidRT<-filter(dmid,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sterrRT=std.error(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

dmidRT$seconds<-dmidRT$binsTime
plot(dmidRT$seconds,dmidRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; Mid mag; median RT with stderr; n =",toString(length(dmid$response[dmid$response=='gamble'])),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
 for(i in 1:length(dmidRT$seconds)){
   arrows(as.numeric(dmidRT$seconds[i]),as.numeric(dmidRT[i,'medianRT']+(as.numeric(dmidRT[i,'sterrRT']))),as.numeric(dmidRT$seconds[i]),as.numeric(dmidRT[i,'medianRT']-(as.numeric(dmidRT[i,'sterrRT']))),length=0.05, angle=90, code=3)
 }
rtPlotFun(dmid,type='raw',eb='stderr',line=T)
#Speed
#Still need to figure out errorBars / how to average
#plot(dmidRT$seconds,dmidRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; Mid mag; median Speed with sd; n =",toString(sum(dmidRT$gambleCount)),"trials;"),
#     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dmidRT$seconds)){
#  arrows(as.numeric(dmidRT$seconds[i]),as.numeric(dmidRT[i,'medianSpeed']+(as.numeric(dmidRT[i,'sdSpeed']))),as.numeric(dmidRT$seconds[i]),as.numeric(dmidRT[i,'medianSpeed']-(as.numeric(dmidRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
#}






##High mag
dhigh<-filter(dgamble,standardGamble==5|standardGamble==6)
c("Number of trials that they gambled on: ",length(dhigh$gambleRT[dhigh$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dhigh$gambleDelay))
#By uniqueId
dBehavioralHigh<-dhigh %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(dBehavioralHigh$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on high mag gambles; n =",toString(sum(dBehavioralHigh$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(dhigh$gambleRT,0),xlim=c(0,1200),breaks=50,main=paste("RT high mag gambles; n =",toString(sum(dBehavioralHigh$gambleCount)),"gambled trials;",toString(length(dBehavioralHigh$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
dhigh2<-dhigh %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(setdiff(gambleRT,0)))
dhigh2$seconds<-dhigh2$binsTime

#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dhigh2$seconds,dhigh2$percentageGambled,xlim = c(0,8),ylim = c(35,50),
     main=paste("High mag; Gamble propensity; n =",toString(sum(dhigh2$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
gamblePlot(dhigh,orig=F,eb='sem',ylimit=c(35,50),title='High Mag')
#Plotting RTs with sd
dhighRT<-filter(dhigh,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sterrRT=std.error(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

dhighRT$seconds<-dhighRT$binsTime
plot(dhighRT$seconds,dhighRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; High mag; median RT with sterr; n =",toString(sum(dhigh2$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dhighRT$seconds)){
#  arrows(as.numeric(dhighRT$seconds[i]),as.numeric(dhighRT[i,'medianRT']+(as.numeric(dhighRT[i,'sdRT']))),as.numeric(dhighRT$seconds[i]),as.numeric(dhighRT[i,'medianRT']-(as.numeric(dhighRT[i,'sdRT']))),length=0.05, angle=90, code=3)
#}
#Speed
#Still need to figure out errorBars
#plot(dhighRT$seconds,dhighRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; High mag; median Speed with sd; n =",toString(sum(dhigh2$gambleCount)),"trials;"),
#     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dhighRT$seconds)){
#  arrows(as.numeric(dhighRT$seconds[i]),as.numeric(dhighRT[i,'medianSpeed']+(as.numeric(dhighRT[i,'sdSpeed']))),as.numeric(dhighRT$seconds[i]),as.numeric(dhighRT[i,'medianSpeed']-(as.numeric(dhighRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
#}



gamblePlot(dhigh,orig=T,ylimit=c(40,50),title='High Mag')
rtPlot(dhigh,type='raw',eb='stderr',title="High Mag")








#How does odds(payout) affect gamble propensity
#Low odds = 1.5 x standard gamble
#Mid odds = 2 x standard gamble
#High odds = 3 x standard gamble



##Low odds
dlowp<-filter(dgamble,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
c("Number of trials that they gambled on: ",length(dlow$gambleRT[dlowp$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dlowp$gambleDelay))
#By uniqueId
dBehavioralLowp<-dlowp %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(dBehavioralLowp$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low odds gambles; n =",toString(sum(dBehavioralLowp$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(dlowp$gambleRT,0),xlim=c(0,1200),breaks=50,main=paste("RT low mag gambles; n =",toString(sum(dBehavioralLowp$gambleCount)),"gambled trials;",toString(length(dBehavioralLowp$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
dlow2p<-dlowp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(setdiff(gambleRT,0)))
dlow2p$seconds<-dlow2p$binsTime

#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dlow2p$seconds,dlow2p$percentageGambled,xlim = c(0,8),ylim = c(15,30),
     main=paste("Low odds; Gamble propensity; n =",toString(sum(dlow2p$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

gamblePlot(dlowp,orig=T,eb='sem',ylimit=c(15,30),title='Low Value Trials')
gamblePlot(dlowp,orig=F,eb='sem',ylimit=c(0,100),title='Low Value Trials')
#Plotting RTs with sd
dlowpRT<-filter(dlowp,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sdRT=sd(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

dlowpRT$seconds<-dlowpRT$binsTime
plot(dlowpRT$seconds,dlowpRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; Low odds; median RT with sd; n =",toString(sum(dlow2p$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dlowpRT$seconds)){
#  arrows(as.numeric(dlowpRT$seconds[i]),as.numeric(dlowpRT[i,2]+(as.numeric(dlowpRT[i,3]))),as.numeric(dlowpRT$seconds[i]),as.numeric(dlowpRT[i,2]-(as.numeric(dlowpRT[i,3]))),length=0.05, angle=90, code=3)
#}
#Speed
#Still need to figure out errorBars
#plot(dlowpRT$seconds,dlowpRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; Low odds; median Speed with sd; n =",toString(sum(dlowp2$gambleCount)),"trials;"),
#     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dlowpRT$seconds)){
#  arrows(as.numeric(dlowpRT$seconds[i]),as.numeric(dlowpRT[i,'medianSpeed']+(as.numeric(dlowpRT[i,'sdSpeed']))),as.numeric(dlowpRT$seconds[i]),as.numeric(dlowpRT[i,'medianSpeed']-(as.numeric(dlowpRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
#}


gamblePlot(dlowp,orig=T,ylimit=c(20,30),title='Low value')
rtPlot(dlowp,type='raw',eb='stderr',title="Low value")





##Mid odds
dmidp<-filter(dgamble,Trialid==1|Trialid==2|Trialid==3|Trialid==4|Trialid==5|Trialid==6)
c("Number of trials that they gambled on: ",length(dmidp$gambleRT[dmidp$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dmidp$gambleDelay))
#By uniqueId
dBehavioralMidp<-dmidp %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(dBehavioralMidp$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on mid odds gambles; n =",toString(sum(dBehavioralMidp$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(dmidp$gambleRT,0),xlim=c(0,1200),breaks=50,main=paste("RT mid mag gambles; n =",toString(sum(dBehavioralMidp$gambleCount)),"gambled trials;",toString(length(dBehavioralMidp$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
dmid2p<-dmidp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(setdiff(gambleRT,0)))
dmid2p$seconds<-dmid2p$binsTime

#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dmid2p$seconds,dmid2p$percentageGambled,xlim = c(0,8),ylim = c(35,50),
     main=paste("Mid odds; Gamble propensity; n =",toString(sum(dmid2p$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
gamblePlot(dmidp,orig=T,ylimit=c(35,50))
gamblePlot(dmidp,orig=F,eb='sem',ylimit=c(35,50))

#Plotting RTs with sd
dmidpRT<-filter(dmidp,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sdRT=sd(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

dmidpRT$seconds<-dmidpRT$binsTime
plot(dmidpRT$seconds,dmidpRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; Mid odds; median RT with sd; n =",toString(sum(dmid2p$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
# for(i in 1:length(dmidpRT$seconds)){
#   arrows(as.numeric(dmidpRT$seconds[i]),as.numeric(dmidpRT[i,2]+(as.numeric(dmidpRT[i,3]))),as.numeric(dmidpRT$seconds[i]),as.numeric(dmidpRT[i,2]-(as.numeric(dmidpRT[i,3]))),length=0.05, angle=90, code=3)
# }
# #Speed
# #Still need to figure out errorBars
# plot(dmidpRT$seconds,dmidpRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; Mid odds; median Speed with sd; n =",toString(sum(dmid2p$gambleCount)),"trials;"),
#      xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
# for(i in 1:length(dmidpRT$seconds)){
#   arrows(as.numeric(dmidpRT$seconds[i]),as.numeric(dmidpRT[i,'medianSpeed']+(as.numeric(dmidpRT[i,'sdSpeed']))),as.numeric(dmidpRT$seconds[i]),as.numeric(dmidpRT[i,'medianSpeed']-(as.numeric(dmidpRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
# }


gamblePlot(dmidp,orig=T,ylimit=c(35,50),title='Mid value')
rtPlot(dmidp,type='raw',eb='stderr',title="Mid value")





##High odds
dhighp<-filter(dgamble,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
c("Number of trials that they gambled on: ",length(dhighp$gambleRT[dhighp$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dhighp$gambleDelay))
#By uniqueId
dBehavioralHighp<-dhighp %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(dBehavioralHighp$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on high odds gambles; n =",toString(sum(dBehavioralHighp$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(dhighp$gambleRT,0),xlim=c(0,1200),breaks=50,main=paste("RT high odds gambles; n =",toString(sum(dBehavioralHighp$gambleCount)),"gambled trials;",toString(length(dBehavioralHighp$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
dhigh2p<-dhighp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(setdiff(gambleRT,0)))
dhigh2p$seconds<-dhigh2p$binsTime

#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dhigh2p$seconds,dhigh2p$percentageGambled,xlim = c(0,8),ylim = c(60,75),
     main=paste("High odds; Gamble propensity; n =",toString(sum(dhigh2p$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

gamblePlot(dhighp,orig=T,ylimit=c(60,75))
gamblePlot(dhighp,orig=F,eb='sem',ylimit=c(40,50))

#Plotting RTs with sd
dhighpRT<-filter(dhighp,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sdRT=sd(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

dhighpRT$seconds<-dhighpRT$binsTime
plot(dhighpRT$seconds,dhighpRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; High odds; median RT with sd; n =",toString(sum(dhigh2p$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)


gamblePlot(dhighp,orig=T,ylimit=c(60,75),title='High value')
rtPlot(dhighp,type='raw',eb='stderr',title="High value")

# for(i in 1:length(dhighpRT$seconds)){
#   arrows(as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,2]+(as.numeric(dhighpRT[i,3]))),as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,2]-(as.numeric(dhighpRT[i,3]))),length=0.05, angle=90, code=3)
# }
# #Speed
# #Still need to figure out errorBars
# plot(dhighpRT$seconds,dhighpRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; High odds; median Speed with sd; n =",toString(sum(dhigh2p$gambleCount)),"trials;"),
#      xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
# for(i in 1:length(dhighpRT$seconds)){
#   arrows(as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,'medianSpeed']+(as.numeric(dhighpRT[i,'sdSpeed']))),as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,'medianSpeed']-(as.numeric(dhighpRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
# }

#####################################################################################
#RPE
#Change the 3 here to whatever you want to do and just rerun it; it's a lot simpler
drpe<-filter(dgamble,dgamble$rpe2<0)
c("Number of trials that they gambled on: ",length(drpe$gambleRT[drpe$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(drpe$gambleDelay))
#By uniqueId
dBehavioralRpe<-drpe %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(dBehavioralRpe$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low RPE gambles; n =",toString(sum(dBehavioralRpe$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(drpe$gambleRT,0),xlim=c(0,1200),breaks=50,main=paste("RT RPE mag gambles; n =",toString(sum(dBehavioralRpe$gambleCount)),"gambled trials;",toString(length(dBehavioralRpe$uniqueid)),"participants"),xlab="Reaction time")
#By GambleDelay
drpe2<-drpe %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(setdiff(gambleRT,0)))
drpe2$seconds<-drpe2$binsTime

#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(drpe2$seconds,drpe2$percentageGambled,xlim = c(0,8),ylim = c(20,35),
     main=paste("RPE mag; Gamble propensity; n =",toString(sum(drpe2$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
gamblePlot(drpe,orig=F,eb='sem',title='RPE',ylimit=c(40,50))

#Plotting RTs with sd
drpeRT<-filter(drpe,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sdRT=sd(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))

drpeRT$seconds<-drpeRT$binsTime
plot(drpeRT$seconds,drpeRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; RPE ; median RT with sd; n =",toString(sum(drpe2$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(drpeRT$seconds)){
#  arrows(as.numeric(drpeRT$seconds[i]),as.numeric(drpeRT[i,'medianRT']+(as.numeric(drpeRT[i,'sdRT']))),as.numeric(drpeRT$seconds[i]),as.numeric(drpeRT[i,'medianRT']-(as.numeric(drpeRT[i,'sdRT']))),length=0.05, angle=90, code=3)
#}
#Speed
#Still need to figure out errorBars
#plot(drpeRT$seconds,drpeRT$medianSpeed,xlim = c(0,8),ylim=c(),main=paste("Group data; RPE mag; median Speed with sd; n =",toString(sum(drpe2$gambleCount)),"trials;"),
#     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(drpeRT$seconds)){
#  arrows(as.numeric(drpeRT$seconds[i]),as.numeric(drpeRT[i,'medianSpeed']+(as.numeric(drpeRT[i,'sdSpeed']))),as.numeric(drpeRT$seconds[i]),as.numeric(drpeRT[i,'medianSpeed']-(as.numeric(drpeRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
#}

gamblePlot(drpe,orig=T,ylimit=c(35,50),title='Low rpe')
rtPlot(drpe,type='raw',eb='stderr',title="Low rpe")


#####################################################################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################


#############################################################################################
#Breaking down by participant

#Some definitions
lowMag<-c(1,2,21,22,31,32)
midMag<-c(3,4,23,24,33,34)
highMag<-c(5,6,25,26,35,36)

lowOdds<-c(1:6)
midOdds<-c(21:26)
highOdds<-c(31:36)

####The following is to just get one participant's data
#INDIVIDUAL
p=370

plotZscore<-FALSE

dsub<-d[d$uniqueid==p,]
dsubgambled<-filter(dsub,response=="gamble")
d3<-dsub %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d3$seconds<-d3$binsTime

par(ps = 12, cex = 1, cex.main = 1)
plot(d3$seconds,d3$percentageGambled,xlim = c(1,7),ylim = c(0,100),
     main=paste("Individual participant data (all trials); n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Percentage Gambled")

mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0),family="binomial")
summary(mlogtemp)


#One participant RT
d3RT<-filter(dsub,gambleRT!=0) %>% 
  group_by(binsTime) %>% 
  summarise(meanRT=mean(gambleRT),
            sdRT=sd(gambleRT),
            medianRtz=median(gambleRTz),
            sdRtz=sd(gambleRTz))
d3RT$seconds<-d3RT$binsTime

plot(d3RT$seconds,d3RT$meanRT,xlim = c(0,8),ylim=c(200,1500),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(d3RT$seconds)){
  arrows(as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]+(as.numeric(d3RT[i,3]))),as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]-(as.numeric(d3RT[i,3]))),length=0.05, angle=90, code=3)
}
if(plotZscore){
  plot(d3RT$seconds,d3RT$meanRT,xlim = c(0,8),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
       xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
  for(i in 1:length(d3RT$seconds)){
    arrows(as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]+(as.numeric(d3RT[i,3]))),as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]-(as.numeric(d3RT[i,3]))),length=0.05, angle=90, code=3)
  }
}

msubRT<-lm(dsubgambled$gambleRT~dsubgambled$gambleDelay)
abline(msubRT)
summary(msubRT)

#Gamble slope for participant p
summary(mlogtemp)$coefficients[2]

#RT slope for participant p
msubRT$coefficients[2]

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#Looping through  all participants data at an indiv. level to extract useful 

subj<-unique(d$uniqueid)
intN<-NULL
botN<-NULL
rtn<-NULL
oddsN<-NULL
oddsNtemp<-data.frame(matrix(NA, ncol = 2, nrow =1))
  colnames(oddsNtemp)[1]<-"OddsGamblingScore"
  colnames(oddsNtemp)[2]<-"Participant"
magN<-NULL
magNtemp<-data.frame(matrix(NA, ncol = 2, nrow =1))
  colnames(magNtemp)[1]<-"MagGamblingScore"
  colnames(magNtemp)[2]<-"Participant"  

acf1<-NULL
acf2<-NULL
acf3<-NULL
acf4<-NULL
acf5<-NULL

rtSlopes<-NULL
gambleSlopes<-NULL
run<-NULL
slopeDF<-NULL
plotRT=FALSE
plotGD=FALSE
#Add in knobs for different sub categories (though this number is very small....)

#Participants is default (all participants)
#If you want to run sub groups, go to bottom of script
for(i in Participants){
  print(i)
  dsub<-d[d$uniqueid==i,]
  dsubgambled<-filter(dsub,response=="gamble")
  dsubhigh<-filter(dsub,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
  dsublow<-filter(dsub,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
  #Breaking down by subFilter
  d4behavioralHigh<-dsubhigh %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
  d4behavioralLow<-dsublow %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
  oddsNtemp[1,1]=as.integer(d4behavioralHigh$gambleCount-d4behavioralLow$gambleCount)
  oddsNtemp[1,2]=as.character(i)
  oddsN=rbind(oddsN,oddsNtemp)
  
  dsubhigh<-filter(dsub,Trialid==5|Trialid==6|Trialid==25|Trialid==26|Trialid==35|Trialid==36)
  dsublow<-filter(dsub,Trialid==1|Trialid==2|Trialid==21|Trialid==22|Trialid==31|Trialid==32)
  d4behavioralHigh<-dsubhigh %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
  d4behavioralLow<-dsublow %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
  magNtemp[1,1]=as.integer(d4behavioralHigh$gambleCount-d4behavioralLow$gambleCount)
  magNtemp[1,2]=as.character(i)
  magN=rbind(magNtemp,magN)
  
  
  #Breaking down subdf by gambleDelay
  d4<-dsub[dsub$gambleDelay!=0,] %>% 
    group_by(binsTime) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  d4$seconds<-d4$binsTime
  
  if (plotGD){
    plot(d4$seconds,d4$percentageGambled,xlim = c(0,7),ylim = c(0,100),
         main=paste("Individual participant data; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
         xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
  }
  #One participant RT
  d4RT<-filter(dsub,gambleRT!=0) %>% 
    group_by(binsTime) %>% 
    summarise(medianRT=median(gambleRT),
              sdRT=sd(gambleRT))
  d4RT$seconds<-d4RT$binsTime
  
  if (plotRT){
    plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
         main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
         xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
  }
  #This is top check for gamble ramp
  mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0),family="binomial")
  #This is glm for RT by gamble Delay
  mtempRT<-lm(d4RT$medianRT~d4RT$seconds)
  
  #This checks to see if any participant is gamble ramping
  if(summary(mlogtemp)$coefficients[8]<.1 & summary(mlogtemp)$coefficients[2]>0){
    intN<-c(intN,i)
  }
  #Check to see if any participant has a RT that's susupiciously low..
  if(mean(setdiff(dsub$outcomeRT,0),na.rm=TRUE)<200){
    botN<-c(botN,i)
  }
  #Check to see if participant has a downwards decreasing RT ramp

  if(summary(mtempRT)$coefficients[8]<.1 & summary(mtempRT)$coefficients[2]<0){
    rtn<-c(rtn,i)
  }
  
  
  #Making acf coefficient vectors
  acf1<-c(acf1,acf(dsub$gambled,lag.max=5,plot = FALSE)$acf[2])
  acf2<-c(acf2,acf(dsub$gambled,lag.max=5,plot = FALSE)$acf[3])
  acf3<-c(acf3,acf(dsub$gambled,lag.max=5,plot = FALSE)$acf[4])
  acf4<-c(acf4,acf(dsub$gambled,lag.max=5,plot = FALSE)$acf[5])
  acf5<-c(acf5,acf(dsub$gambled,lag.max=5,plot = FALSE)$acf[6])
  
  #Making df of gambleSlopes, rtSlopes, and i (who was just analyzed)
  
  rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])
  gambleSlopes<-c(gambleSlopes,summary(mlogtemp)$coefficients[2])
  run<-c(run,i)
}

slopeDF<-data.frame(cbind(run,rtSlopes,gambleSlopes))
a<-oddsN[order(oddsN$OddsGamblingScore),]
#oddsN is the median split of people who respond well to odds
oddsN<-as.integer(a$Participant[(length(a$Participant)/2):length(a$Participant)])
#Quartile split
oddsN<-oddsN[(length(oddsN)/2):(length(oddsN))]

#Plot of gamble slopes vs. rtSlopes
#POSTER graph
#POSTER3

par(bty='n')
box(which="plot",lty='solid')
plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='Individual Gamble slopes (% change/second)',ylab='Individual RT Slopes (ms/second)',pch=16,cex=0.8,main='Gamble slopes vs. RT slopes; n=141',xlim=c(-.7,.7),ylim=c(-80,50),bty='7')
abline(v=0,col="black")
abline(h=0,col="black")
#with(slopeDF, text(slopeDF$rtSlopes~slopeDF$gambleSlopes, labels = slopeDF$run,cex=.8), pos = 2)
m1<-lm(slopeDF$rtSlopes~slopeDF$gambleSlopes)
#Make ggExtrav(ggMarginal)
#Andrew3
ap3<-ggplot(slopeDF,aes(gambleSlopes,rtSlopes))+geom_point()+theme_bw()+ theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0)+geom_vline(xintercept=0)+
  labs(y="RT Slopes",x="Gamble Slopes")+xlim(-.75,.75)+ylim(-110,110)
ggExtra::ggMarginal(ap3,type='density')


abline(m1)
cor(slopeDF$rtSlopes~slopeDF$gambleSlopes)
summary(m1)

#TO do make plot of gamble slopes histogram
#POSTER graph
#POSTER4
par(bty="7")
hist(slopeDF$gambleSlopes,xlab='Individual Gamble slopes (% change/second)',ylab='Frequency',pch=16,cex=0.8,main='Individual Gamble slopes; n=88',
     breaks=50,xlim=c(-.7,.7),bty='7',col='black')
abline(v=0,col="black")
t.test(slopeDF$gambleSlopes)



#TO do make plot of RT slopes histogram
#POSTER graph
#POSTER5
hist(slopeDF$rtSlopes,xlab='Individual RT Slopes (ms/seconds)',ylab='Frequency',pch=16,cex=0.8,main='Individual RT slopes; n=88',breaks=50,xlim=c(-150,150),col='black')
abline(v=0,col="black")
t.test(slopeDF$rtSlopes)


# #Same plot but rtSlopes is on x axis
# plot(slopeDF$gambleSlopes~slopeDF$rtSlopes,xlab='rtSlopes',ylab='gambleSlopes',main='All Participants (n=112)',xlim=c(-90,50),ylim=c(-1,1))
# with(slopeDF, text(slopeDF$gambleSlopes~slopeDF$rtSlopes, labels = slopeDF$run,cex=.8), pos = 2)
# m1<-lm(slopeDF$gambleSlopes~slopeDF$rtSlopes)
# abline(m1)













########Looking into subsets of participants, high mag preferetial, odds, etc.


#THIS IS WHATEVER SUBGROUP YOU WANT TO LOOK AT
#SUMMARY GRAPHS
#SUBGROUP 1 - excelsior

summaryMagFilter=F
  summaryMagCond='high'
summaryOddsFilter=F
  summaryOddsCond='highp'
#sfn1  
#New way which is better
 d5<-dgamble[dgamble$uniqueid %in% rtn,]
# d5<-dgamble[dgamble$uniqueid %in% Participants,]
 d5<-dgamble[dgamble$uniqueid %in% intN,]
 d5<-dgamble[dgamble$uniqueid %in% catchSuccessId,]
 d5<-dgamble[dgamble$uniqueid %in% male,]
 d5<-dgamble[dgamble$uniqueid %in% highGamblers,]
d5<-dgamble[dgamble$uniqueid %in% failCatchId,]
d5<-dgamble[dgamble$uniqueid %in% imp,]

#This is if you want intersection of two groups
#d5<-dgamble[dgamble$uniqueid %in% inters bect(catchSuccessId,highGamblers),]


d5gamble<-filter(d5,gambleDelay!=0)
gamblePlot(d5,orig=T,ylimit=c(40,60),title='failCatch',line=T)
rtPlot(d5,type='raw',eb='stderr',title='failCatch',ylimit=c(600,900),line=F)



#This filters summary d5 by odds/mag if specified above via T/F
if(summaryMagFilter){
  d5<-filter(d5,magCond==summaryMagCond)
}
if(summaryOddsFilter){
  d5<-filter(d5,oddsCond==summaryOddsCond)
}

c("Number of trials that they gambled on:",length(d5$gambleRT[d5$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on:",length(d5$gambleDelay))


#By uniqueId
d5Behavioral<-d5 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,20),xlim=c(-5,100),main=paste("Propensity to gamble; n =",toString(sum(d5Behavioral$trials)),"possible trials;"),xlab="Percentage of time gambled")
#RTs histogram
#hist(setdiff(d5$gambleRT,0),xlim=c(0,1500),breaks=50,main=paste("RT; n =",toString(sum(d5Behavioral$gambleCount)),"gambled trials;"),xlab="Reaction time")
#By GambleDelay
d52<-d5 %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
d52$seconds<-d52$binsTime
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
 plot(d52$seconds,d52$percentageGambled,xlim = c(0,8),ylim = c(0,100),
      main=paste("Gamble propensity; n =",toString(sum(d52$gambleCount))," gambled trials;"),
      xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

plot(d52$seconds,d52$percentageGambled,xlim = c(0,8),ylim = c(40,60),
     main=paste("Gamble propensity; n =",toString(sum(d52$gambleCount)),"gambled trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5pRT<-filter(d5,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d5pRT$seconds<-d5pRT$binsTime
plot(d5pRT$seconds,d5pRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; median RT with sd; n =",toString(sum(d52$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)

d5high<-filter(d5,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
d5low<-filter(d5,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
#Breaking down by subFilter to get Odds score and mag score
d5behavioralHigh<-d5high %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d5behavioralLow<-d5low %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
oddsScore<-mean(d5behavioralHigh$percentageGambled)-mean(d5behavioralLow$percentageGambled)
oddsScore

#Sig effect of gambleDelay on gambling? 
summary(glm(gambled~gambleDelay,
            data=filter(d5,gambleDelay!=0),family="binomial"))

#Sig effect of gambleDelay on RT when gambling? 
d6<-filter(d5,response=='gamble')
  summary(glm(gambleRT~gambleDelay,
              data=d6))

#Overlaying RT histograms of d over d5 of sure thing and gamble
#d5 should be defined above
rtall<-filter(d5,response=='success')
rtsub<-filter(d5prime,response=='success')

#Making venn diagrams of overlapping participants
#setdiff(a,b)
#Things that are in A but not in B
# grid.newpage()
# #draw.single.venn(length(Participants),category="All participants",fill=c('cornflower blue'),alpha=.5)
# grid.newpage()
# draw.pairwise.venn(length(Participants),length(unique(d5$uniqueid)),length(unique(d5$uniqueid)),
#                    fill=c(' cornflower blue',"red"),category=c("All participants","subgroup"),
#                    alpha=c(.5,.5),cat.pos=c(0,0),cat.dist=rep(-0.025,2))

hist(rtall$outcomeRT,col=rgb(0,0,1,0.5), main='Reaction Times when accepting sure thing', xlab='Reaction Time (ms)',breaks=40,xlim=c(0,800),ylim=c(0,350))
abline(v=median(rtall$outcomeRT),col="blue",lwd=2)
hist(rtsub$outcomeRT,col=rgb(1,0,0,0.5), add=T,breaks=40)
abline(v=median(rtsub$outcomeRT),col="red",lwd=2)
legend(600,350,cex=.7, bty = "n",legend=c("Failed at least one catch trial","Passed all the catch trials"),col=c("blue","red"),title="",pch=15)

t.test((1/rtall$outcomeRT),(1/rtsub$outcomeRT))

rtall<-filter(d5,response=='gamble',gambleRT!=0)
rtsub<-filter(d5prime,response=='gamble',gambleRT!=0)

hist(rtall$gambleRT,col=rgb(0,0,1,0.5), main='Reaction Times when gambling', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1300),ylim=c(0,200))
abline(v=median(rtall$gambleRT),col="blue",lwd=2)
hist(rtsub$gambleRT,col=rgb(1,0,0,0.5), add=T,breaks=50)
abline(v=median(rtsub$gambleRT),col="red",lwd=2)
legend(900,200,cex=.7, bty = "n",legend=c("Failed at least one catch trial","Passed all the catch trials"),col=c("blue","red"),title="",pch=15)

t.test((1/rtall$gambleRT),(1/rtsub$gambleRT))



#guillaume1
#Now making a second 'd5' so you can compare summary graphs of two summary plots of subgroups

#THIS IS WHATEVER SUBGROUP YOU WANT TO LOOK AT
#SUMMARY GRAPHS
#SUBGROUP 2, stored in df called dfprime

summaryMagFilter=FALSE
summaryMagCond='high'
summaryOddsFilter=FALSE
summaryOddsCond='highp'

#New way which is better
# d5prime<-dgamble[dgamble$uniqueid %in% rtn,]
# d5prime<-dgamble[dgamble$uniqueid %in% Participants,]
# d5prime<-dgamble[dgamble$uniqueid %in% intN,]
 d5prime<-dgamble[dgamble$uniqueid %in% catchSuccessId,]
# d5prime<-dgamble[dgamble$uniqueid %in% oddsN,]
# d5prime<-dgamble[dgamble$uniqueid %in% highGamblers,]
#d5prime<-dgamble[dgamble$uniqueid %in% failCatchId,]
# d5prime<-dgamble[dgamble$uniqueid %in% fail5,]
# d5prime<-dgamble[dgamble$uniqueid %in% truehigh,]

 gamblePlot(d5prime,orig=F,ylimit=c(35,50),title='CatchSuccess')
 rtPlot(d5prime,type='raw',eb='stderr',title="CatchSuccess")
 
 
d5prime<-filter(d5prime,gamblePrevTrial==1) 
 
#This filters summary d5prime by odds/mag if specified above via T/F
if(summaryMagFilter){
  filter(d5prime,magCond==SummaryMagCond)
}
if(summaryOddsFilter){
  filter(d5prime,oddsCond==SummaryOddsCond)
}

c("Number of trials that they gambled on:",length(d5prime$gambleRT[d5prime$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on:",length(d5prime$gambleDelay))

#n<-filter(d5primeBehavioral,percentageGambled<60)

d5prime<-d5prime[dgamble$uniqueid %in% n,]

#By uniqueId
d5primeBehavioral<-d5prime %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(d5primeBehavioral$percentageGambled,breaks=50,ylim=c(0,20),xlim=c(-5,100),main=paste("Propensity to gamble; n =",toString(sum(d5primeBehavioral$trials)),"possible trials;"),xlab="Percentage of time gambled")
#RTs histogram
#hist(setdiff(d5prime$gambleRT,0),xlim=c(0,1500),breaks=50,main=paste("RT; n =",toString(sum(d5primeBehavioral$gambleCount)),"gambled trials;"),xlab="Reaction time")
#By GambleDelay
d5prime2<-d5prime %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
d5prime2$seconds<-d5prime2$binsTime
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(d5prime2$seconds,d5prime2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Gamble propensity; n =",toString(sum(d5prime2$gambleCount))," gambled trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

plot(d5prime2$seconds,d5prime2$percentageGambled,xlim = c(0,8),ylim = c(35,50),
     main=paste("Gamble propensity; n =",toString(sum(d5prime2$gambleCount)),"gambled trials;",toString(length(unique(d5prime$uniqueid))),"participants"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5primepRT<-filter(d5prime,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d5primepRT$seconds<-d5primepRT$binsTime
plot(d5primepRT$seconds,d5primepRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; median RT with sd; n =",toString(sum(d5prime2$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)

d5primehigh<-filter(d5prime,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
d5primelow<-filter(d5prime,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
#Breaking down by subFilter to get Odds score and mag score
d5primebehavioralHigh<-d5primehigh %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d5primebehavioralLow<-d5primelow %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
oddsScore<-mean(d5primebehavioralHigh$percentageGambled)-mean(d5primebehavioralLow$percentageGambled)
oddsScore

#Sig effect of gambleDelay on gambling? 
summary(glm(gambled~gambleDelay,
            data=filter(d5prime,gambleDelay!=0),family="binomial"))

#Sig effect of gambleDelay on RT when gambling? 
d6<-filter(d5prime,response=='gamble')
summary(glm(gambleRT~gambleDelay,
            data=d6))


#Overlaying histograms of d5 and d5prime
#d5 should be defined above
#rtsub1 and rtsub should be identical, just used in different contextx
rtsub1<-filter(d5,response=='success')
rtsub2<-filter(d5prime,response=='success')

#This plots the two subgroups histograms against one another
hist(rtsub1$outcomeRT,col=rgb(1,0,0,0.5), main='RTs when accepting sure thing', xlab='RT',breaks=40,xlim=c(0,1300))
abline(v=median(rtsub1$outcomeRT),col="red",lwd=2)
hist(rtsub2$outcomeRT,col=rgb(0,0,1,0.5), add=T,breaks=40)
abline(v=median(rtsub2$outcomeRT),col="blue",lwd=2)
t.test((1/rtsub1$outcomeRT),(1/rtsub2$outcomeRT))

rtsub1<-filter(d5,response=='gamble')
rtsub2<-filter(d5prime,response=='gamble')

hist(rtall$gambleRT,col=rgb(1,0,0,0.5), main='RTs when gambling', xlab='RT',breaks=40,xlim=c(0,1300))
abline(v=median(rtall$gambleRT),col="red",lwd=2)

hist(rtsub2$gambleRT,col=rgb(0,0,1,0.5), add=T,breaks=40)
abline(v=median(rtsub2$gambleRT),col="blue",lwd=2)
t.test((1/rtsub1$gambleRT),(1/rtsub2$gambleRT))
#Shenhav2
#Venn diagram between all all participants, d5, and d5prime
grid.newpage()
draw.triple.venn(area1=length(Participants),area2=length(unique(d5$uniqueid)),area3=length(unique(d5prime$uniqueid)),
                 n12=length(intersect(Participants,unique(d5$uniqueid))),n23=length(intersect(unique(d5$uniqueid),unique(d5prime$uniqueid))),
                 n13=length(intersect(Participants,unique(d5prime$uniqueid))),n123=length(intersect(intersect(Participants,unique(d5$uniqueid)),unique(d5prime$uniqueid))),
                 fill=c(' cornflower blue',"red","springgreen"),category=c("All participants","highGamblers","ParticipantsFailedCatch"),
                   alpha=c(.3,.3,.3),cat.pos=c(0,0,0))


##############################################################################################################################
##############################################################################################################################

#shenhav3
#Looping through subgroups to get INDIVIDUAL graphs 
#indivd5<-failCatchId

subplotRT=FALSE
subplotGD=FALSE
soddsN=NULL
oddsScoreKnob=FALSE
magScoreKnob=FALSE
smagN=NULL
rtSlopes<-NULL
gambleSlopes<-NULL
run<-NULL
subslope1DF<-NULL
gambleMeans<-NULL

#Want to look at subgroups of participants?
oddsFilter<-FALSE
  subOddsCond<-'highp'
MagFilter<-FALSE
  subMagCond<-'high'

  
#Subgroup 1
for(i in failCatchId){
  print(i)
  dsub<-filter(d,uniqueid==i)
  
  if(oddsFilter){
    dsub<-filter(dsub,oddsCond==subOddsCond)
  }
  if(MagFilter){
    dsub<-filter(dsub,magCond==subMagCond)
  }
  if(oddsScoreKnob){
    dsubhigh<-filter(dsub,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
    dsublow<-filter(dsub,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
    #Breaking down by subFilter to get OddsScore
    d4behavioralHigh<-dsubhigh %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
    d4behavioralLow<-dsublow %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
    oddsNtemp[1,1]=as.integer(d4behavioralHigh$gambleCount-d4behavioralLow$gambleCount)
    oddsNtemp[1,2]=as.character(i)
    soddsN=rbind(soddsN,oddsNtemp)}
  
  #Breaking down by subFilter to get MagScore
  if(magScoreKnob){
    dsubhigh<-filter(dsub,Trialid==5|Trialid==6|Trialid==25|Trialid==26|Trialid==35|Trialid==36)
    dsublow<-filter(dsub,Trialid==1|Trialid==2|Trialid==21|Trialid==22|Trialid==31|Trialid==32)
    d4behavioralHigh<-dsubhigh %>% 
      group_by(uniqueid) %>% 
      summarise(trials=length(trialNumber),
                gambleCount=sum(response=="gamble"),
                didNotGamble=sum(response=="fail"|response=="success"),
                percentageGambled=round(gambleCount/trials*100))
    
    d4behavioralLow<-dsublow %>% 
      group_by(uniqueid) %>% 
      summarise(trials=length(trialNumber),
                gambleCount=sum(response=="gamble"),
                didNotGamble=sum(response=="fail"|response=="success"),
                percentageGambled=round(gambleCount/trials*100))
    
    magNtemp[1,1]=as.integer(d4behavioralHigh$gambleCount-d4behavioralLow$gambleCount)
    magNtemp[1,2]=as.character(i)
    smagN=rbind(smagN,magNtemp)
  }
  
  #Breaking down subdf by gambleDelay
  d4<-dsub %>% 
    group_by(binsTime) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  d4$seconds<-d4$binsTime
  
  if (subplotGD){
    plot(d4$seconds,d4$percentageGambled,xlim = c(1,7),ylim = c(0,100),
         main=paste("Individual participant data; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
         xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
  }
  
  
  #Have to put in a check to see if they actually gambled
  if(sum(d4$percentageGambled)>0){
    #One participant RT
    d4RT<-filter(dsub,gambleRT!=0) %>% 
      group_by(binsTime) %>% 
      summarise(medianRT=median(gambleRT),
                sdRT=sd(gambleRT))
    d4RT$seconds<-d4RT$binsTime
    
    if (subplotRT){
      plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
           main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
           xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
    }
    
    #This is tracking all the gamble slopes of everyone in subgroup
    mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0),family="binomial")
    gambleSlopes<-c(gambleSlopes,summary(mlogtemp)$coefficients[2])
  
    
    tempRTdf<-filter(dsub,response=='gamble')
    
    mtempRT<-lm(d4RT$medianRT~d4RT$seconds)
    
    #This is tracking all the RT slopes of everyone in subgroup
    
    rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])
    
    run<-c(run,i)
    
    #This is just one number to see how much overall they gambled
    gambleMeans=c(gambleMeans,mean(d4$percentageGambled))
    
    }
  
  

}

  
#sfnhist
subslope1DF<-data.frame(cbind(run,rtSlopes,gambleSlopes,gambleMeans))
par(bty="7")
hist(subslope1DF$gambleSlopes,xlab='Individual Gamble slopes (% change/second)',ylab='Frequency',pch=16,cex=0.8,main='Individual Gamble slopes; SuccessCatch; n=58',
     breaks=50,xlim=c(-1,1),bty='7',col='black')
abline(v=0,col='black')

par(new=T)

hist(subslope1DF$gambleSlopes[subslope1DF$gambleSlopes>0],pch=16,cex=0.8,
     breaks=50,xlim=c(-1,1),bty='7',col='green',axes=F,xlab="",ylab="",main="")

par(bty="7")
hist(subslope1DF$rtSlopes,xlab='Individual RT Slopes (ms/seconds)',ylab='Frequency',pch=16,cex=0.8,
     main='Individual RT slopes; n=58',breaks=50,xlim=c(-100,100),col='black')
abline(v=0,col='black')
par(new=T)
hist(subslope1DF$rtSlopes[subslope1DF$rtSlopes<0],xlab='Individual RT Slopes (ms/seconds)',ylab='Frequency',pch=16,cex=0.8,main='Individual RT slopes; n=88',breaks=50,xlim=c(-100,100),col='green')

  
  
  
  #guillaume2
  #Subgroup 2
  #Looping through subgroups to get INDIVIDUAL graphs
 # indivd5prime<-intN

  subplotRT=FALSE
  subplotGD=F
  soddsN=NULL
  oddsScoreKnob=FALSE
  magScoreKnob=FALSE
  smagN=NULL
  rtSlopes<-NULL
  gambleSlopes<-NULL
  run<-NULL
  gambleMeans<-NULL
  subslope2DF<-NULL

  #Want to look at subgroups of participants?
  oddsFilter<-F
  subOddsCond<-'highp'
  MagFilter<-F
  subMagCond<-'low'


#Subgroup 2
for(i in catchSuccessId){
    print(i)
    dsub<-filter(d,uniqueid==i)

    if(oddsFilter){
      dsub<-filter(dsub,oddsCond==subOddsCond)
    }
    if(MagFilter){
      dsub<-filter(dsub,magCond==subMagCond)
    }
    if(oddsScoreKnob){
      dsubhigh<-filter(dsub,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
      dsublow<-filter(dsub,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
      #Breaking down by subFilter to get OddsScore
      d4behavioralHigh<-dsubhigh %>%
        group_by(uniqueid) %>%
        summarise(trials=length(trialNumber),
                  gambleCount=sum(response=="gamble"),
                  didNotGamble=sum(response=="fail"|response=="success"),
                  percentageGambled=round(gambleCount/trials*100))

      d4behavioralLow<-dsublow %>%
        group_by(uniqueid) %>%
        summarise(trials=length(trialNumber),
                  gambleCount=sum(response=="gamble"),
                  didNotGamble=sum(response=="fail"|response=="success"),
                  percentageGambled=round(gambleCount/trials*100))

      oddsNtemp[1,1]=as.integer(d4behavioralHigh$gambleCount-d4behavioralLow$gambleCount)
      oddsNtemp[1,2]=as.character(i)
      soddsN=rbind(soddsN,oddsNtemp)}

    #Breaking down by subFilter to get MagScore
    if(magScoreKnob){
      dsubhigh<-filter(dsub,Trialid==5|Trialid==6|Trialid==25|Trialid==26|Trialid==35|Trialid==36)
      dsublow<-filter(dsub,Trialid==1|Trialid==2|Trialid==21|Trialid==22|Trialid==31|Trialid==32)
      d4behavioralHigh<-dsubhigh %>%
        group_by(uniqueid) %>%
        summarise(trials=length(trialNumber),
                  gambleCount=sum(response=="gamble"),
                  didNotGamble=sum(response=="fail"|response=="success"),
                  percentageGambled=round(gambleCount/trials*100))

      d4behavioralLow<-dsublow %>%
        group_by(uniqueid) %>%
        summarise(trials=length(trialNumber),
                  gambleCount=sum(response=="gamble"),
                  didNotGamble=sum(response=="fail"|response=="success"),
                  percentageGambled=round(gambleCount/trials*100))

      magNtemp[1,1]=as.integer(d4behavioralHigh$gambleCount-d4behavioralLow$gambleCount)
      magNtemp[1,2]=as.character(i)
      smagN=rbind(smagN,magNtemp)
    }

    #Breaking down subdf by gambleDelay
    d4<-dsub %>%
      group_by(binsTime) %>%
      summarise(trials=length(trialNumber),
                gambleCount=sum(response=="gamble"),
                didNotGamble=sum(response=="fail"|response=="success"),
                percentageGambled=round(gambleCount/trials*100))
    d4$seconds<-d4$binsTime

    if (subplotGD){
      plot(d4$seconds,d4$percentageGambled,xlim = c(1,7),ylim = c(0,100),
           main=paste("Individual participant data; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
           xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
    }


    #Have to put in a check to see if they actually gambled
    if(sum(d4$percentageGambled)>0){
      #One participant RT
      d4RT<-filter(dsub,gambleRT!=0) %>%
        group_by(binsTime) %>%
        summarise(medianRT=median(gambleRT),
                  sdRT=sd(gambleRT))
      d4RT$seconds<-d4RT$binsTime

      if (subplotRT){
        plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
             main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
             xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
      }

      #This is tracking all the gamble slopes of everyone in subgroup
      mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0),family="binomial")
      gambleSlopes<-c(gambleSlopes,summary(mlogtemp)$coefficients[2])


      tempRTdf<-filter(dsub,response=='gamble')

      mtempRT<-lm(d4RT$medianRT~d4RT$seconds)

      #This is tracking all the RT slopes of everyone in subgroup

      rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])

      run<-c(run,i)

      #This is just one number to see how much overall they gambled
      gambleMeans=c(gambleMeans,mean(d4$percentageGambled))

    }



  }
subslope2DF<-data.frame(cbind(run,rtSlopes,gambleSlopes,gambleMeans))



#Making vectors for barplots of gambleSlopes and RTslopes
if(oddsFilter & subOddsCond=='lowp'){
  lowvaluegambleslopes<-subslope2DF$gambleSlopes
  lowvaluertslopes<-subslope2DF$rtSlopes
  lowvaluegamblepropensity<-subslope2DF$gambleMeans
}else if(oddsFilter & subOddsCond=='midp'){
  midvaluegambleslopes<-subslope2DF$gambleSlopes
  midvaluertslopes<-subslope2DF$rtSlopes
  midvaluegamblepropensity<-subslope2DF$gambleMeans
}else if(oddsFilter & subOddsCond=='highp'){
  highvaluegambleslopes<-subslope2DF$gambleSlopes
  highvaluertslopes<-subslope2DF$rtSlopes
  highvaluegamblepropensity<-subslope2DF$gambleMeans
}else if(MagFilter & subMagCond=='low'){
  lowmaggambleslopes<-subslope2DF$gambleSlopes
  lowmagrtslopes<-subslope2DF$rtSlopes
  lowmaggamblepropensity<-subslope2DF$gambleMeans
}else if(MagFilter & subMagCond=='mid'){
  midmaggambleslopes<-subslope2DF$gambleSlopes
  midmagrtslopes<-subslope2DF$rtSlopes
  midmaggamblepropensity<-subslope2DF$gambleMeans
}else if(MagFilter & subMagCond=='high'){
  highmaggambleslopes<-subslope2DF$gambleSlopes
  highmagrtslopes<-subslope2DF$rtSlopes
  highmaggamblepropensity<-subslope2DF$gambleMeans
}else if (MagFilter==FALSE & oddsFilter==FALSE){
  totalgambleslopes<-subslope2DF$gambleSlopes
  totalrtslopes<-subslope2DF$rtSlopes
  totalgamblepropensity<-subslope2DF$gambleMeans
}else {
  disp("error????")
}

#sfn3
#Subplot dot plot
#Adding colors to overall plot of gambleSlopes (only 1 of the two subgroups)
plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',main='All Participants gambleSlopes vs. rtSlopes n=140',xlim=c(-1,1),ylim=c(-90,50),pch=16,col='red')

abline(v=0,col='black')
abline(h=0,col='black')

par(new=TRUE)
plot(subslope1DF$rtSlopes~subslope1DF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',xlim=c(-1,1),ylim=c(-90,50),col='red',pch=16)
par(new=TRUE)
plot(subslope2DF$rtSlopes~subslope2DF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',xlim=c(-1,1),ylim=c(-90,50),col='blue',pch=16)
par(new=TRUE)
plot(fail3df$rtSlopes~fail3df$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',xlim=c(-1,1),ylim=c(-90,50),col='maroon',pch=18)
par(new=TRUE)
plot(fail4df$rtSlopes~fail4df$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',xlim=c(-1,1),ylim=c(-90,50),col='orange',pch=18)
par(new=TRUE)
plot(fail5df$rtSlopes~fail5df$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',xlim=c(-1,1),ylim=c(-90,50),col='red',pch=18)
#legend(.5,40,legend=c("1","2","3","4","5"),col=c("blue","green","maroon","orange","red"),title="Number of catch Trials failed",pch=18)
legend(.5,55,cex=.7, bty = "n",legend=c("Failed at least one catch trial","Passed all the catch trials"),col=c("red","blue"),title="",pch=16)

with(subslope2DF, text(subslope2DF$rtSlopes~subslope2DF$gambleSlopes, labels = subslope2DF$run,cex=.6), pos = 2)
with(subslope1DF, text(subslope1DF$rtSlopes~subslope1DF$gambleSlopes, labels = subslope1DF$run,cex=.6), pos = 2)

#Txtlabels<-c(rep('SuccessfulCatch ',length(successfulCatchM$gambleSlopes)),rep('FailCatch',length(failCatchM$gambleSlopes)))
#sds<-c(rep(sd(successfulCatchM$gambleSlopes),length(successfulCatchM$gambleSlopes)),rep(sd(failCatchM$gambleSlopes),length(failCatchM$gambleSlopes)))

#Plotting both subgroups gamble slopes against each other 
plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',main='All Participants n=88',xlim=c(-1,1),ylim=c(-90,50))
with(failCatchM, text(failCatchM$rtSlopes~failCatchM$gambleSlopes, labels = failCatchM$run,cex=.6), pos = 2)
Txtlabels<-c(rep('SuccessfulCatch ',length(successfulCatchM$gambleSlopes)),rep('FailCatch',length(failCatchM$gambleSlopes)))
sds<-c(rep(sd(successfulCatchM$gambleSlopes),length(successfulCatchM$gambleSlopes)),rep(sd(failCatchM$gambleSlopes),length(failCatchM$gambleSlopes)))

#Barplots
#POSTER6
##Plotting the slopes of different conditions doesn't look good
#barplot(c(mean(lowmaggambleslopes[lowmaggambleslopes %in% boxplot.stats(lowmaggambleslopes)$out]),mean(midmaggambleslopes[midmaggambleslopes %in% boxplot.stats(midmaggambleslopes)$out]),mean(highmaggambleslopes[highmaggambleslopes %in% boxplot.stats(highmaggambleslopes)$out])),names.arg = c("Lowmag","Midmag","Highmag"),bty='n')

#Gamble propensity does though - Mag
bpCol<- rgb(200, 200, 200, max = 255, alpha = 125, names = "blue50")
magMeans<-c(mean(lowmaggamblepropensity),mean(midmaggamblepropensity),mean(highmaggamblepropensity))
pg6<-barplot(magMeans,names.arg = c("Low magnitude","Mid magnitude","High magnitude"),ylim=c(0,80),ylab="Gamble Propensity",col=bpCol, main="Effect of Magnitude on Gamble propensity for failCatch (n=82)")
magSems<-c((std.error(lowmaggamblepropensity)),(std.error(midmaggamblepropensity)),(std.error(highmaggamblepropensity)))
magSds<-c((sd(lowmaggamblepropensity)),(sd(midmaggamblepropensity)),(sd(highmaggamblepropensity)))
arrows(pg6,magMeans-magSems,pg6,magMeans+magSems,lwd=2,angle=90,code=3)


#To do gamble propensity Value
#POSTER7
valueMeans<-c(mean(lowvaluegamblepropensity),mean(midvaluegamblepropensity),mean(highvaluegamblepropensity))
pg7<-barplot(valueMeans,names.arg = c("Low value","Mid value","High value"),ylim=c(0,80),ylab="Gamble Propensity",col=bpCol,main="Effect of Value on Gamble propensity for failCatch (n=82)")
valueSems<-c((sd(lowvaluegamblepropensity)/sqrt(length(lowvaluegamblepropensity))),(sd(midvaluegamblepropensity)/sqrt(length(midvaluegamblepropensity))),(sd(highvaluegamblepropensity)/sqrt(length(highvaluegamblepropensity))))
valueSds<-c((sd(lowvaluegamblepropensity)),(sd(midvaluegamblepropensity)),(sd(highvaluegamblepropensity)))
arrows(pg7,valueMeans-valueSems,pg7,valueMeans+valueSems,lwd=2,angle=90,code=3)




#poster graphs
#Bargraphs of 
#1) gamble propensity infrequent vs frequent
#POSTER8

#subslope1DF is low gamblers, 2DF is high gamblers
compMeans<-c(mean(subslope1DF$gambleMeans),mean(subslope2DF$gambleMeans))
pg8<-barplot(compMeans,names.arg = c("Infrequent gamblers","Frequent gamblers"),ylim=c(0,80),ylab="Gamble Propensity",col=bpCol)
compSems<-c((sd(subslope1DF$gambleMeans)/sqrt(length(subslope1DF$gambleMeans))),(sd(subslope2DF$gambleMeans)/sqrt(length(subslope2DF$gambleMeans))))
compSds<-c((sd(subslope1DF$gambleMeans)),(sd(subslope2DF$gambleMeans)))
arrows(pg8,compMeans-compSds,pg8,compMeans+compSds,lwd=2,angle=90,code=3)


#2)Gamble slopes of infrequent vs frequent
compMeans<-c(mean(subslope1DF$gambleSlopes),mean(subslope2DF$gambleSlopes))
pg9<-barplot(compMeans,names.arg = c("Infrequent gamblers","Frequent gamblers"),ylab="Gamble Slopes",col=bpCol,ylim=c(0,.15))
compSems<-c((sd(subslope1DF$gambleSlopes)/sqrt(length(subslope1DF$gambleSlopes))),(sd(subslope2DF$gambleSlopes)/sqrt(length(subslope2DF$gambleSlopes))))
compSds<-c((sd(subslope1DF$gambleSlopes)),(sd(subslope2DF$gambleSlopes)))
arrows(pg9,compMeans-compSems,pg9,compMeans+compSems,lwd=2,angle=90,code=3)
t.test(subslope1DF$gambleSlopes,subslope2DF$gambleSlopes)

#3)RT slopes of infrequent vs frequent
compMeans<-c(mean(subslope1DF$rtSlopes),mean(subslope2DF$rtSlopes))
pg10<-barplot(compMeans,names.arg = c("Infrequent gamblers","Frequent gamblers"),ylab="RT Slopes",col=bpCol)
compSems<-c((sd(subslope1DF$rtSlopes)/sqrt(length(subslope1DF$rtSlopes))),(sd(subslope2DF$rtSlopes)/sqrt(length(subslope2DF$rtSlopes))))
compSds<-c((sd(subslope1DF$rtSlopes)),(sd(subslope2DF$rtSlopes)))
arrows(pg10,compMeans-compSems,pg10,compMeans+compSems,lwd=2,angle=90,code=3)
t.test(subslope1DF$rtSlopes,subslope2DF$rtSlopes)



dfb<-data.frame(c(successfulCatchM$gambleSlopes,failCatchM$gambleSlopes),Txtlabels,sds)
colnames(dfb)[1]<-"gambleSlope"
dfb2<-dfb %>% 
  group_by(Txtlabels) %>% 
  summarise(mean=mean(gambleSlopes))
#Error bars
#Find error bar function for vectors
#Remove black bars
#Color appropriately and use consistent colors

barplot(c(mean(SubslopeDF$gambleSlopes),mean(slopeDF$gambleSlopes)),ylim=c(-.02,.06),names.arg = c("subgroup","All Participants"),bty='n')
barplot(c(mean(successfulCatchM$gambleSlopes),mean(failCatchM$gambleSlopes)),ylim=c(-.1,.15),names.arg = c("SucessfulCatch","FailCatch"),ylabl="MeanGambleSlopes")
p=ggplot(data=dfb,aes(x=as.factor(Txtlabels),y=gambleSlope))+geom_bar(stat="summary",fun.y="mean",position="dodge")+stat_summary(geom="errorbar",fun.data=mean_se)
p
#slopeDF analysis for indiv. subgroup

plot(SubslopeDF$rtSlopes~SubslopeDF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',main=paste('logical gamblers n=',toString(length(SubslopeDF$gambleSlopes))),xlim=c(-1,1),ylim=c(-90,50))
with(SubslopeDF, text(SubslopeDF$rtSlopes~SubslopeDF$gambleSlopes, labels = SubslopeDF$run,cex=.2), pos = 1)
summary(m1<-lm(SubslopeDF$rtSlopes~SubslopeDF$gambleSlopes))
abline(m1)
cor(SubslopeDF$rtSlopes~SubslopeDF$gambleSlopes)


plot(SubslopeDF$gambleSlopes~SubslopeDF$rtSlopes,xlab='rtSlopes',ylab='gambleSlopes',main='rt rampers',ylim=c(-1,1),xlim=c(-90,50))
with(SubslopeDF, text(SubslopeDF$gambleSlopes~SubslopeDF$rtSlopes, labels = SubslopeDF$run,cex=.8), pos = 2)
summary(m1<-lm(SubslopeDF$gambleSlopes~SubslopeDF$rtSlopes))
abline(m1)




#People who gambled a lot
highGamblers<-filter(dBehavioral,percentageGambled>60)$uniqueid
lowGamblers<-filter(dBehavioral,percentageGambled<30)$uniqueid

highInterest<-filter(dsurvey,interest>7)$uniqueid

male<-filter(dsurvey,gender==1)$uniqueid

#This plots the two subgroups histograms against one another
par(new=FALSE)
hist(a$OddsGamblingScore,breaks=30,col="red",xlim=c(-10,40),main="propensity to gamble on high value vs. low value",xlab="Percentage of high value trials gambled on - percentage of low value trials gambled on")
abline(v=median(a$OddsGamblingScore),col="red",lwd=2)
par(new=TRUE)
hist(b$OddsGamblingScore,breaks=30,col="blue",xlim=c(-10,40),main="propensity to gamble on high value vs. low value",xlab="Percentage of high value trials gambled on - percentage of low value trials gambled on")
abline(v=median(b$OddsGamblingScore),col="blue",lwd=2)
t.test((1/rtsub1$outcomeRT),(1/rtsub2$outcomeRT))

d5prime<-dgamble[dgamble$uniqueid %in% failCatchId,]

hist(acf1,breaks=50,main="ACF constants for each participant, lag of 1",xlim=c(-1,1))
hist(acf2,breaks=50,main="ACF constants for each participant, lag of 2",xlim=c(-1,1))
hist(acf3,breaks=50,main="ACF constants for each participant, lag of 3",xlim=c(-1,1))
hist(acf4,breaks=50,main="ACF constants for each participant, lag of 4",xlim=c(-1,1))
hist(acf5,breaks=50,main="ACF constants for each participant, lag of 5",xlim=c(-1,1))

#sfngprt
gamblePlot(d,orig=T,eb=F,ylimit=c(35,50),title='All participants',line=T)
rtPlot(d,type='raw',eb='stderr',title='All participants',ylimit=c(600,900),line=F)

gamblePlot(d5,orig=T,eb=F,ylimit=c(40,55),title='catchFail',line=T)
rtPlot(d5,type='raw',eb='stderr',title='catchFail',ylimit=c(600,900),line=F)

gamblePlot(d5prime,orig=T,eb=F,ylimit=c(35,50),title='catchSuccess',line=T)
rtPlot(d5prime,type='raw',eb='stderr',title='catchSuccess',ylimit=c(600,900),line=F)


#sfnValueChange
#All participants
compMeans<-c(oddsScoreMean(d,time='early'),oddsScoreMean(d,time='mid'),oddsScoreMean(d,time='late'))
compSds<-c(oddsScoreEb(d,type='sem'),oddsScoreEb(d,type='sem'),oddsScoreEb(d,type='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Value Sensitivity; All participants")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#FailCatch (d5)
compMeans<-c(oddsScoreMean(d5,time='early'),oddsScoreMean(d5,time='mid'),oddsScoreMean(d5,time='late'))
compSds<-c(oddsScoreEb(d5,type='sem'),oddsScoreEb(d5,type='sem'),oddsScoreEb(d5,type='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Value Sensitivity;FailCatch; n=87 participants")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#SuccessCatch (d5prime)
compMeans<-c(oddsScoreMean(d5prime,time='early'),oddsScoreMean(d5prime,time='mid'),oddsScoreMean(d5prime,time='late'))
compSds<-c(oddsScoreEb(d5prime,type='sem'),oddsScoreEb(d5prime,type='sem'),oddsScoreEb(d5prime,type='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Value Sensitivity; catchSuccess; n=53 participants")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#sfnGambleSlopes
#2)Gamble slopes of infrequent vs frequent
compMeans<-c(mean(slopeDF$gambleSlopes),mean(subslope1DF$gambleSlopes),mean(subslope2DF$gambleSlopes))
pg10<-barplot(compMeans,names.arg = c("All participants","catchFail","successCatch"),ylab="Gamble Slopes",ylim=c(-.15,.15),main="Gamble Slopes of subgroups")
compSems<-c(std.error(slopeDF$gambleSlopes),std.error(subslope1DF$gambleSlopes),std.error(subslope2DF$gambleSlopes))
compSds<-c((sd(subslope1DF$gambleSlopes)),(sd(subslope2DF$gambleSlopes)))
arrows(pg10,compMeans-compSems,pg10,compMeans+compSems,lwd=2,angle=90,code=3)

#3)RT slopes of infrequent vs frequent
compMeans<-c(mean(slopeDF$rtSlopes),mean(subslope1DF$rtSlopes),mean(subslope2DF$rtSlopes))
pg10<-barplot(compMeans,names.arg = c("All participants","catchFail","successCatch"),ylab="RT Slopes",ylim=c(-25,0),main="RT Slopes of subgroups")
compSems<-c(std.error(slopeDF$rtSlopes),std.error(subslope1DF$rtSlopes),std.error(subslope2DF$rtSlopes))
arrows(pg10,compMeans-compSems,pg10,compMeans+compSems,lwd=2,angle=90,code=3)

