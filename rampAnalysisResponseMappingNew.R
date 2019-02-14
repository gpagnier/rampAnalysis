###Ramp analysis; instrumental responseMapping - no preview, 1.7 sec forced choice
###Rewritten/organized 9.4.2018
###Updated 1.15.189

#Things to do

#Poisson distribution 

##Loading packages
#install.packages('mosaic')
#install.packages('plotrix')
#install.packages('plotrix', dependencies=TRUE, repos='http://cran.rstudio.com/')

require(mosaic)
require(plotrix)
require(VennDiagram)
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/reg_fns.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/gamblePlotFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/gambleRtPlotFun.R")

source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/ignorePlotFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/ignoreRtPlotFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/oddsScoreMeanFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/oddsScoreEbFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/totalPlotFun.R")
source("C:/Users/Guillaume/Documents/GitHub/rampAnalysis/rtPlotFun.R")

source("/Users/Guillaume/Documents/GitHub/rampAnalysis/correctRThist.R")

##Loading data
#d0<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")
d0<-read.csv(file="C:/Users/gpagn/Documents/GitHub/rampAnalysis/50rampRMNew",sep=",")
d0<-read.csv(file="C:/Users/Guillaume/Documents/GitHub/rampAnalysis/75rampRM.csv",sep=",")

#d0<-read.csv(file="//files.brown.edu/Home/gpagnier/Documents/GitHub/rampAnalysis/Totalrampv03.csv",sep=",")


#d0<-read.csv(file.choose())
#Cleaning data for totalrampv3
#d0<-d0[9040:length(d0$viewTime),]
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
#Export bonusAmount csv if you want
#write.csv(bonusAmounts,'bonusAmounts.csv')
#If you want to see survey results
dsurvey<-d0 %>% 
  group_by(uniqueid) %>% 
  summarise(engagement=unique(engagement)[2],
            difficulty=unique(difficulty)[2],
            length=unique(length)[2],
            design=unique(design)[2],
            gender=unique(gender)[2],
            interest=unique(interest)[2],
            understand=unique(understand)[2])
#Removing rows that have empty column values (specifically if gender is empty)
dsurvey<-dsurvey[!(is.na(dsurvey$gender)|dsurvey$gender==""),]

#Engagment: how engaging, 1 is not, 10 is very engaging
hist(as.integer(dsurvey$engagement),breaks=50,main="Length: 10 is very engaging")
#difficult: 1 is easy, 10 is hard
hist(as.integer(dsurvey$difficulty),breaks=50,main="Length: 10 is too hard")
#length: 1 is could be longer, 10 is much too long
hist(as.integer(dsurvey$length),breaks=50,main="Length: 10 is too long")
#design: how engaging, 1 is unplayable, 4 is worked fine
table(as.integer(dsurvey$design))
#Gender: 1 male, 2 is female 4: decline to answer
table(as.integer(dsurvey$gender))
#interest: 1 is no way; 10 is absolutely yes
hist(as.integer(dsurvey$interest),breaks=50,main="interest; 10 is very interesting")
#understand
hist(as.integer(dsurvey$understand),breaks=50,main="interest; 10 is very easy to understand")


#Warning! CSV needs to be in exact column order:
#"trialid" #"expTime" "gambleDelay" "ignoreRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"

#Need to replace uniqueid with numbers and clean out columns that aren't useful
d<-d0[,c("Trialid","gambleDelay","ignoreRT","gambleRT","outcomeRT","response","standardGamble","trialNumber","uniqueid","trialType")]
d<-subset(d,!grepl("debug",as.character(d$uniqueid)))
d<-subset(d,d$response!="")
d$engagement<-NULL
d$difficulty<-NULL
d$length<-NULL
d$design<-NULL
d$gender<-NULL
d$interest<-NULL


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
seed=301
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
#"trialid" #"expTime" "gambleDelay" "ignoreRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"

d[d==""] <- NA
d$gambleDelay<-d$gambleDelay/1000
d$binsTime=0;

#Clearing pictures
graphics.off()

##Some basic behavioral metrics and filtering participants and adding gamble delay
#Intitial filtering of participants
removeIds=c()
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}
unique(d$uniqueid)

#Where did gambles interrupt
hist(d$gambleDelay,breaks=50,xlim=c(0,8),main="When did gambles interrupt the progress bar?",xlab="Seconds into trial gamble appeared",col='black',ylab="Total number of trials")


#How many points do you want on the gambleDelay?
d$binsTme=NULL
bins=4

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
a1tail<-.9
a2head<-.90001
a2tail<-1.55
a3head<-1.5500001
a3tail<-2.2
a4head<-2.200001
a4tail<-2.85
a5head<-2.85000001
a5tail<-3.5


#For 4 bins #Use this for RM NEW
a1head<-.25
a1tail<-1.25
a2head<-1.25001
a2tail<-2
a3head<-2.00001
a3tail<-2.75
a4head<-2.75001
a4tail<-3.5


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
  else if (d[row,'gambleDelay']>=a4head&d[row,'gambleDelay']<=a4tail)
  {return(mean(c(a4head,a4tail)))}
  #else if (d[row,'gambleDelay']>=a5head&d[row,'gambleDelay']<=a5tail)
  #{return(mean(c(a5head,a5tail)))}
  #else if (d[row,'gambleDelay']>=a6head&d[row,'gambleDelay']<=a6tail)
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
  } else if (d[row,'standardGamble']==4|d[row,'standardGamble']==5){
    return("mid")
  } else if (d[row,'standardGamble']==8|d[row,'standardGamble']==9){
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
  if(d[row,'Trialid']==21|d[row,'Trialid']==22|d[row,'Trialid']==24|d[row,'Trialid']==25|d[row,'Trialid']==28|d[row,'Trialid']==29){
    return("lowp")
  } else if (d[row,'Trialid']==1|d[row,'Trialid']==2|d[row,'Trialid']==4|d[row,'Trialid']==5|d[row,'Trialid']==8|d[row,'Trialid']==9){
    return("midp")
  } else if (d[row,'Trialid']==31|d[row,'Trialid']==32|d[row,'Trialid']==34|d[row,'Trialid']==35|d[row,'Trialid']==38|d[row,'Trialid']==39){
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

#dbackup<-d
#d<-dbackup

#Adding prediction errors:
#Adding prediction errors as possible variable
d2=d[0,]
#rpe 1 2 and 3
d$rpe1=NULL
d$rpe2=NULL
d$rpe3=NULL
d$gamblePrevTrial=NULL
d$gamblePrevTrial2=NULL
d$gamblePrevTrial3=NULL

#RT z score
#Should I do z scores on speed or raw RT?
d$ignoreRTz<-NULL
d$outcomeRTz<-NULL
acfdf<-matrix(NA, ncol = 133, nrow =0)
#Length of totally unfiltered participants (minus debug)
Participants<-unique(d$uniqueid)
length(Participants)
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  print(nrow(dsub))
  #if(length(dsub$gambled)==133){
  #  acfdf<-rbind(acfdf,dsub$gambled)
  #}
}

#Adding vector to filter out fast RTers
fastRTers<-NULL
fewTrials<-NULL
noIgnore<-NULL
noSuccess<-NULL
noGamble<-NULL
failures<-NULL
#Adding RPE as a factor AND normalized RT z score
#rpe1 is the sure thing of trial t- standard gamble of t-1
#rpe2 is the sure thing of t - the average of whatever was chosen in t-1
#rpe3 is the sure thing of t - whatever was chosen in t-1 but the highest gamble option instead of average

dbackup<-d
#d<-dbackup

for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  kickOut=0
  if(nrow(dsub)<50){
    fewTrials<-c(fewTrials,unique(dsub$uniqueid))
    next()
  }

  if(length(setdiff(dsub$ignoreRT,0))<10){
    noIgnore<-c(noIgnore,unique(dsub$uniqueid))
    kickOut=1
  }
  
  if(sum(dsub$response=='success')<10){
    noSuccess<-c(noSuccess,unique(dsub$uniqueid))
    kickOut=1
  }
  if(sum(dsub$response=='gamble')<10){
    noGamble<-c(noGamble,unique(dsub$uniqueid))
    kickOut=1
  }
  if(kickOut==1){
    next()
  }
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
  #gamblePrevTrial2
  for(row in 3:length(dsub$Trialid)){
    if(dsub[(row-2),"gambled"]){
      dsub[row,"gamblePrevTrial2"]=1
    } else{
      dsub[row,"gamblePrevTrial2"]=0
    }
  }
  #gamblePrevTrial3
  for(row in 4:length(dsub$Trialid)){
    if(dsub[(row-3),"gambled"]){
      dsub[row,"gamblePrevTrial3"]=1
    } else{
      dsub[row,"gamblePrevTrial3"]=0
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
  meanGambleZ=mean(dsub$ignoreRT[dsub$ignoreRT!=0])
  meanOutcomeZ=mean(dsub$outcomeRT[dsub$outcomeRT!=0])
  sdGambleZ=sd(dsub$ignoreRT[dsub$ignoreRT!=0])
  sdOutcomeZ=sd(dsub$outcomeRT[dsub$outcomeRT!=0])
  
  for(row in 1:length(dsub$Trialid)){
    if(dsub[row,"ignoreRT"]!=0){
      dsub[row,"ignoreRTz"]=(dsub[row,"ignoreRT"]-meanGambleZ)/sdGambleZ
    } else if(dsub[row,"ignoreRT"]==0){
      dsub[row,"ignoreRTz"]=0
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
d$NignoreRT=0
d$NoutcomeRT=0


#Dividing ignoreRT by (1/RT) on new column
for(i in 1:length(d$response)){
  if(d[i,"ignoreRT"]!=0){
    d[i,"NignoreRT"]=(1/d[i,"ignoreRT"])
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

###Behavioral analyses
##Reaction time
#Whenever they gambled
ignoreRTs<-d$ignoreRT[d$ignoreRT!=0]
gambleRTs<-d$gambleRT[d$gambleRT!=0]
hist(ignoreRTs,main="Aggregated ignore RTs",breaks=70,xlim=c(0,1600))
hist(gambleRTs,main="Aggregated gamble RTs",breaks=70,xlim=c(0,1600))

#Overlaying RT histograms of gamble and ignore RTs
hist(ignoreRTs,col=rgb(0,0,1,0.5), main='Reaction Times at gamble time', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(ignoreRTs),col="blue",lwd=2)
hist(gambleRTs,col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(gambleRTs),col="red",lwd=2)
legend(200,35,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))


#Whenever they claimed 'boring' reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
hist(outcomeRTs,main=c("Aggregated outcome RTs; number of responses:",length(outcomeRTs)),breaks=70)

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
dsurvey
#Removing participants who gambled too much/not enough
dgamble0<-filter(d,gambleDelay!=0,Trialid!=75|86)

dBehavioralTotal<-dgamble0 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            ignoredTrials=sum(ignoreRT!=0),
            successTrials=sum(response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'))
head(dBehavioralTotal)


#Overall preference for gambling
hist(dBehavioralTotal$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity(everyone) to gamble; n =",toString(sum(dBehavioralTotal$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

dlowg<-filter(dBehavioralTotal,percentageGambled<9)
noGamblers<-dlowg$uniqueid
dhighg<-filter(dBehavioralTotal,percentageGambled>95)
allGamblers<-dhighg$uniqueid
lowTrials<-filter(dBehavioralTotal,trials<50)$uniqueid
failures<-filter(dBehavioralTotal,failedTrials>(round(15/trials*100)))$uniqueid

removeIds<-c(noGamblers,allGamblers,lowTrials,fastRTers,failures)


#Removing any subjects from dataset, using unique ids in vector removeIds
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}


#Check for catch trials
#75 should gamble; 86 should success/failOutcome; 6 catch trials
#Check for catch trials
dcatch<-filter(d,Trialid==75|Trialid==86)[,c("Trialid","response","uniqueid","gambleDelay")]
dcatchfinish<-filter(dcatch,Trialid==86,response=="gamble")
#Differential analysis of failCatch
#Called what they SHOULD do
dcatchidFinish<-dcatch[dcatch$Trialid==86,] %>% 
  group_by(uniqueid) %>% 
  summarise(FailTrials=sum(response=="gamble"))
imp<-filter(dcatchidFinish,FailTrials>0)$uniqueid
dcatchgamble<-filter(dcatch,Trialid==75,response=="fail"|response=='success')
dcatchidGamble<-dcatch[dcatch$Trialid==75,] %>% 
  group_by(uniqueid) %>% 
  summarise(FailTrials=sum(response=="fail"|response=='success'|response=='earlyFail'))
ra<-filter(dcatchidGamble,FailTrials>0)$uniqueid

dcatchfail<-rbind(dcatchfinish,dcatchgamble)
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

d$failedTrials<-NULL
d2=d[0,]
#Adding d column of number of failed trials
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  if(is.element(unique(dsub$uniqueid),fail1)){
    dsub$failedTrials<-1
  }else if(is.element(unique(dsub$uniqueid),fail2)){
    dsub$failedTrials<-2
  }else if(is.element(unique(dsub$uniqueid),fail3)){
    dsub$failedTrials<-3
  }else if(is.element(unique(dsub$uniqueid),fail4)){
    dsub$failedTrials<-4
  }else if(is.element(unique(dsub$uniqueid),fail5)){
    dsub$failedTrials<-5
  }else if(is.element(unique(dsub$uniqueid),fail6)){
    dsub$failedTrials<-6
    }else{
    dsub$failedTrials<-0
    }
  d2<-rbind(d2,dsub)
}
d<-d2
    

#Now this is refined number of participants
nParticipants<- length(unique(d$uniqueid))
Participants<-unique(d$uniqueid)
nParticipants
Participants
#######################################################################################################
#Clearing pictures
graphics.off()

#Behavioral analyses
dgamble<-filter(d,gambleDelay!=0,oddsCond!="catch")

dBehavioral<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            ignoredTrials=sum(ignoreRT!=0),
            successTrials=sum(response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'))
dBehavioral
hist(dBehavioral$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall propensity to gamble; n =",toString(nrow(dgamble[dgamble$gambleDelay!=0,]))," possible trials;",nParticipants,"subj"),xlab="Percentage of time gambled",col='red')
boxplot(dBehavioral$percentageGambled,ylim=c(0,100),main=paste("Distribution of gambling percentages across everyone; mean G % = ",toString(mean(dBehavioral$percentageGambled))),ylab="Percentage gambled (per participant)")

#Adding propensity to gamble and propensity to press as possible predictors
d$basePercentageGambled<-NULL

d2=d[0,]
#Adding d column of number of failed trials
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  tempdb<-filter(dBehavioral,uniqueid==i)
  dsub$basePercentageGambled<-tempdb$percentageGambled
  d2<-rbind(d2,dsub)
}
d<-d2

#Remaking behavioral histograms
#Whenever they ignored -RTIgnore
ignoreRTs<-dgamble$ignoreRT[dgamble$ignoreRT!=0]
hist(ignoreRTs,main=paste("Aggregated ignore RTs; ",toString(sum(dBehavioral$ignoredTrials)),"trials ignored"),breaks=90,xlim=c(0,1700))

#Whenever they gambled -RTGamble
gambleRTs<-dgamble$gambleRT[dgamble$gambleRT!=0]
hist(gambleRTs,main=paste("Aggregated gamble RTs; ",toString(sum(dBehavioral$gambleCount)),"trials gambled"),breaks=110,xlim=c(0,1700))


#Whenever they  ignored and claimed guaranteed reward
outcomeRTs<-dgamble$outcomeRT[dgamble$outcomeRT!=0]
hist(outcomeRTs,main=paste("Aggregated outcome RTs; ",toString(length(outcomeRTs)),"trials accepted sure thing"),breaks=150,xlim=c(0,700))

#Histogram of RTs and t test
hist(ignoreRTs,col=rgb(0,0,1,0.5), main='Reaction Times at gamble time', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(ignoreRTs),col="blue",lwd=2)
hist(gambleRTs,col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(gambleRTs),col="red",lwd=2)
legend(200,15,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))



#Number of gambled trials per participant 
dTrials<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            ignoredTrials=sum(ignoreRT!=0),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            percentageIgnored=round(ignoredTrials/ntrials*100))
head(dTrials)
#dTrials
hist(dTrials$ntrials,breaks=50,xlim=c(0,140),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")

#Looking at trialType breakdown
dtrialType<-dgamble %>% 
  group_by(binsTime,trialType) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            ignoredTrials=sum(ignoreRT!=0),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            percentageIgnored=round(ignoredTrials/ntrials*100))
head(dtrialType)

#Statistics
#Resetting dgamble
dgamble<-filter(d,gambleDelay!=0,Trialid!=75,Trialid!=86)
#Logistic regression models to predict gambled
#Recoding
dgamble$contOdds<-recode(dgamble$oddsCond,lowp=1,midp=2,highp=3)
dgamble$contMag<-recode(dgamble$magCond,low=1,mid=2,high=3)

#Need to figure out which one to use
mlog<-glm(gambled~contOdds,
          data=dgamble,family="binomial");
summary(mlog)

mlog2<-glm(gambled~gambleDelay*contOdds+trialNumber+contMag+contMag:gambleDelay,
           data=dgamble,family="binomial");
summary(mlog2)

# d5gambleearly$contOdds<-recode(d$oddsCond,lowp=1,midp=2,highp=3)
# d5gamblemid$contOdds<-recode(d$oddsCond,lowp=1,midp=2,highp=3)
# d5gamblelate$contOdds<-recode(d$oddsCond,lowp=1,midp=2,highp=3)
# 
# dgambleearly$contOdds<-recode(dgambleearly$oddsCond,lowp=1,midp=2,highp=3)
# dgamblemid$contOdds<-recode(dgamblemid$oddsCond,lowp=1,midp=2,highp=3)
# dgamblelate$contOdds<-recode(dgamblelate$oddsCond,lowp=1,midp=2,highp=3)
# 
# 
# d5gambleearly<-filter(d5gamble,gambleDelay<2.5)
# d5gamblemid<-filter(d5gamble,gambleDelay>2.5&gambleDelay<5)
# d5gamblelate<-filter(d5gamble,gambleDelay>5)
# 
# dgambleearly<-filter(dgamble,gambleDelay<2.5)
# dgamblemid<-filter(dgamble,gambleDelay>2.5&gambleDelay<5)
# dgamblelate<-filter(dgamble,gambleDelay>5)

# d5success<-filter(d5,Trialid==75)
# gamblePlot(d5success,orig=T,title="shouldGamble")
# ignoreRtPlot(d5success,type='raw',eb='stderr',ylimit=c(400,1000),title="ShouldGamble")

library(lme4)
mlmerog<-glmer(gambled~scale(contOdds)+(scale(gambleDelay)+contOdds|uniqueid),
               data=dgamble,family="binomial");
summary(mlmerog)

mlmerog1<-glmer(gambled~scale(contOdds)+scale(gambleDelay)+
                    (scale(gambleDelay)+scale(contOdds)+1|uniqueid),
                data=dgamble,family="binomial");
summary(mlmerog1)
 
 mlmerog2<-glmer(gambled~scale(contOdds)+scale(gambleDelay)*scale(contOdds)+scale(basePercentageGambled):scale(gambleDelay)+
                  scale(gamblePrevTrial)+scale(contMag)+scale(contMag):scale(gambleDelay)+scale(failedTrials)+
                   scale(failedTrials):scale(gambleDelay)+as.factor(trialType)+scale(trialNumber)+scale(trialNumber):scale(gambleDelay)+
                   
                   (scale(gambleDelay)+scale(contMag)+scale(trialNumber)+scale(trialNumber):scale(gambleDelay)+
                      scale(contOdds)+1|uniqueid),
                data=dgamble,family="binomial");
 summary(mlmerog2)
##Total data
c("Number of trials that they gambled on: ",length(dgamble$response[dgamble$response=='gamble']))
c("Number of trials that they had the chance to gamble on: ",length(dgamble$response))

#By GambleDelay
d2<-filter(dgamble) %>% 
  group_by(binsTime) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            ignoredTrials=sum(ignoreRT!=0),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            percentageIgnored=round(ignoredTrials/ntrials*100))

d2$seconds<-d2$binsTime
d2=filter(d2,binsTime!=0)
d2


#Interesting plot of gambleDelay vs propensity to gamble. 
#Andrew1
plot(d2$seconds,d2$percentageGambled,xlim = c(0,4),ylim = c(0,100),
     main=paste("Total group data; Gamble propensity; n =",toString(sum(d2$ntrials)),
                "trials;",toString(length(Participants)),"participants"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
abline(lm(d2$percentageGambled~d2$seconds))


gamblePlot(d,orig=T,eb='sem',ylimit=c(35,55))
gamblePlot(d,orig=F,eb='sem',ylimit=c(40,50),title='all data')
gamblePlot(d,orig=T,eb='sem',ylimit=c(30,50),trialType="gambleRight")
gamblePlot(d,orig=T,eb='sem',ylimit=c(35,55),trialType="gambleLeft")

ignorePlot(d,orig=T,eb='sem',ylimit=c(40,60))

gamblePlot(d,orig=F,eb='sem',ylimit=c(30,70))
ignorePlot(d,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(d,eb='stderr',title="gambleLeft",ylim=c(800,1200),trialType='gambleLeft')
gambleRtPlot(d,eb='stderr',title="gambleRight",ylim=c(800,1200),trialType='gambleRight')

ignoreRtPlot(d,eb='stderr',title="gambleRight",ylim=c(900,1100),trialType='gambleRight')
totalRTPlot(d,line=T,title="all data",ylimit = c(900,1100))

totalRTPlot(dlow,line=T,title="low mag")
totalRTPlot(dmid,line=T,title="mid mag")
totalRTPlot(dhigh,line=T,title="high mag")
totalRTPlot(dlowp,line=T,title="low value")
totalRTPlot(dmidp,line=T,title="mid value")
totalRTPlot(dhighp,line=T,title="high value")




#Plotting gamble RTs with std.error All data
dRT<-filter(dgamble,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(setdiff(gambleRT,0)),
            sdRT=std.error(setdiff(gambleRT,0)))
dRT$seconds<-dRT$binsTime
summary(m2RT<-glm(dRT$medianRT~dRT$binsTime))

#This is raw RT
plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=c(600,1200),main=paste("Group data; total data; median gambling T with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dRT$seconds)){
  arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]+(as.numeric(dRT[i,3]))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]-(as.numeric(dRT[i,3]))),length=0.05, angle=90, code=3)
}

#Plotting ignore RTs with std.error All data
dRT<-filter(dgamble,ignoreRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(setdiff(ignoreRT,0)),
            sdRT=std.error(setdiff(ignoreRT,0)))
dRT$seconds<-dRT$binsTime
summary(m2RT<-glm(dRT$medianRT~dRT$binsTime))

#This is raw RT
plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=c(0,1200),main=paste("Group data; total data; median ignore RT with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dRT$seconds)){
  arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]+(as.numeric(dRT[i,3]))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]-(as.numeric(dRT[i,3]))),length=0.05, angle=90, code=3)
}





gambleRtPlot(d,eb='stderr',title="all data")
ignoreRtPlot(d,eb='stderr',title="all data")

d5<-dgamble[dgamble$uniqueid %in% failCatchId,]
d5prime<-dgamble[dgamble$uniqueid %in% catchSuccessId,]


#Histograms to check for spacing out
correctRThist(d,interruption="early",title="All corrrect responses, early mid and late")
correctRThist(d,interruption="mid",addknob=T)
correctRThist(d,interruption="late",addknob=T)
legend(250,10,cex=.7, bty = "n",legend=c("early","mid","late"),col=c("blue","red","green"),title="",pch=15)

correctRThist(d,time="early",title="All corrrect responses, early, mid, and late in session")
correctRThist(d,time="mid",addknob=T)
correctRThist(d,time="late",addknob=T)

legend(250,10,cex=.7, bty = "n",legend=c("early","mid","late"),col=c("blue","red","green"),title="",pch=15)



hist(dmid$ignoreRT[dmid$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time mid mag only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(ignoreRTs),col="blue",lwd=2)
hist(dmid$gambleRT[dmid$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(gambleRTs),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)


dbackup<-d
########################################################################################################################################
##Breaking down by 6 sub conditions - mag/odds

#How does mag affect gamble propensity
#Low mag =Guaranteed amount = $1 or $2
#Mid mag =Guaranteed amount = $4 or $5
#High mag =Guaranteed amount = $8 or $9

#Low mag
dlow<-filter(dgamble,standardGamble==1|standardGamble==2)
c("Number of trials that they ignored: ",length(dlow$ignoreRT[dlow$ignoreRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dlow$gambleDelay))
#By uniqueId
dBehavioralLow<-dlow %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioralLow
#How much did each participant choose to gamble
hist(dBehavioralLow$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low-mag gambles; n =",toString(sum(dBehavioralLow$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')

gamblePlot(dlow,orig=F,eb='sem',ylimit=c(0,100))
gamblePlot(dlow,orig=F,eb='sem',ylimit=c(30,60))
gamblePlot(dlow,orig=T,eb='sem',ylimit=c(40,60))

#ignorePlot(dlow,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(dlow,eb='stderr',title="low mag trials only",ylim=c(800,1200))
ignoreRtPlot(dlow,eb='stderr',title="low mag trials only")
totalRTPlot(dlow,line=T,ylimit=c(700,1200),title="low mag trials only")

#Histogram of RTs and t test
hist(dlow$ignoreRT[dlow$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time low mag only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(dlow$ignoreRT[dlow$ignoreRT!=0]),col="blue",lwd=2)
hist(dlow$gambleRT[dlow$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(dlow$gambleRT[dlow$gambleRT!=0]),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))

dtrialType<-dgamble %>% 
  group_by(binsTime,trialType) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            ignoredTrials=sum(ignoreRT!=0),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            percentageIgnored=round(ignoredTrials/ntrials*100))
head(dtrialType)

##Mid mag
dmid<-filter(dgamble,standardGamble==4|standardGamble==5)
c("Number of trials that they ignored: ",length(dmid$ignoreRT[dmid$ignoreRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dmid$gambleDelay))
#By uniqueId
dBehavioralMid<-dmid %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

#How much did each participant choose to gamble
hist(dBehavioralMid$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),col='red',main=paste("Propensity to gamble on mid-mag gambles; n =",toString(sum(dBehavioralMid$trials)),"possible trials;",toString(length(dBehavioralMid$uniqueid)),"participants"),xlab="Percentage of time gambled")
gamblePlot(dmid,orig=F,eb='sem',ylimit=c(40,60))
gamblePlot(dmid,orig=T,eb='sem',ylimit=c(40,60))

#ignorePlot(dmid,orig=T,eb='sem',ylimit=c(0,100))
#gambleRtPlot(dmid,eb='stderr',title="midMag",ylimit=c(800,1050))
#ignoreRtPlot(dmid,eb='stderr',title="all data")
totalRTPlot(dmid,line=T,ylimit=c(800,1200),title="mid mag trials only")

#Histogram of RTs and t test
hist(dmid$ignoreRT[dmid$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time mid mag only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(dmid$ignoreRT[dmid$ignoreRT!=0]),col="blue",lwd=2)
hist(dmid$gambleRT[dmid$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(dmid$gambleRT[dmid$gambleRT!=0]),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))


##High mag
dhigh<-filter(dgamble,standardGamble==8|standardGamble==9)
c("Number of trials that they ignored: ",length(dhigh$ignoreRT[dhigh$ignoreRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dhigh$gambleDelay))
#By uniqueId
dBehavioralHigh<-dhigh %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
hist(dBehavioralHigh$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),col='red',main=paste("Propensity to gamble on high mag gambles; n =",toString(sum(dBehavioralHigh$trials))," possible trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

gamblePlot(dhigh,orig=T,eb='sem',ylimit=c(40,60))

gamblePlot(dhigh,orig=F,eb='sem',ylimit=c(40,60))
#ignorePlot(dhigh,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(dhigh,eb='stderr',title="highMag")
#ignoreRtPlot(dhigh,eb='stderr',title="all data")
totalRTPlot(dhigh,line=T,ylimit=c(800,1200),title="high mag trials only")

#Histogram of RTs and t test
hist(dhigh$ignoreRT[dhigh$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time high mag only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(ignoreRTs),col="blue",lwd=2)
hist(dhigh$gambleRT[dhigh$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(gambleRTs),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))










#How does odds(payout) affect gamble propensity
#Low odds = 1.5 x standard gamble
#Mid odds = 2 x standard gamble
#High odds = 3 x standard gamble



##Low odds
dlowp<-filter(dgamble,Trialid==21|Trialid==22|Trialid==24|Trialid==25|Trialid==28|Trialid==29)
c("Number of trials that they gambled on: ",length(dlow$ignoreRT[dlowp$ignoreRT!=0]))
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

gamblePlot(dlowp,orig=T,eb='sem',ylimit=c(30,50))
#ignorePlot(dlowp,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(dlowp,eb='stderr',title="all data")
#ignoreRtPlot(dlowp,eb='stderr',title="all data")
totalRTPlot(dlowp,line=T,ylimit=c(800,1200),title="low value trials only")

#Histogram of RTs and t test
hist(dlowp$ignoreRT[dlowp$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time low value only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(dlowp$ignoreRT[dlowp$ignoreRT!=0]),col="blue",lwd=2)
hist(dlowp$gambleRT[dlowp$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(dlowp$gambleRT[dlowp$gambleRT!=0]),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/dlowp$ignoreRT[dlowp$ignoreRT!=0]),(1/dlowp$gambleRT[dlowp$gambleRT!=0]))


##Mid odds
dmidp<-filter(dgamble,Trialid==1|Trialid==2|Trialid==4|Trialid==5|Trialid==8|Trialid==9)
c("Number of trials that they gambled on: ",length(dmidp$ignoreRT[dmidp$ignoreRT!=0]))
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
gamblePlot(dmidp,orig=T,eb='sem',ylimit=c(40,60))
#ignorePlot(dmidp,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(dmidp,eb='stderr',title="Mid Value")
#ignoreRtPlot(dmidp,eb='stderr',title="all data")
totalRTPlot(dmidp,line=T,ylimit=c(800,1200),title="mid value trials only")

#Histogram of RTs and t test
hist(dmidp$ignoreRT[dmidp$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time mid value only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(dmidp$ignoreRT[dmidp$ignoreRT!=0]),col="blue",lwd=2)
hist(dmidp$gambleRT[dmidp$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(dmidp$gambleRT[dmidp$gambleRT!=0]),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))



##High odds
dhighp<-filter(dgamble,Trialid==31|Trialid==32|Trialid==34|Trialid==35|Trialid==38|Trialid==39)
c("Number of trials that they gambled on: ",length(dhighp$ignoreRT[dhighp$ignoreRT!=0]))
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

gamblePlot(dhighp,orig=T,eb='sem',ylimit=c(50,80))

gamblePlot(dhighp,orig=F,eb='sem',ylimit=c(60,80))
#ignorePlot(dhighp,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(dhighp,eb='stderr',title="high value",ylimit=c(800,1000))
#ignoreRtPlot(dhighp,eb='stderr',title="all data")
totalRTPlot(dhighp,line=T,ylimit=c(800,1100),title="high value trials only")

#Histogram of RTs and t test
hist(dhighp$ignoreRT[dhighp$ignoreRT!=0],col=rgb(0,0,1,0.5), main='Reaction Times at gamble time high value only', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1500))
abline(v=median(ignoreRTs),col="blue",lwd=2)
hist(dhighp$gambleRT[dhighp$gambleRT!=0],col=rgb(1,0,0,0.5), add=T,breaks=70)
abline(v=median(gambleRTs),col="red",lwd=2)
legend(200,10,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("blue","red"),title="",pch=15)
t.test((1/ignoreRTs),(1/gambleRTs))


#####################################################################################
#RPE
#Change the 3 here to whatever you want to do and just rerun it; it's a lot simpler
drpe<-filter(dgamble,dgamble$rpe3>0)
c("Number of trials that they gambled on: ",length(drpe$ignoreRT[drpe$ignoreRT!=0]))
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
gamblePlot(drpe,orig=F,eb='sem',ylimit=c(0,100))
ignorePlot(drpe,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(drpe,eb='stderr',title="all data")
ignoreRtPlot(drpe,eb='stderr',title="all data")
totalRTPlot(drpe,line=T,ylimit=c(400,800),title="all data")


#####################################################################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################
#####################################################################################################################################################################################################################################################################################################################


#############################################################################################
#Breaking down by participant

####The following is to just get one participant's data
#INDIVIDUAL
p=916

plotZscore<-FALSE

dsub<-d[d$uniqueid==p,]
gamblePlot(dsub,orig=F,eb='sem',ylimit=c(50,90))
ignorePlot(dsub,orig=T,eb='sem',ylimit=c(0,100))
gambleRtPlot(dsub,eb='stderr',title="all data")
ignoreRtPlot(dsub,eb='stderr',title="all data")
totalRTPlot(dsub,line=F,ylimit=c(400,1400),title="participant:916")

dsubignored<-filter(dsub,ignoreRT!=0,Trialid!=75|86)
d3<-dsub %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            percentageGambled=round(gambleCount/trials*100))
d3$seconds<-d3$binsTime

par(ps = 12, cex = 1, cex.main = 1)
plot(d3$seconds,d3$percentageGambled,xlim = c(1,7),ylim = c(0,100),
     main=paste("Individual participant data (all trials); n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0),family="binomial")
summary(mlogtemp)


#One participant ignore RT
d3RT<-filter(dsub,ignoreRT!=0,gambleDelay!=0,Trialid!=75|86) %>% 
  group_by(binsTime) %>% 
  summarise(meanRT=mean(ignoreRT),
            sdRT=std.error(ignoreRT),
            medianRtz=median(ignoreRTz),
            sdRtz=std.error(ignoreRTz))
d3RT$seconds<-d3RT$binsTime

plot(d3RT$seconds,d3RT$meanRT,xlim = c(0,8),ylim=c(200,1500),main=paste("All gambles; Ignore RT with stderr; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
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

msubRT<-lm(dsubignored$ignoreRT~dsubignored$gambleDelay)
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
plotRT=F
plotGD=F
#Add in knobs for different sub categories (though this number is very small....)

#Participants is default (all participants)
#If you want to run sub groups, go to bottom of script
for(i in Participants){
  print(i)
  dsub<-d[d$uniqueid==i,]
  dsubgamble<-filter(dsub,gambleDelay!=0,Trialid!=75|86)
  dsubgambled<-filter(dsub,gambleDelay!=0,Trialid!=75|86,gambleRT!=0)
  dsubignored<-filter(dsub,ignoreRT!=0,gambleDelay!=0,Trialid!=75,86)
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
    gamblePlot(dsub,line=T,title=paste("id#",toString(unique(dsub$uniqueid))," oddsScore: ",toString(oddsNtemp[1,1])))
    # plot(d4$seconds,d4$percentageGambled,xlim = c(0,7),ylim = c(0,100),
    #      main=paste("Individual participant data; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
    #      xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
  }
  #One participant RT
  d4RT<-filter(dsub,ignoreRT!=0) %>% 
    group_by(binsTime) %>% 
    summarise(medianRT=median(ignoreRT),
              sdRT=sd(ignoreRT))
  d4RT$seconds<-d4RT$binsTime
  
  if (plotRT){
    totalRTPlot(dsub,line=T,title=paste("Indiv graph; Participant# ",toString(unique(dsub$uniqueid))))
    # plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
    #      main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
    #      xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
  }
  #This is to check for gamble ramp
  mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0,Trialid!=75|86),family="binomial")
  #This is glm for RT by gamble Delay
  mtempRT<-lm(dsubgambled$gambleRT~dsubgambled$gambleDelay)
  
  #This checks to see if any participant is gamble ramping
  if(summary(mlogtemp)$coefficients[8]<.1 & summary(mlogtemp)$coefficients[2]>0){
    intN<-c(intN,i)
  }
  #Check to see if any participant has a RT that's suspiciously low..
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
oddsN<-oddsNtable[(length(oddsNtable)/2):(length(oddsNtable))]
#illogical Gamblers
reverseOddsN<-as.integer(a[1:(nrow(a)/4),]$Participant)

#Plot of gamble slopes vs. rtSlopes
#POSTER graph
#POSTER3

par(bty='n')
box(which="plot",lty='solid')
plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='Individual Gamble slopes (% change/second)',ylab='Individual RT Slopes (ms/second)',pch=16,cex=0.8,main=paste('Gamble slopes vs. RT slopes; n=',nrow(slopeDF)),xlim=c(-.7,.7),ylim=c(-80,50),bty='7')
abline(v=0,col="black")
abline(h=0,col="black")
with(slopeDF, text(slopeDF$rtSlopes~slopeDF$gambleSlopes, labels = slopeDF$run,cex=.8), pos = 2)
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

#plot of gamble slopes histogram
#POSTER graph
#POSTER4
par(bty="7")
hist(slopeDF$gambleSlopes,xlab='Individual Gamble slopes (% change/second)',ylab='Frequency',pch=16,cex=0.8,main='Individual Gamble slopes; n=88',
     breaks=50,xlim=c(-.7,.7),bty='7',col='black')
abline(v=0,col="black")
t.test(slopeDF$gambleSlopes)



#plot of RT slopes histogram
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


#New way which is better
  d5<-dgamble
 d5<-dgamble[dgamble$uniqueid %in% rtn,]
 d5<-dgamble[dgamble$uniqueid %in% Participants,]
 d5<-dgamble[dgamble$uniqueid %in% intN,]
 d5<-dgamble[dgamble$uniqueid %in% catchSuccessId,]
 d5<-dgamble[dgamble$uniqueid %in% male,]
 d5<-dgamble[dgamble$uniqueid %in% highGamblers,]
d5<-dgamble[dgamble$uniqueid %in% failCatchId,]
d5<-dgamble[dgamble$uniqueid %in% imp,]
d5<-dgamble[dgamble$uniqueid %in% ra,]
d5<-dgamble[dgamble$uniqueid %in% highInterest,]
d5<-dgamble[dgamble$uniqueid %in% reverseOddsN,]
d5<-dgamble[dgamble$uniqueid %in% noProx,]
d5<-dgamble[dgamble$uniqueid %in% dsub,]
d5<-dgamble[dgamble$uniqueid %in% g,]
d5<-dgamble[dgamble$uniqueid %in% oddsN,]


#This is if you want intersection of two groups
#d5<-dgamble[dgamble$uniqueid %in% intersect(highGamblers,noProx),]


#This is if you want filters
summaryMagFilter=T
summaryMagCond='low'
summaryOddsFilter=F
summaryOddsCond='lowp'

#This filters summary d5 by odds/mag if specified above via T/F
if(summaryMagFilter){
  d5<-filter(d5,magCond==summaryMagCond)
}
if(summaryOddsFilter){
  d5<-filter(d5,oddsCond==summaryOddsCond)
}

c("Number of trials that they gambled on:",length(d5$ignoreRT[d5$ignoreRT!=0]))
c("Number of trials that they had the chance to gamble on:",length(d5$gambleDelay))
#By uniqueId
d5Behavioral<-d5 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
#hist(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,20),xlim=c(-5,100),main=paste("Propensity to gamble; n =",toString(sum(d5Behavioral$trials)),"possible trials;"),xlab="Percentage of time gambled")

gamblePlot(d5,orig=T,eb='sem',ylimit=c(30,60),title='ValueSensitiveLowMag')
totalRTPlot(d5,line=T,title="CatchSuccess",ylimit=c(800,1000))

#EV sensitivity early mid and late within subject
compMeans<-c(oddsScoreMean(d5,time='early'),oddsScoreMean(d5,time='mid'),oddsScoreMean(d5,time='late'))
compSds<-c(oddsScoreEb(d5,type='sem',time='early'),oddsScoreEb(d5,type='sem',time='mid'),oddsScoreEb(d5,type='sem',time='late'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; Within-session all data")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#EV sensitivity early mid and late 
compMeans<-c(oddsScoreMean(d5,int='early'),oddsScoreMean(d5,int='mid'),oddsScoreMean(d5,int='late'))
compSds<-c(oddsScoreEb(d5,type='sem',int='early'),oddsScoreEb(d5,type='sem',int='mid'),oddsScoreEb(d5,type='sem',int='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; When interruption happened;catchSuccess")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

totalRTPlot(dlow,line=T,ylimit=c(800,1100),title="rtRampers only; n=6")





#RTs histogram
#hist(setdiff(d5$ignoreRT,0),xlim=c(0,1500),breaks=50,main=paste("RT; n =",toString(sum(d5Behavioral$gambleCount)),"gambled trials;"),xlab="Reaction time")
#By GambleDelay
d52<-d5 %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(ignoreRT,0)),
            sdRT=sd(ignoreRT))
d52$seconds<-d52$binsTime
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
 plot(d52$seconds,d52$percentageGambled,xlim = c(0,8),ylim = c(0,100),
      main=paste("Gamble propensity; n =",toString(sum(d52$gambleCount))," gambled trials;"),
      xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

plot(d52$seconds,d52$percentageGambled,xlim = c(0,8),ylim = c(40,60),
     main=paste("Gamble propensity; n =",toString(sum(d52$gambleCount)),"gambled trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5pRT<-filter(d5,ignoreRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(ignoreRT),
            sdRT=sd(ignoreRT))
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
  summary(glm(ignoreRT~gambleDelay,
              data=d6))

#Overlaying RT histograms of d over d5 of sure thing and gamble
#d5 should be defined above
rtall<-filter(dgamble,response=='success')
rtsub<-filter(d5gamble,response=='success')

#Making venn diagrams of overlapping participants
#setdiff(a,b)
#Things that are in A but not in B
# grid.newpage()
# #draw.single.venn(length(Participants),category="All participants",fill=c('cornflower blue'),alpha=.5)
grid.newpage()
draw.pairwise.venn(length(Participants),length(unique(d5$uniqueid)),length(unique(d5$uniqueid)),
                   fill=c(' cornflower blue',"red"),category=c("All participants","subgroup"),
                   alpha=c(.5,.5),cat.pos=c(0,0),cat.dist=rep(-0.025,2))

hist(rtall$outcomeRT,col=rgb(0,0,1,0.5), main='Reaction Times when accepting sure thing', xlab='Reaction Time (ms)',breaks=40,xlim=c(0,800),ylim=c(0,350))
abline(v=median(rtall$outcomeRT),col="blue",lwd=2)
hist(rtsub$outcomeRT,col=rgb(1,0,0,0.5), add=T,breaks=40)
abline(v=median(rtsub$outcomeRT),col="red",lwd=2)
legend(600,350,cex=.7, bty = "n",legend=c("Failed at least one catch trial","Passed all the catch trials"),col=c("blue","red"),title="",pch=15)

t.test((1/rtall$outcomeRT),(1/rtsub$outcomeRT))

rtall<-filter(d5,response=='gamble',ignoreRT!=0)
rtsub<-filter(d5prime,response=='gamble',ignoreRT!=0)

hist(rtall$ignoreRT,col=rgb(0,0,1,0.5), main='Reaction Times when gambling', xlab='Reaction Time (ms)',breaks=70,xlim=c(0,1300),ylim=c(0,200))
abline(v=median(rtall$ignoreRT),col="blue",lwd=2)
hist(rtsub$ignoreRT,col=rgb(1,0,0,0.5), add=T,breaks=50)
abline(v=median(rtsub$ignoreRT),col="red",lwd=2)
legend(900,200,cex=.7, bty = "n",legend=c("Failed at least one catch trial","Passed all the catch trials"),col=c("blue","red"),title="",pch=15)

t.test((1/rtall$ignoreRT),(1/rtsub$ignoreRT))



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
 ignoreRtPlot(d5prime,type='raw',eb='stderr',title="CatchSuccess")
 
 
d5prime<-filter(d5prime,gamblePrevTrial==1) 
 
#This filters summary d5prime by odds/mag if specified above via T/F
if(summaryMagFilter){
  filter(d5prime,magCond==SummaryMagCond)
}
if(summaryOddsFilter){
  filter(d5prime,oddsCond==SummaryOddsCond)
}

c("Number of trials that they gambled on:",length(d5prime$ignoreRT[d5prime$ignoreRT!=0]))
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
#hist(setdiff(d5prime$ignoreRT,0),xlim=c(0,1500),breaks=50,main=paste("RT; n =",toString(sum(d5primeBehavioral$gambleCount)),"gambled trials;"),xlab="Reaction time")
#By GambleDelay
d5prime2<-d5prime %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(ignoreRT,0)),
            sdRT=sd(ignoreRT))
d5prime2$seconds<-d5prime2$binsTime
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(d5prime2$seconds,d5prime2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Gamble propensity; n =",toString(sum(d5prime2$gambleCount))," gambled trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

plot(d5prime2$seconds,d5prime2$percentageGambled,xlim = c(0,8),ylim = c(35,50),
     main=paste("Gamble propensity; n =",toString(sum(d5prime2$gambleCount)),"gambled trials;",toString(length(unique(d5prime$uniqueid))),"participants"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5primepRT<-filter(d5prime,ignoreRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(ignoreRT),
            sdRT=sd(ignoreRT))
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
summary(glm(ignoreRT~gambleDelay,
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

hist(rtall$ignoreRT,col=rgb(1,0,0,0.5), main='RTs when gambling', xlab='RT',breaks=40,xlim=c(0,1300))
abline(v=median(rtall$ignoreRT),col="red",lwd=2)

hist(rtsub2$ignoreRT,col=rgb(0,0,1,0.5), add=T,breaks=40)
abline(v=median(rtsub2$ignoreRT),col="blue",lwd=2)
t.test((1/rtsub1$ignoreRT),(1/rtsub2$ignoreRT))
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
for(i in imp){
  print(i)
  dsub<-filter(d,uniqueid==i)
  dsubignored<-filter(dsub,response=="success",gambleDelay!=0)
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
    d4RT<-filter(dsub,ignoreRT!=0) %>% 
      group_by(binsTime) %>% 
      summarise(medianRT=median(ignoreRT),
                sdRT=sd(ignoreRT))
    d4RT$seconds<-d4RT$binsTime
    
    if (subplotRT){
     # plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
    #       main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
    #       xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
      totalRTPlot(dsub,line=T,title=paste("participant:",i))
    }
    
    #This is tracking all the gamble slopes of everyone in subgroup
    mlogtemp<-glm(gambled~gambleDelay,data=filter(dsub,gambleDelay!=0),family="binomial")
    gambleSlopes<-c(gambleSlopes,summary(mlogtemp)$coefficients[2])
  
    
    tempRTdf<-filter(dsub,response=='gamble')
    
    mtempRT<-lm(dsubignored$ignoreRT~dsubignored$gambleDelay)
    
    #This is tracking all the RT slopes of everyone in subgroup
    
    rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])
    
    run<-c(run,i)
    
    #This is just one number to see how much overall they gambled
    gambleMeans=c(gambleMeans,mean(d4$percentageGambled))
    
    }
  
  

}

  
#histograms of slopes 
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

  subplotRT=T
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
for(i in Participants){
    print(i)
    dsub<-filter(d,uniqueid==i)
    dsubignoredfilter(dsub,response=="success",gambleDelay!=0)

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
      d4RT<-filter(dsub,ignoreRT!=0) %>%
        group_by(binsTime) %>%
        summarise(medianRT=median(ignoreRT),
                  sdRT=sd(ignoreRT))
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

      mtempRT<-lm(dsubignored$ignoreRT~dsubignored$gambleDelay)

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
plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',main='All Participants gambleSlopes vs. rtSlopes',xlim=c(-1,1),ylim=c(-90,50),pch=16,col='red')

abline(v=0,col='black')
abline(h=0,col='black')

par(new=TRUE)
plot(subslope1DF$rtSlopes~subslope1DF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',xlim=c(-1,1),ylim=c(-90,50),col='green',pch=16)
legend(.5,55,cex=.7, bty = "n",legend=c("All Participants","RiskSeeking"),col=c("red","green"),title="",pch=16)

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
pg7<-barplot(valueMeans,names.arg = c("Low EV","Mid EV","High EV"),ylim=c(0,80),ylab="Gamble Propensity",main="Effect of Expected Value (EV) on gamble propensity; n=53")
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
ignoreRtPlot(d,type='raw',eb='stderr',title='All participants',ylimit=c(600,900),line=F)

gamblePlot(d5,orig=T,eb=F,ylimit=c(40,55),title='catchFail',line=T)
ignoreRtPlot(d5,type='raw',eb='stderr',title='catchFail',ylimit=c(600,900),line=F)

gamblePlot(d5prime,orig=T,eb=F,ylimit=c(35,50),title='catchSuccess',line=T)
ignoreRtPlot(d5prime,type='raw',eb='stderr',title='catchSuccess',ylimit=c(600,900),line=F)

d5<-filter(d,trialType=='ignoreTrial')
#sfnValueSensitivity
#All participants
compMeans<-c(oddsScoreMean(d,time='early'),oddsScoreMean(d,time='mid'),oddsScoreMean(d,time='late'))
compSds<-c(oddsScoreEb(d,type='sem'),oddsScoreEb(d,type='sem'),oddsScoreEb(d,type='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),col='gray',
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; all data",cex.main=.9,cex.axis = .9,cex.lab=.9)
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#FailCatch (d5)
compMeans<-c(oddsScoreMean(d5,time='early'),oddsScoreMean(d5,time='mid'),oddsScoreMean(d5,time='late'))
compSds<-c(oddsScoreEb(d5,type='sem'),oddsScoreEb(d5,type='sem'),oddsScoreEb(d5,type='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; n=87 participants")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#SuccessCatch (d5prime)
compMeans<-c(oddsScoreMean(d5prime,time='early'),oddsScoreMean(d5prime,time='mid'),oddsScoreMean(d5prime,time='late'))
compSds<-c(oddsScoreEb(d5prime,type='sem'),oddsScoreEb(d5prime,type='sem'),oddsScoreEb(d5prime,type='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Value Sensitivity; n=53 participants")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#sfnGambleSlopes
#2)Gamble slopes of infrequent vs frequent
compMeans<-c(mean(slopeDF$gambleSlopes),mean(subslope1DF$gambleSlopes),mean(subslope2DF$gambleSlopes))
pg10<-barplot(compMeans,names.arg = c("All participants","catchFail","catchSuccess"),ylab="Gamble Slopes (% / second)",ylim=c(-.15,.15),main="Gamble Slopes")
compSems<-c(std.error(slopeDF$gambleSlopes),std.error(subslope1DF$gambleSlopes),std.error(subslope2DF$gambleSlopes))
arrows(pg10,compMeans-compSems,pg10,compMeans+compSems,lwd=2,angle=90,code=3)

#3)RT slopes of infrequent vs frequent
compMeans<-c(mean(slopeDF$rtSlopes),mean(subslope1DF$rtSlopes),mean(subslope2DF$rtSlopes))
pg10<-barplot(compMeans,names.arg = c("All participants","catchFail","catchSuccess"),ylab="RT Slopes (ms / second)",ylim=c(-25,0),main="RT Slopes")
compSems<-c(std.error(slopeDF$rtSlopes),std.error(subslope1DF$rtSlopes),std.error(subslope2DF$rtSlopes))
arrows(pg10,compMeans-compSems,pg10,compMeans+compSems,lwd=2,angle=90,code=3)


#People who gambled a lot
highGamblers<-filter(dBehavioral,percentageGambled>50)$uniqueid
lowGamblers<-filter(dBehavioral,percentageGambled<40)$uniqueid

highInterest<-filter(dBehavioral,percentagePressed>50)$uniqueid

male<-filter(dsurvey,gender==1)$uniqueid
highPressers<-filter()
noProx<-c(981,976,975,972,960,947,933,932,927,910,905,904,897,893,876,874,854,852,830,827,826,824,821,818,816,804,802,801)
#FailCatch analysis
compMeans<-c(sum(dcatchfail$gambleDelay<3.5),sum(dcatchfail$gambleDelay>3.5&dcatchfail$gambleDelay<4.5),sum(dcatchfail$gambleDelay>4.5))
pg11<-barplot(compMeans,names.arg = c("earlyFail","MidFail","LateFail"),ylab="# of catch trials failed",ylim=c(0,30),main="When did people fail catch trials?")











#Odds mag analysis Lowodds

summaryMagFilter=T
summaryMagCond='low'
summaryOddsFilter=F
summaryOddsCond='lowp'

#New way which is better
d5<-dgamble

#This filters summary d5 by odds/mag if specified above via T/F
if(summaryMagFilter){
  d5<-filter(d5,magCond==summaryMagCond)
}
if(summaryOddsFilter){
  d5<-filter(d5,oddsCond==summaryOddsCond)
}

gamblePlot(d5,orig=T,eb='sem',ylimit=c(60,80),title='LowMagAllValue')
totalRTPlot(d5,line=F,title="LowMagAllValue")


#Odds mag analysis Mid

summaryMagFilter=T
summaryMagCond='mid'
summaryOddsFilter=F
summaryOddsCond='midp'

#New way which is better
d5<-dgamble

#This filters summary d5 by odds/mag if specified above via T/F
if(summaryMagFilter){
  d5<-filter(d5,magCond==summaryMagCond)
}
if(summaryOddsFilter){
  d5<-filter(d5,oddsCond==summaryOddsCond)
}

gamblePlot(d5,orig=T,eb='sem',ylimit=c(50,70),title='MidMagAllValue')
totalRTPlot(d5,line=F,title="MidMagAllValue")


#Odds mag analysis HighOdds

summaryMagFilter=T
summaryMagCond='high'
summaryOddsFilter=F
summaryOddsCond='highp'

#New way which is better
d5<-dgamble

#This filters summary d5 by odds/mag if specified above via T/F
if(summaryMagFilter){
  d5<-filter(d5,magCond==summaryMagCond)
}
if(summaryOddsFilter){
  d5<-filter(d5,oddsCond==summaryOddsCond)
}

gamblePlot(d5,orig=T,eb='sem',ylimit=c(30,50),title='HighMagAllValue')
#totalRTPlot(d5,line=F,title="HighMagAllValue")
