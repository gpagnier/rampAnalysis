###Ramp analysis
###Rewritten/organized 9.4.2018 by Guillaume

#Things to do
#Filter df that only includes participants who passed catch
#Need to find a way to quantify per particpiant gambleRamp and rtRAMP
#Add in knobs to filter sub sub categoies per participant - very few trials though
#
#Normalize z score RT

##Loading packages
#install.packages('mosaic')
library(mosaic)

##Loading data
#d0<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")
#d0<-read.csv(file="C:/Users/Guillaume/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")
d0<-read.csv(file="//files.brown.edu/Home/gpagnier/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")

#Cleaning data
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
#Warning! CSV needs to be in exact column order:
#"trialid" #"expTime" "gambleDelay" "gambleRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"

#Need to replace uniqueid with numbers and clean out columns that aren't useful
d<-d0[,c("Trialid","expTime","gambleDelay","gambleRT","outcomeRT","response","standardGamble","trialNumber","uniqueid")]
d<-subset(d,!grepl("debug",as.character(d$uniqueid)))
d<-subset(d,d$response!="")

#Adding col uniqueID uniqueid with numbers
d$uniqueID=NA
seed=201
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
removeIds=c()
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}
unique(d$uniqueid)

#Where did gambles interrupt
hist(d$gambleDelay,breaks=50,xlim=c(0,8),main="Where gambles interrupted trials. All trials",xlab="seconds into trial gamble appeared",col='black')


#How many points do you want on the gambleDelay?
d$binsTme=NULL
bins=9

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


#This is the function that creates gambleBins
binTimeCalc<-function(d,row){
  if(d[row,3]==0)
  {return(0)}
  else if (d[row,3]>0&d[row,3]<=a1tail)
  {return(mean(c(a1head,a1tail)))}
  else if (d[row,3]>=a2head&d[row,3]<=a2tail)
  {return(mean(c(a2head,a2tail)))}
  else if (d[row,3]>=a3head&d[row,3]<=a3tail)
  {return(mean(c(a3head,a3tail)))}
  else if (d[row,3]>=a4head&d[row,3]<=a4tail)
  {return(mean(c(a4head,a4tail)))}
  else if (d[row,3]>=a5head&d[row,3]<=a5tail)
  {return(mean(c(a5head,a5tail)))}
  else if (d[row,3]>=a6head&d[row,3]<=a6tail)
  {return(mean(c(a6head,a6tail)))}
  else if (d[row,3]>=a7head&d[row,3]<=a7tail)
  {return(mean(c(a7head,a7tail)))}
  else if (d[row,3]>=a8head&d[row,3]<=a8tail)
  {return(mean(c(a8head,a8tail)))}
  else if (d[row,3]>=a9head&d[row,3]<=max(d$gambleDelay))
  {return(mean(c(a9head,a9tail)))}
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
#Right now this takes the last one and adds it to the last 'bin'
for(i in 1:nrow(d)){
  if(d[i,"binsTime"]==mean(c(a8head,a8tail))){
    d[i,"binsTime"]=mean(c(a7head,a7tail))
    i
  }
};
dbins<-d %>% 
  group_by(binsTime) %>% 
  summarise(Number=length(response))
dbins

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


Participants<-unique(d$uniqueid)

#Adding RPE as a factor AND normalized z score
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
    if(dsub[row,"gambled"]){
      dsub[row,"rpe2"]=(dsub[row,"standardGamble"]-(dsub[(row-1),"gambleMaxAmt"]/2))
    } else{
      dsub[row,"rpe2"]=(dsub[row,"standardGamble"]-dsub[(row-1),"standardGamble"])
    }
  }
  #This is rpe3
  for (row in 2:length(dsub$Trialid)){
    if(dsub[row,"gambled"]){
      dsub[row,"rpe3"]=(dsub[row,"standardGamble"]-(dsub[(row-1),"gambleMaxAmt"]))
    } else{
      dsub[row,"rpe3"]=(dsub[row,"standardGamble"]-dsub[(row-1),"standardGamble"])
    }
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

#Check for catch trials
#75 should gamble; 86 should success/fail; 2 catch trials?
#Check for catch trials
dcatch<-filter(d,Trialid==75|Trialid==86)[,c(1,6,9)]
#dcatch[order(dcatch$Trialid),]
dcatchGamble<-dcatch[dcatch$Trialid==75,]
failCatchId<-dcatchGamble[dcatchGamble$response!='gamble',]$uniqueid
dcatchSuccess<-dcatch[dcatch$Trialid==86,]

failCatchId<-unique(c(failCatchId,dcatchSuccess[dcatchSuccess$response=='gamble',]$uniqueid))
catchSuccessId<-setdiff(Participants,failCatchId)




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

#Removing any subjects from dataset, using unique ids in vector removeIds
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}
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

hist(dBehavioral$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall propensity to gamble; n =",toString(sum(dBehavioral$trials))," possible trials;",nParticipants,"subj"),xlab="Percentage of time gambled")
boxplot(dBehavioral$percentageGambled,ylim=c(0,100),main="Distribution of gambling percentages across everyone",ylab="Percentage gambled (per participant)")

#Remaking behavioral histograms
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
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
hist(dTrials$ntrials,breaks=50,xlim=c(0,120),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")


#Logistic regression models to predict gambled
#Need to figure out which one to use
mlog<-glm(gambled~rpe3+oddsCond,
          data=dgamble,family="binomial");
summary(mlog)

mlog2<-glm(gambled~trialNumber+rpe1+
             gambleDelay+oddsCond+magCond+rpe2,
           data=dgamble,family="binomial");
summary(mlog2)

##Total data
c("Number of trials that they gambled on: ",length(dgamble$response[dgamble$response=='gamble']))
c("Number of trials that they had the chance to gamble on: ",length(dgamble$response))

#By GambleDelay
d2<-d %>% 
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

#Interesting plot of gambleDelay vs propensity to gamble. NEED TO ADD SDS HERE
plot(d2$seconds,d2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Total group data; Gamble propensity; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting gamble RTs with sd
dRT<-filter(dgamble,response=='gamble') %>%
  group_by(binsTime) %>%
  summarise(medianRT=median(gambleRT),
            sdRT=sd(gambleRT),
            medianSpeed=median(NgambleRT),
            sdSpeed=sd(NgambleRT))
dRT$seconds<-dRT$binsTime

#This is raw RT
plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data; total data; median RT with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dRT$seconds)){
  arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]+(as.numeric(dRT[i,3]))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]-(as.numeric(dRT[i,3]))),length=0.05, angle=90, code=3)
}

#Took a break here to keep on rewriting
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
dBehavioralLow
#How much did each participant choose to gamble
histogram(dBehavioralLow$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low-mag gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(dlow$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for low mag gambles",xlab="Reaction time")
#By GambleDelay
dlow2<-dlow %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dlow2$seconds<-dlow2$binsTime
dlow2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dlow2$seconds,dlow2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low mag; Gamble propensity; n =",toString(sum(dlow2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dlowRT<-filter(dlow,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dlowRT$seconds<-dlowRT$binsTime
plot(dlowRT$seconds,dlowRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; Low mag; median RT with sd; n =",toString(sum(dlow2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dlowRT$seconds)){
  arrows(as.numeric(dlowRT$seconds[i]),as.numeric(dlowRT[i,2]+(as.numeric(dlowRT[i,3]))),as.numeric(dlowRT$seconds[i]),as.numeric(dlowRT[i,2]-(as.numeric(dlowRT[i,3]))),length=0.05, angle=90, code=3)
}

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
dBehavioralMid
#How much did each participant choose to gamble
histogram(dBehavioralMid$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on mid-mag gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(dmid$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for mid mag gambles",xlab="Reaction time")
#By GambleDelay
dmid2<-dmid %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dmid2$seconds<-dmid2$binsTime
dmid2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dmid2$seconds,dmid2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Mid mag; Gamble propensity; n =",toString(sum(dmid2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dmidRT<-filter(dmid,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dmidRT$seconds<-dmidRT$binsTime
plot(dmidRT$seconds,dmidRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; Mid mag; median RT with sd; n =",toString(sum(dmid2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dmidRT$seconds)){
  arrows(as.numeric(dmidRT$seconds[i]),as.numeric(dmidRT[i,2]+(as.numeric(dmidRT[i,3]))),as.numeric(dmidRT$seconds[i]),as.numeric(dmidRT[i,2]-(as.numeric(dmidRT[i,3]))),length=0.05, angle=90, code=3)
}

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
dBehavioralHigh
#How much did each participant choose to gamble
histogram(dBehavioralHigh$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on high-mag gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(dhigh$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for high mag gambles",xlab="Reaction time")
#By GambleDelay
dhigh2<-dhigh %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dhigh2$seconds<-dhigh2$binsTime
dhigh2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dhigh2$seconds,dhigh2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("High mag; Gamble propensity; n =",toString(sum(dhigh2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dhighRT<-filter(dhigh,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dhighRT$seconds<-dhighRT$binsTime
plot(dhighRT$seconds,dhighRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; High mag; median RT with sd; n =",toString(sum(dhigh2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dhighRT$seconds)){
  arrows(as.numeric(dhighRT$seconds[i]),as.numeric(dhighRT[i,2]+(as.numeric(dhighRT[i,3]))),as.numeric(dhighRT$seconds[i]),as.numeric(dhighRT[i,2]-(as.numeric(dhighRT[i,3]))),length=0.05, angle=90, code=3)
}


#How does odds(payout) affect gamble propensity
#Low odds = 1.5 x standard gamble
#Mid odds = 2 x standard gamble
#High odds = 3 x standard gamble



##Low odds
dlowp<-filter(dgamble,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
c("Number of trials that they gambled on: ",length(dlowp$gambleRT[dlowp$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dlowp$gambleDelay))
#By uniqueId
dBehavioralLowp<-dlowp %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioralLowp
#How much did each participant choose to gamble
histogram(dBehavioralLowp$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low odds gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(dlowp$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for low odds gambles",xlab="Reaction time")
#By GambleDelay
dlowp2<-dlowp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dlowp2$seconds<-dlowp2$binsTime
dlowp2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dlowp2$seconds,dlowp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low odds; Gamble propensity; n =",toString(sum(dlowp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dlowpRT<-filter(dlowp,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dlowpRT$seconds<-dlowpRT$binsTime
plot(dlowpRT$seconds,dlowpRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; Low odds; median RT with sd; n =",toString(sum(dlowp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dlowpRT$seconds)){
  arrows(as.numeric(dlowpRT$seconds[i]),as.numeric(dlowpRT[i,2]+(as.numeric(dlowpRT[i,3]))),as.numeric(dlowpRT$seconds[i]),as.numeric(dlowpRT[i,2]-(as.numeric(dlowpRT[i,3]))),length=0.05, angle=90, code=3)
}

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
dBehavioralMidp
#How much did each participant choose to gamble
histogram(dBehavioralMidp$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on mid odds gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(dmidp$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for mid odds gambles",xlab="Reaction time")
#By GambleDelay
dmidp2<-dmidp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dmidp2$seconds<-dmidp2$binsTime
dmidp2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dmidp2$seconds,dmidp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Mid odds; Gamble propensity; n =",toString(sum(dmidp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dmidpRT<-filter(dmidp,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dmidpRT$seconds<-dmidpRT$binsTime
plot(dmidpRT$seconds,dmidpRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; Mid odds; median RT with sd; n =",toString(sum(dmidp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dmidpRT$seconds)){
  arrows(as.numeric(dmidpRT$seconds[i]),as.numeric(dmidpRT[i,2]+(as.numeric(dmidpRT[i,3]))),as.numeric(dmidpRT$seconds[i]),as.numeric(dmidpRT[i,2]-(as.numeric(dmidpRT[i,3]))),length=0.05, angle=90, code=3)
}

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
dBehavioralHighp
#How much did each participant choose to gamble
histogram(dBehavioralHighp$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on high odds gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(dhighp$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for high odds gambles",xlab="Reaction time")
#By GambleDelay
dhighp2<-dhighp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dhighp2$seconds<-dhighp2$binsTime
dhighp2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(dhighp2$seconds,dhighp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("High odds; Gamble propensity; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dhighpRT<-filter(dhighp,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dhighpRT$seconds<-dhighpRT$binsTime
plot(dhighpRT$seconds,dhighpRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; High odds; median RT with sd; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dhighpRT$seconds)){
  arrows(as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,2]+(as.numeric(dhighpRT[i,3]))),as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,2]-(as.numeric(dhighpRT[i,3]))),length=0.05, angle=90, code=3)
}
#####################################################################################
#RPE
#Need to add high/low RPE sets of 6 graphs -rpe1, rpe2, rpe3
drpe<-filter(dgamble,dgamble$rpe3>0)
length(drpe$response)
#By uniqueId
dBehavioralrpe<-drpe %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioralrpe
#How much did each participant choose to gamble
histogram(dBehavioralrpe$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on RPE gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(drpe$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for RPE gambles",xlab="Reaction time")
#By GambleDelay
drpe2<-drpe %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
drpe2$seconds<-drpe2$binsTime
drpe2
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(drpe2$seconds,drpe2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("RPE; Gamble propensity; n =",toString(sum(drpe2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
drpeRT<-filter(drpe,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
drpeRT$seconds<-drpeRT$binsTime
plot(drpeRT$seconds,drpeRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; RPE; median RT with sd; n =",toString(sum(drpe2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(drpeRT$seconds)){
  arrows(as.numeric(drpeRT$seconds[i]),as.numeric(drpeRT[i,2]+(as.numeric(drpeRT[i,3]))),as.numeric(drpeRT$seconds[i]),as.numeric(drpeRT[i,2]-(as.numeric(drpeRT[i,3]))),length=0.05, angle=90, code=3)
}

#############################################################################################
#Sandbox mode
#Design your own mag/odds filter for group participants

mag="high"
odds="mid"

#Breaking down by mag condition?
if(mag=="low"){
  d5=dlow
} else if(mag=="mid"){
  d5=dmid
} else if (mag=="high"){
  d5=dhigh}

#If you dont want both, run the following line which resets
d5=dgamble

#Breaking down by odds condition?
if(odds=="low"){
  d5=filter(dgamble,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
} else if(odds=="mid"){
  d5=filter(dgamble,Trialid==1|Trialid==2|Trialid==3|Trialid==4|Trialid==5|Trialid==6)
} else if(odds=="high"){
  d5=filter(dgamble,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
}









#Breaking down by RPE
d5=filter(dgamble,rpe3>(1))

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
histogram(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble; n =",toString(sum(d5Behavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(d5$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time ",xlab="Reaction time")
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
     main=paste("Gamble propensity; n =",toString(sum(d52$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5pRT<-filter(d5,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d5pRT$seconds<-d5pRT$binsTime
plot(d5pRT$seconds,d5pRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Group data; median RT with sd; n =",toString(sum(d52$gambleCount)),"trials;"),
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







for(i in 1:length(d5pRT$seconds)){
  arrows(as.numeric(d5pRT$seconds[i]),as.numeric(d5pRT[i,2]+(as.numeric(d5pRT[i,3]))),as.numeric(dhighpRT$seconds[i]),as.numeric(d5pRT[i,2]-(as.numeric(d5pRT[i,3]))),length=0.05, angle=90, code=3)
}

#Sandbox mode
#Design your own mag/odds filter for group participants

mag="high"
odds="low"

d5<-filter(dgamble,uniqueid==231|uniqueid==245|uniqueid==267|uniqueid==279|uniqueid==282|uniqueid==298|uniqueid==308|uniqueid==334|uniqueid==352|uniqueid==367|uniqueid==371|uniqueid==375|uniqueid==360|uniqueid==296|uniqueid==287)
c("Number of trials that they gambled on: ",length(dhighp$gambleRT[dhighp$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(dhighp$gambleDelay))
d5<-filter(d5,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
d5<-filter(d5,Trialid==1|Trialid==2|Trialid==3|Trialid==4|Trialid==5|Trialid==6)
d5<-filter(d5,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
d5<-filter(d5,standardGamble==1|standardGamble==2)
d5<-filter(d5,standardGamble==3|standardGamble==4)
d5<-filter(d5,standardGamble==5|standardGamble==6)


#By uniqueId
d5Behavioral<-d5 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d5Behavioral
#How much did each participant choose to gamble
histogram(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on mag gambles; n =",toString(sum(d5Behavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(d5$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for gambles",xlab="Reaction time")
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
d52
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(d52$seconds,d52$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Gamble propensity; n =",toString(sum(d52$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5pRT<-filter(d5,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d5pRT$seconds<-d5pRT$binsTime
plot(d5pRT$seconds,d5pRT$medianRT,xlim = c(0,8),ylim=c(600,800),main=paste("Small Group data;; median RT with sd; n =",toString(sum(d52$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(d5pRT$seconds)){
  arrows(as.numeric(d5pRT$seconds[i]),as.numeric(d5pRT[i,2]+(as.numeric(d5pRT[i,3]))),as.numeric(dhighpRT$seconds[i]),as.numeric(d5pRT[i,2]-(as.numeric(d5pRT[i,3]))),length=0.05, angle=90, code=3)
}
#############################################################################################
#Breaking down by participant

#Some definitions
highMag<-c(5,6,25,26,35,36)


####The following is to just get one participant's data
p=256
dsub<-d[d$uniqueid==p,]
d3<-dsub %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d3$seconds<-d3$binsTime

par(ps = 12, cex = 1, cex.main = 1)
plot(d3$seconds,d3$percentageGambled,xlim = c(0,7),ylim = c(0,100),
     main=paste("Individual participant data (all trials); n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Percentage Gambled")
mtemp<-lm(d3$percentageGambled~d3$seconds)
summary(mtemp)
#One participant RT
d3RT<-filter(dsub,gambleRT!=0) %>% 
  group_by(binsTime) %>% 
  summarise(meanRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d3RT$seconds<-d3RT$binsTime

plot(d3RT$seconds,d3RT$meanRT,xlim = c(0,8),ylim=c(200,1500),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(d3RT$seconds)){
  arrows(as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]+(as.numeric(d3RT[i,3]))),as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]-(as.numeric(d3RT[i,3]))),length=0.05, angle=90, code=3)
}
msubRT<-lm(d3RT$meanRT~d3RT$seconds)
abline(msubRT)
summary(msubRT)
#Gamble slope
summary(mtempRT)$coefficients[2]
#RT slope
tempRTdf<-filter(dsub,gambleRT!=0)
mtempRT<-lm(tempRTdf$gambleRT~tempRTdf$gambleDelay)
#############################################################################################
#Looping through participants data

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

rtSlopes<-NULL
gambleSlopes<-NULL
run<-NULL
slopeDF<-NULL
plotRT=FALSE
plotGD=TRUE
#Add in knobs for different sub categories (though this number is very small....)

#Participants is default (all participants)
#If you want to run sub groups, go to bottom of script
for(i in Participants){
  print(i)
  dsub<-d[d$uniqueid==i,]
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
    summarise(meanRT=mean(gambleRT),
              sdRT=sd(gambleRT))
  d4RT$seconds<-d4RT$binsTime
  
  if (plotRT){
    plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
         main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
         xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
  }
  
  mtemp<-lm(d4$percentageGambled~d4$seconds)
  tempRTdf<-filter(dsub,gambleRT!=0)
  
  mtempRT<-lm(tempRTdf$gambleRT~tempRTdf$gambleDelay)
  #This checks to see if any participant is ramping
  if(summary(mtemp)$coefficients[8]<.05 & summary(mtemp)$coefficients[2]>0){
    intN<-c(intN,i)
  }
  #Check to see if any participant has a RT that's susupiciously low..
  if(mean(setdiff(dsub$outcomeRT,0),na.rm=TRUE)<250){
    botN<-c(botN,i)
  }
  #Check to see if participant has a downwards decreasing RT ramp
  
  if(summary(mtempRT)$coefficients[8]<.1 & summary(mtempRT)$coefficients[2]<0){
    rtn<-c(rtn,i)
  }
  #Making df of gambleSlopes, rtSlopes, and i (who was just analyzed)
  rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])
  gambleSlopes<-c(gambleSlopes,summary(mtemp)$coefficients[2])
  run<-c(run,i)
}

slopeDF<-data.frame(cbind(run,rtSlopes,gambleSlopes))
intN=c(intN,375,360,296,287,272,237)
a<-oddsN[order(oddsN$OddsGamblingScore),]
oddsN<-as.integer(a$Participant[82:112])



plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',main='All Participants (n=112)',xlim=c(-10,15),ylim=c(-90,40))
with(slopeDF, text(slopeDF$rtSlopes~slopeDF$gambleSlopes, labels = slopeDF$run,cex=.8), pos = 2)
m1<-lm(slopeDF$rtSlopes~slopeDF$gambleSlopes)
abline(m1)
cor(slopeDF$rtSlopes~slopeDF$gambleSlopes)




plot(slopeDF$gambleSlopes~slopeDF$rtSlopes,xlab='rtSlopes',ylab='gambleSlopes',main='All Participants (n=112)',xlim=c(-90,40),ylim=c(-10,15))
with(slopeDF, text(slopeDF$gambleSlopes~slopeDF$rtSlopes, labels = slopeDF$run,cex=.8), pos = 2)
m1<-lm(slopeDF$gambleSlopes~slopeDF$rtSlopes)
abline(m1)













#Looking into subsets of participants, high mag preferetial, odds, etc.


#group rtn = group that was automatically filtered


#Breaking down by oddsScore
##This is old way
#d5<-filter(dgamble,uniqueid==209|uniqueid==219|uniqueid==225|uniqueid==262|uniqueid==353|uniqueid==235|uniqueid==253|uniqueid==351|uniqueid==355|uniqueid==266|uniqueid==280|uniqueid==376|uniqueid==212|uniqueid==239|uniqueid==273|uniqueid==297)
#New way which is better
d5<-dgamble[dgamble$uniqueid %in% rtn,]
d5<-dgamble[dgamble$uniqueid %in% Participants,]
c("Number of trials that they gambled on:",length(d5$gambleRT[d5$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on:",length(d5$gambleDelay))


#CONDITIONAL IF YOU WANT TO LOOK AT SUBGROUPS OF ODDS
d5<-filter(d5,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
d5<-filter(d5,Trialid==1|Trialid==2|Trialid==3|Trialid==4|Trialid==5|Trialid==6)
d5<-filter(d5,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)

#By uniqueId
d5Behavioral<-d5 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
#How much did each participant choose to gamble
histogram(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble; n =",toString(sum(d5Behavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
#RTs histogram
histogram(setdiff(d5$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time ",xlab="Reaction time")
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
     main=paste("Gamble propensity; n =",toString(sum(d52$gambleCount)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5pRT<-filter(d5,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d5pRT$seconds<-d5pRT$binsTime
plot(d5pRT$seconds,d5pRT$medianRT,xlim = c(0,8),ylim=c(500,1000),main=paste("Group data; median RT with sd; n =",toString(sum(d52$gambleCount)),"trials;"),
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
d6<-filter(d5,response=='gamble')
#Sig effect of gambleDelay on gambling? 
summary(glm(gambled~gambleDelay,
            data=d5,family="binomial"))
#Sig effect of gambleDelay on RT when gambling? 
  summary(glm(gambleRT~gambleDelay,
              data=d6))


#Overlaying histograms
d5<-d[d$uniqueid %in% catchSucc,]

rthigh<-filter(d,response=='success')
rtlow<-filter(d5,response=='success')

hist(rthigh$outcomeRT,col=rgb(1,0,0,0.5), main='RTs when accepting sure thing', xlab='RT',breaks=40,xlim=c(0,1200))
abline(v=median(rthigh$outcomeRT),col="red",lwd=2)
hist(rtlow$outcomeRT,col=rgb(0,0,1,0.5), add=T,breaks=40)
abline(v=median(rtlow$outcomeRT),col="blue",lwd=2)


rthigh<-filter(d,response=='gamble')
rtlow<-filter(d5,response=='gamble')

hist(rthigh$gambleRT,col=rgb(1,0,0,0.5), main='RTs when gambling', xlab='RT',breaks=40,xlim=c(0,1200))
abline(v=median(rthigh$gambleRT),col="red",lwd=2)

hist(rtlow$gambleRT,col=rgb(0,0,1,0.5), add=T,breaks=40)
abline(v=median(rtlow$gambleRT),col="blue",lwd=2)















#Looking into participants who shown downwards ramp in RT
#rtn2<-c(367,354,348,345,334,324,304,296,291,272,261,257,237)

d5<-dgamble[dgamble$uniqueid %in% rtn,]
c("Number of trials that they gambled on: ",length(d5$gambleRT[d5$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(d5$gambleDelay))
c("Percentage of trials they gambled on: ",length(d5$gambleRT[d5$gambleRT!=0])/length(d5$gambleDelay))

#Only click on one of the following conditions otherwise it's just too much of a filter
d5<-filter(d5,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
d5<-filter(d5,Trialid==1|Trialid==2|Trialid==3|Trialid==4|Trialid==5|Trialid==6)
d5<-filter(d5,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
d5<-filter(d5,standardGamble==1|standardGamble==2)
d5<-filter(d5,standardGamble==3|standardGamble==4)
d5<-filter(d5,standardGamble==5|standardGamble==6)


#By uniqueId
d5Behavioral<-d5 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d5Behavioral
#How much did each participant choose to gamble
hist(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,10),xlim=c(-5,100),main=paste("Propensity to gamble on gambles; n =",toString(sum(d5Behavioral$trials)),"trials;",length(unique(d5$uniqueid)),"participants"),xlab="Percentage of time gambled")
#RTs histogram
hist(setdiff(d5$gambleRT,0),xlim=c(0,1000),breaks=50,main="Reaction Time for gambles",xlab="Reaction time")
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
d52
#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(d52$seconds,d52$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Gamble propensity; n =",toString(sum(d52$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
d5pRT<-filter(d5,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d5pRT$seconds<-d5pRT$binsTime
plot(d5pRT$seconds,d5pRT$medianRT,xlim = c(0,8),ylim=c(400,800),main=paste("Small Group data; median RT with sd; n =",toString(sum(d52$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(d5pRT$seconds)){
  arrows(as.numeric(d5pRT$seconds[i]),as.numeric(d5pRT[i,2]+(as.numeric(d5pRT[i,3]))),as.numeric(dhighpRT$seconds[i]),as.numeric(d5pRT[i,2]-(as.numeric(d5pRT[i,3]))),length=0.05, angle=90, code=3)
}

summary(glm(gambled~gambleDelay,
                  data=d5,family="binomial"))






#Extra analyses maybe save for later
#Overlaying RTs (no gambles) for high mag (red) vs low mag (blue) trials
d6<-d[d$uniqueid %in% rtn,]

rthigh<-filter(d,response=='success')
rtlow<-filter(d5,response=='success')

hist(rthigh$gambleRT,col=rgb(1,0,0,0.5), main='RTs when accepting sure thing', xlab='RT',breaks=50,xlim=c(0,1200))

hist(rtlow$gambleRT,col=rgb(0,0,1,0.5), add=T,breaks=50)

#Looping through subgroups
splotRT=FALSE
splotGD=FALSE
soddsN=NULL
oddsScoreKnob=FALSE
magScoreKnob=FALSE
smagN=NULL
rtSlopes<-NULL
gambleSlopes<-NULL
run<-NULL
slopeDF<-NULL

for(i in Participants){
  print(i)
  dsub<-filter(d,uniqueid==i&gambleDelay!=0,oddsCond=='highp')
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
  
  if (splotGD){
    plot(d4$seconds,d4$percentageGambled,xlim = c(0,7),ylim = c(0,100),
         main=paste("Individual participant data; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
         xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
  }
  
  
  #Have to put in a check to see if they actually gambled
  if(sum(d4$percentageGambled)>0){
    #One participant RT
    d4RT<-filter(dsub,gambleRT!=0) %>% 
      group_by(binsTime) %>% 
      summarise(meanRT=mean(gambleRT),
                sdRT=sd(gambleRT))
    d4RT$seconds<-d4RT$binsTime
    
    if (splotRT){
      plot(d4RT$seconds,d4RT$meanRT,xlim = c(0,8),ylim=c(400,1000),
           main=paste("Individual participant RT; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
           xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
    }
    mtemp<-lm(d4$percentageGambled~d4$seconds)
    gambleSlopes<-c(gambleSlopes,summary(mtemp)$coefficients[2])
  
    
    tempRTdf<-filter(dsub,gambleRT!=0)
    
    mtempRT<-lm(tempRTdf$gambleRT~tempRTdf$gambleDelay)
    #Making df of gambleSlopes, rtSlopes, and i (who was just analyzed)
    rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])
    
    run<-c(run,i)
    
    }
  
  

}
slopeDF<-data.frame(cbind(run,rtSlopes,gambleSlopes))
slopeDF<-filter(slopeDF,slopeDF$rtSlopes>-80&slopeDF$rtSlopes<40)
#slopeDF analysis

plot(slopeDF$gambleSlopes~slopeDF$rtSlopes,xlab='rtSlopes',ylab='gambleSlopes',main='All participants',xlim=c(-90,40),ylim=c(-10,15))
#with(slopeDF, text(slopeDF$gambleSlopes~slopeDF$rtSlopes, labels = slopeDF$run,cex=.8), pos = 2)
summary(m1<-lm(slopeDF$gambleSlopes~slopeDF$rtSlopes))
abline(m1)

plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='gambleSlopes',ylab='rtSlopes',main='All participants',xlim=c(-10,15),ylim=c(-90,40))
#with(slopeDF, text(slopeDF$rtSlopes~slopeDF$gambleSlopes, labels = slopeDF$run,cex=.8), pos = 2)
summary(m1<-lm(slopeDF$rtSlopes~slopeDF$gambleSlopes))
abline(m1)
cor(slopeDF$rtSlopes~slopeDF$gambleSlopes)
