###Ramp analysis
###Rewritten/organized 9.4.2018 by Guillaume

#Things to do
#Add High/low RPE
#Add  sub sub analyses
#Add in knobs to filter sub sub categoies per participant
#Add easy to use sandbox mode
#Normalize z score RT

##Loading packages
#install.packages('mosaic')
library(mosaic)

##Loading data
#d0<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/5ramp9.10.csv",sep=",")
d0<-read.csv(file="C:/Users/Guillaume/Documents/GitHub/rampAnalysis/50ramp9.11.csv",sep=",")

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
histogram(d$gambleDelay,breaks=50,xlim=c(0,8),main="Where gambles interrupted trials. All trials",xlab="seconds into trial gamble appeared")



colnames(d)[1]<-"Trialid"

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


#The number of statements in this function should match
#This block works well for participants 228 on..
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


for(i in 1:nrow(d)){
  d[i,10]=binTimeCalc(d,i)
}

#Creating new df to see how many ended up in each bin/ this is sanity check
dbins<-d %>% 
  group_by(binsTime) %>% 
  summarise(Number=length(response))
dbins

#Remove/coalesce any rows with bins if the numbers are too far apart from one another
#Highly conditional, should hardcode for every dataset
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

#Adding which condition trial was in
#For new data set
magCondCalc<-function(d,row){
  if(d[row,7]==1|d[row,7]==2){
    return("low")
  } else if (d[row,7]==3|d[row,7]==4){
    return("mid")
  } else if (d[row,7]==5|d[row,7]==6){
    return("high")
  } else{
    return("null")
  }
}

for(i in 1:nrow(d)){
  d[i,11]=magCondCalc(d,i)
}
colnames(d)[11]<-"magCond"

oddsCondCalc<-function(d,row){
  if(d[row,1]==21|d[row,1]==22|d[row,1]==23|d[row,1]==24|d[row,1]==25|d[row,1]==26){
    return("lowp")
  } else if (d[row,1]==1|d[row,1]==2|d[row,1]==3|d[row,1]==4|d[row,1]==5|d[row,1]==6){
    return("midp")
  } else if (d[row,1]==31|d[row,1]==32|d[row,1]==33|d[row,1]==34|d[row,1]==35|d[row,1]==36){
    return("highp")
  } else{
    return("catch")
  }
}

for(i in 1:nrow(d)){
  d[i,12]=oddsCondCalc(d,i)
}
colnames(d)[12]<-"oddsCond"

#Adding prediction errors:
#Adding prediction errors as possible variable
d2=d[0,]
d[,13]=0
colnames(d)[13]<-"PredictionError"
Participants<-unique(d$uniqueid)

#Adding RPE as a factor
#Currently only one participant
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  dsub[1,13]=dsub[1,7]
  for (row in 2:length(dsub$Trialid)){
    #This is essentially calculating the difference between potential reward on trial
    #t - reward on trial t-1
    dsub[row,13]=(dsub[row,7]-dsub[(row-1),7])
  }
  d2=rbind(d2,dsub)
}
head(d2)
d=d2

#This should be ready to run now without any artificial filtering of participants for low RT etc.

nParticipants<- length(unique(d$uniqueid))
nParticipants

#Check for catch trials
#This prints off the trial ids of anyone who made a mistake
#75 should gamble; 86 should success/fail; 2 catch trials?
head(d)

###Behavioral analyses
##Reaction time
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
histogram(gambleRTs,main="Aggregated gamble RTs",breaks=70)
#Whenever they claimed 'boring' reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
histogram(outcomeRTs,main=c("Aggregated outcome RTs; number of trials:",length(outcomeRTs)),breaks=70)
#Printing experiment times in minutes CONFIRM PSITURK TRACKS IN MILLISECONDS
expTimes<-((unique(d$expTime,na.rm=TRUE)/1000)/60)
histogram(expTimes,main="Experiment Time (in minutes)",breaks=50,xlim=c(0,60))


#Removing participants who gambled too much/not enough

dgamble0<-filter(d,gambleDelay!=0)

dBehavioralTotal<-dgamble0 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

head(dBehavioralTotal)
#Overall preference for gambling
histogram(dBehavioralTotal$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity(everyone) to gamble; n =",toString(sum(dBehavioralTotal$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

dlowG<-filter(dBehavioralTotal,percentageGambled<6)
noGamblers<-dlowG$uniqueid
dhighg<-filter(dBehavioralTotal,percentageGambled>95)
allGamblers<-dhighg$uniqueid
removeIds<-c(noGamblers,allGamblers)

#Removing any subjects from dataset if needed, using unique ids in vector removeIds
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}
#Now this is refinde number of participants
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

histogram(dBehavioral$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity to gamble; n =",toString(sum(dBehavioralTotal$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

#Remaking behavioral histograms
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
histogram(gambleRTs,main="Aggregated gamble RTs",breaks=70)
c("Numberof trials that they gambled on: ",length(gambleRTs))

#Whenever they claimed 'boring' reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
histogram(outcomeRTs,main="Aggregated outcome RTs; number of trials:",breaks=70)
c("Numberof trials that they gambled on: ",length(outcomeRTs))

#Adding another column 1 if they gambled, 0 if they didn't
#This will be outcome variable in logistic regression
for (i in 1:length(dgamble$response)){
  if(dgamble$response[i]=='gamble'){
    dgamble$gambled[i]=1
  } else {
    dgamble$gambled[i]=0
  }
}


#Number of trials per participant 
dTrials<-d %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/ntrials*100))
dTrials
histogram(dTrials$ntrials,breaks=50,xlim=c(0,120),ylim=c(0,30),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")

#Logistic regression for gambled
#Need to figure out which one to use
mlog<-glm(gambled~trialNumber+PredictionError+gambleDelay*oddsCond+
            gambleDelay*magCond+
            PredictionError*gambleDelay,
          data=dgamble,family="binomial");
summary(mlog)

mlog2<-glm(gambled~trialNumber+PredictionError+
             gambleDelay+oddsCond+magCond+PredictionError,
           data=dgamble,family="binomial");
summary(mlog2)

##Total data
c("Number of trials that they gambled on: ",length(d$gambleRT[d$gambleRT!=0]))
c("Number of trials that they had the chance to gamble on: ",length(d$gambleDelay))

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

#Interesting plot of gambleDelay vs propensity to gamble. Add sds? May be meaningless..
plot(d2$seconds,d2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Total group data; Gamble propensity; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)

#Plotting RTs with sd
dRT<-filter(d,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dRT$seconds<-dRT$binsTime
plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; total data; median RT with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dRT$seconds)){
  arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]+(as.numeric(dRT[i,3]))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,2]-(as.numeric(dRT[i,3]))),length=0.05, angle=90, code=3)
}


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
plot(dlowRT$seconds,dlowRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; Low mag; median RT with sd; n =",toString(sum(dlow2$trials)),"trials;"),
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
plot(dmidRT$seconds,dmidRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; Mid mag; median RT with sd; n =",toString(sum(dmid2$trials)),"trials;"),
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
plot(dhighRT$seconds,dhighRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; High mag; median RT with sd; n =",toString(sum(dhigh2$trials)),"trials;"),
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
plot(dlowpRT$seconds,dlowpRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; Low odds; median RT with sd; n =",toString(sum(dlowp2$trials)),"trials;"),
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
plot(dmidpRT$seconds,dmidpRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; Mid odds; median RT with sd; n =",toString(sum(dmidp2$trials)),"trials;"),
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
plot(dhighpRT$seconds,dhighpRT$medianRT,xlim = c(0,8),ylim=c(400,1000),main=paste("Group data; High odds; median RT with sd; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dhighpRT$seconds)){
  arrows(as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,2]+(as.numeric(dhighpRT[i,3]))),as.numeric(dhighpRT$seconds[i]),as.numeric(dhighpRT[i,2]-(as.numeric(dhighpRT[i,3]))),length=0.05, angle=90, code=3)
}

#Need to add high/low RPE sets of 4 graphs

#############################################################################################
#Sandbox mode
#Design your own mag/odds filter
#############################################################################################
#Breaking down by participant

####The following is to just get one participant's data
p=237
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

plot(d3RT$seconds,d3RT$meanRT,xlim = c(0,8),ylim=c(400,1000),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(d3RT$seconds)){
  arrows(as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]+(as.numeric(d3RT[i,3]))),as.numeric(d3RT$seconds[i]),as.numeric(d3RT[i,2]-(as.numeric(d3RT[i,3]))),length=0.05, angle=90, code=3)
}
msubRT<-lm(d3RT$meanRT~d3RT$seconds)
abline(msubRT)
summary(msubRT)
#############################################################################################
#Looping through participants data

subj<-unique(d$uniqueid)
intN<-NULL
botN<-NULL
plotRT=FALSE
plotGD=TRUE
#Add in knobs for different sub categories (though this number is very small....)


for(i in Participants){
  print(i)
  dsub<-d[d$uniqueid==i,]
  d4<-dsub %>% 
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
  #This checks to see if any participant is ramping
  if(summary(mtemp)$coefficients[8]<.05 & summary(mtemp)$coefficients[2]>0){
    intN<-c(intN,i)
  }
  #Check to see if any participant has a RT that's susupiciously low..
  if(mean(setdiff(dsub$outcomeRT,0),na.rm=TRUE)<250){
    botN<-c(botN,i)
  }
}


#Looking into participants that ramp
for(i in intN){
}