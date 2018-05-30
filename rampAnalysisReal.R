install.packages('mosaic')
library(mosaic)

#Warning! CSV needs to be in exact column order:
#"trialid" #"expTime" "gambleDelay" "gambleRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"


#d<-read.csv(file="C:/Users/gpagnier/Documents/Ramping data/ramp.csv",sep=",")
d0<-read.csv(file.choose(),header=TRUE)
d<-d0
d[d==""] <- NA
d$gambleDelay<-d$gambleDelay/1000
d$binsTime=0;

#Getting rid of any trial which gamble interrupted too early
d<-filter(d,gambleDelay<5.6)
histogram(d$gambleDelay,breaks=50,xlim=c(0,7),main="Where gambles interrupted trials. All trials",xlab="seconds into trial gamble appeared")
histogram(d$gambleDelay,breaks=50,xlim=c(.5,7),ylim=c(0,6),main="Where gambles interrupted trials. All trials",xlab="seconds into trial gamble appeared")

binTimeCalc<-function(row){
  if(d[row,3]==0){
    return(0)
  } else if (d[row,3]>0&d[row,3]<1){
    return(.5)
  } else if (d[row,3]>1&d[row,3]<2.5){
    return(1.75)
  } else if (d[row,3]>2.5&d[row,3]<3){
    return(2.75)  
  } else if (d[row,3]>3&d[row,3]<3.4){
    return(3.2)  
  } else if (d[row,3]>3.4&d[row,3]<4.75){
    return(4.1)
  } else if (d[row,3]>4.75&d[row,3]<5.25){
    return(5.1) 
  } else if (d[row,3]>5.25&d[row,3]<5.6){
    return(5.5) 
    } else{
    return(5.5)
  }
}

#For new data set
binTimeCalc<-function(d,row){
  if(d[row,3]==0){
    return(0)
  } else if (d[row,3]>0&d[row,3]<1){
    return(.75)
  } else if (d[row,3]>1&d[row,3]<2.5){
    return(1.35)
  } else if (d[row,3]>2.5&d[row,3]<3){
    return(2.9)  
  } else if (d[row,3]>3&d[row,3]<4.5){
    return(3.2)
  } else if (d[row,3]>4.5&d[row,3]<4.8){
    return(4.65) 
  } else if (d[row,3]>4.8&d[row,3]<5.0){
    return(4.9)
  } else if (d[row,3]>5.0&d[row,3]<5.2){
    return(5.1) 
  } else if (d[row,3]>5.2&d[row,3]<5.5){
    return(5.4) 
  } else{
    return(999)
  }
}


#binTimeCalcEqualTrials<-function(row){
#  if(d[row,3]==0){
#    return(0)
#  } else if (d[row,3]>0&d[row,3]<2.57){
#    return(1)
#  } else if (d[row,3]>2.57&d[row,3]<3.22){
#    return(2.75)
#  } else if (d[row,3]>3.22&d[row,3]<5.35){
#    return(4.25)  
#  } else if (d[row,3]>5.35&d[row,3]<6.5){
#    return(6)  
#   } else{
#    return(NULL)
#  }
#}



#for(i in 1:nrow(d)){
#  d[i,10]=binTimeCalcEqualTrials(i)
#}

for(i in 1:nrow(d)){
  d[i,10]=binTimeCalc(d,i)
}


head(d)
participants<-unique(d$uniqueid)
nParticipants<- length(unique(d$uniqueid))
nParticipants

#Check for catch trials
#dcatch<-filter(d,Trialid==75|Trialid==85)[,c(1,6,9)]
#dcatch[order(dcatch$Trialid),]
#This prints off the trial ids of anyone who made a mistake
#75 should gamble; 85 should success/fail; HOW MANY CATCH TRIALS ARE THERE?
removeIds<-NULL

#catch<-for(i in 1:nrow(dcatch)){
#  if(dcatch[i,1]==85&dcatch[i,2]=="gamble"){
#    removeIds<-c(removeIds,dcatch[i,3])
#  }else if(dcatch[i,1]==75&dcatch[i,2]=="success"){
#    removeIds<-c(removeIds,dcatch[i,3])
#  }else if(dcatch[i,1]==75&dcatch[i,2]=="fail"){
#    removeIds<-c(removeIds,dcatch[i,3])
#  }
#}


#Removing any subjects from dataset if needed, using unique ids in vector removeIds
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}

nParticipants<- length(unique(d$uniqueid))
Participants<-unique(d$uniqueid)
nParticipants


###Behavioral analyses
##Reaction time
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
histogram(gambleRTs,main="Aggregated gamble RTs",breaks=50)
#Whenever they claimed 'boring' reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
histogram(outcomeRTs,main="Aggregated outcome RTs",breaks=50)
#Printing experiment times in minutes CONFIRM PSITURK TRACKS IN MILLISECONDS
expTimes<-((unique(d$expTime,na.rm=TRUE)/1000)/60)
histogram(expTimes,main="Experiment Time (in minutes)",breaks=50)
histogram(expTimes,main="Experiment Time (in minutes)",breaks=50,xlim=c(0,50))


#Always run the following block

dgamble<-filter(d,gambleDelay!=0)

dBehavioralTotal<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime,na.rm=TRUE)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

head(dBehavioralTotal)
#Overall preference for gambling
histogram(dBehavioralTotal$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity to gamble; n =",toString(sum(dBehavioralTotal$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

dlowG<-filter(dBehavioralTotal,percentageGambled<6)
noGamblers<-dlowG$uniqueid
dhighg<-filter(dBehavioralTotal,percentageGambled>95)
allGamblers<-dhighg$uniqueid
removeIds<-c(noGamblers,allGamblers)


#Removing any subjects from dataset if needed, using unique ids in vector removeIds
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}

nParticipants<- length(unique(d$uniqueid))
Participants<-unique(d$uniqueid)
nParticipants
#Always run the following block2

dgamble<-filter(d,gambleDelay!=0)

dBehavioral<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

#Preference for low gambles 
dlow<-filter(dgamble,standardGamble==1|standardGamble==2)
dBehavioral<-dlow %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,50),xlim=c(-5,100),main=paste("Propensity to gamble on low-mag gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dlow$gambleRT,0),breaks=50,main="Reaction Time for low mag gambles",xlab="Reaction time")

dlow2<-dlow %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=median(setdiff(gambleRT,0)),
            sdRT=sd(gambleRT))
dlow2$seconds<-dlow2$binsTime
dlow2<-filter(dlow2, binsTime<6)
plot(dlow2$seconds,dlow2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low mag; Gamble propensity; n =",toString(sum(dlow2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dlow2
#Plotting RTs with sd
plot(dlow2$seconds,dlow2$medianRT,xlim = c(0,8),ylim=c(200,1500),main=paste("Low mag; median RT with sd; n =",toString(sum(dlow2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#arrows(dlow2$seconds[1],dlow2[1,6]+dlow2[1,7],dlow2$seconds[1],dlow[1,6]-dlow2[1,7])
for(i in 1:length(dlow2$seconds)){
  arrows(as.numeric(dlow2$seconds[i]),as.numeric(dlow2[i,6]+(as.numeric(dlow2[i,7])/5)),as.numeric(dlow2$seconds[i]),as.numeric(dlow2[i,6]-(as.numeric(dlow2[i,7])/5)),length=0.05, angle=90, code=3)
  
}

#Printing off Reaction time histograms by bin
a<-filter(dlow,binsTime<5.75)
for(i in sort(unique(dlow$binsTime))){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("Low mag: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
  print(i)
}




#Preference for mid gambles 
dmid<-filter(dgamble,standardGamble==4|standardGamble==3)
dBehavioral<-dmid %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,25),xlim=c(-5,100),main=paste("Propensity to gamble on mid-mag gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dmid$gambleRT,0),breaks=50,main="Reaction Time for mid mag gambles",xlab="Reaction time")

dmid2<-dmid %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dmid2$seconds<-dmid2$binsTime

plot(dmid2$seconds,dmid2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Mid mag; Gamble propensity; n =",toString(sum(dmid2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dmid2
#Plotting RTs with sd
plot(dmid2$seconds,dmid2$medianRT,xlim = c(0,8),ylim=c(0,1500),main=paste("Mid mag; median RT with sd; n =",toString(sum(dmid2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dmid2$seconds)){
  arrows(as.numeric(dmid2$seconds[i]),as.numeric(dmid2[i,6]+(as.numeric(dmid2[i,7])/5)),as.numeric(dmid2$seconds[i]),as.numeric(dmid2[i,6]-(as.numeric(dmid2[i,7])/5)),length=0.05, angle=90, code=3)
  
}

#Printing off Reaction time histograms by bin
a<-filter(dmid,binsTime<5.75)
for(i in sort(unique(dmid$binsTime))){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("Mid mag: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
  print(i)
}






#Preference for high gambles 
dhigh<-filter(dgamble,standardGamble==5|standardGamble==6)
dBehavioral<-dhigh %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,25),xlim=c(-5,100),main=paste("Propensity to gamble on high-mag gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dhigh$gambleRT,0),breaks=50,main="Reaction Time for high mag gambles",xlab="Reaction time")

dhigh2<-dhigh %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dhigh2$seconds<-dhigh2$binsTime
plot(dhigh2$seconds,dhigh2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("High mag; Gamble propensity; n =",toString(sum(dhigh2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dhigh2
#Plotting RTs with sd
plot(dhigh2$seconds,dhigh2$medianRT,xlim = c(0,8),ylim=c(0,1000),main=paste("High mag; median RT with sd; n =",toString(sum(dhigh2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dhigh2$seconds)){
  arrows(as.numeric(dhigh2$seconds[i]),as.numeric(dhigh2[i,6]+(as.numeric(dhigh2[i,7])/5)),as.numeric(dhigh2$seconds[i]),as.numeric(dhigh2[i,6]-(as.numeric(dhigh2[i,7])/5)),length=0.05, angle=90, code=3)
  
}

#Printing off Reaction time histograms by bin
a<-filter(dhigh,binsTime<5.75)
for(i in sort(unique(dhigh$binsTime))){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("High mag: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
  print(i)
}





#How does payout affect gamble propensity
#Low payout = 1.5 x standard gamble
#Mid payout = 2 x standard gamble
#High payout = 3 x standard gamble






# Low payout
dlowp<-filter(dgamble,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
dBehavioralLowp<-dlowp %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioralLowp
histogram(dBehavioralLowp$percentageGambled,breaks=50,ylim=c(0,70),xlim=c(-5,100),main=paste("Propensity to gamble on low payout gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dlowp$gambleRT,0),breaks=50,main="Reaction Time for low odds gambles",xlab="Reaction time")

dlowp2<-dlowp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dlowp2$seconds<-dlowp2$binsTime
plot(dlowp2$seconds,dlowp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low odds; Gamble propensity; n =",toString(sum(dlowp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dlowp2
#Plotting RTs with sd
plot(dlowp2$seconds,dlowp2$medianRT,xlim = c(0,8),ylim=c(0,1000),main=paste("Low Odds; median RT with sd; n =",toString(sum(dlowp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dlowp2$seconds)){
  arrows(as.numeric(dlowp2$seconds[i]),as.numeric(dlowp2[i,6]+(as.numeric(dlowp2[i,7])/5)),as.numeric(dlowp2$seconds[i]),as.numeric(dlowp2[i,6]-(as.numeric(dlowp2[i,7])/5)),length=0.05, angle=90, code=3)
  
}

#Printing off Reaction time histograms by bin
a<-filter(dlowp,binsTime<5.75)
c<-sort(unique(dlowp$binsTime))[c(TRUE, FALSE)]

for(i in c){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("Low odds: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
  print(i)
}






# Mid payout
dmidp<-filter(dgamble,Trialid==1|Trialid==2|Trialid==3|Trialid==4|Trialid==5|Trialid==6)

dBehavioral<-dmidp %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,70),xlim=c(-5,100),main=paste("Propensity to gamble on mid odds gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dmidp$gambleRT,0),breaks=50,main="Reaction Time for mid odds gambles",xlab="Reaction time")

dmidp2<-dmidp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dmidp2$seconds<-dmidp2$binsTime
plot(dmidp2$seconds,dmidp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Mid odds; Gamble propensity; n =",toString(sum(dmidp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dmidp2
#Plotting RTs with sd
plot(dmidp2$seconds,dmidp2$medianRT,xlim = c(0,8),ylim=c(0,1000),main=paste("Mid Odds; median RT with sd; n =",toString(sum(dmidp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dmidp2$seconds)){
  arrows(as.numeric(dmidp2$seconds[i]),as.numeric(dmidp2[i,6]+(as.numeric(dmidp2[i,7])/5)),as.numeric(dmidp2$seconds[i]),as.numeric(dmidp2[i,6]-(as.numeric(dmidp2[i,7])/5)),length=0.05, angle=90, code=3)
  
}
#Printing off Reaction time histograms by bin
a<-filter(dmidp,binsTime<5.75)
for(i in sort(unique(dmidp$binsTime))){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("Mid odds: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
  print(i)
}










# High payout
dhighp<-filter(dgamble,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)

dBehavioral<-dhighp %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime),
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioral
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,70),xlim=c(-5,100),main=paste("Propensity to gamble on high payout gambles; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dhighp$gambleRT,0),breaks=50,main="Reaction Time for high odds gambles",xlab="Reaction time")

dhighp2<-dhighp %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=round(mean(gambleRT)),
            sdRT=sd(gambleRT))
dhighp2$seconds<-dhighp2$binsTime
plot(dhighp2$seconds,dhighp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("High odds; Gamble propensity; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dhighp2
#Plotting RTs with sd
plot(dhighp2$seconds,dhighp2$medianRT,xlim = c(0,8),ylim=c(200,1500),main=paste("High Odds; Reaction time with sd; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(dhighp2$seconds)){
  arrows(as.numeric(dhighp2$seconds[i]),as.numeric(dhighp2[i,6]+(as.numeric(dhighp2[i,7])/5)),as.numeric(dhighp2$seconds[i]),as.numeric(dhighp2[i,6]-(as.numeric(dhighp2[i,7])/5)),length=0.05, angle=90, code=3)
  
}

#Printing off Reaction time histograms by bin
a<-filter(dhighp,binsTime<5.75)
c<-sort(unique(dhighp$binsTime))[c(TRUE, FALSE)]

for(i in c){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("High odds: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
  print(i)
}









#Overall plot of gambleDelay and percentage gambled
d2<-d %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            meanRT=mean(gambleRT),
            sdRT=sd(gambleRT))
d2$seconds<-d2$binsTime

#Number of trials per bin - this is a check to confirm that bin sizes are reasonable
plot(d2$seconds,d2$trials,xlim = c(-.5,8),
     main=paste("Number of trials per bin; n =",toString(sum(d2$trials)),"trials;",nParticipants,"participants"),
     xlab="Seconds into trial",ylab="Number of trials",pch=19)





#Overall regression of p(gambled) as gambles interrupted trial
d2<-filter(d2,seconds>0)

plot(d2$seconds,d2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Propensity to gamble vs. gamble interruption; n =",toString(sum(d2$trials)),"trials;",nParticipants,"participants"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)


m1<-lm(d2$percentageGambled~d2$seconds)
abline(m1)
summary(m1)

#Plotting RTs with sd
plot(d2$seconds,d2$meanRT,xlim = c(0,8),ylim=c(0,1000),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
for(i in 1:length(d2$seconds)){
  arrows(as.numeric(d2$seconds[i]),as.numeric(d2[i,6]+(as.numeric(d2[i,7])/5)),as.numeric(d2$seconds[i]),as.numeric(d2[i,6]-(as.numeric(d2[i,7])/5)),length=0.05, angle=90, code=3)
  
}
arrows(as.numeric(d2$seconds[1]),as.numeric(d2[1,6]+(as.numeric(d2[1,7])/5)),as.numeric(d2$seconds[1]),as.numeric(d2[1,6]-(as.numeric(d2[1,7])/5)),length=0.05, angle=90, code=3)
histogram(dgamble$gambleRT,breaks=50,xlim=c(25,2500))


#Number of trials per participant 
dTrials<-d %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/ntrials*100))
dTrials
histogram(dTrials$ntrials,breaks=50,xlim=c(0,120),ylim=c(0,10),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")


###Repeat regression plot but only WITHIN each participant

#Setting up definitions so we can break down each participant per cond. mag and odds

#The following is to just get one participant
dsub<-d[d$uniqueid==8,]
d3<-dsub %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d3$seconds<-d3$binsTime-.5
d3<-filter(d3,seconds>-.5)
d3
plot(d3$seconds,d3$percentageGambled,xlim = c(0,7),ylim = c(0,100),
     main=paste("Individual participant data; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Percentage Gambled")
mtemp<-lm(d3$percentageGambled~d3$seconds)
summary(mtemp)
#Looping through participants data

subj<-unique(d$uniqueid)
intN<-NULL
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
  d4<-filter(d4,seconds>0)
  plot(d4$seconds,d4$percentageGambled,xlim = c(0,7),ylim = c(0,100),
       main=paste("Individual participant data; n =",toString(sum(d4$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
       xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
  mtemp<-lm(d4$percentageGambled~d4$seconds)
  #This checks to see if any participant is ramping
  if(summary(mtemp)$coefficients[8]<.05 & summary(mtemp)$coefficients[2]>0){
    intN<-c(intN,i)
  }
}

#Printing off Reaction time histograms by bin
a<-filter(dgamble,binsTime<5.75)
for(i in sort(unique(dgamble$binsTime))){
  atemp<-filter(a,binsTime==i&gambleRT!=0)
  print(head(atemp))
  print(histogram(atemp$gambleRT,breaks=50,main=paste("RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))#,"Number of trials:",toString(length(atemp$gambleRT)))))
  print(i)
}

#Looking into participants that ramp
for(i in intN){
}
