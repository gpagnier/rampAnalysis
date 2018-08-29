install.packages('mosaic')
library(mosaic)

#Warning! CSV needs to be in exact column order:
#"trialid" #"expTime" "gambleDelay" "gambleRT" "outcomeRT" "response" "standardGamble" "trialNumber" "uniqueid"

#Up until August 2018, used ramp69.csv for data

d<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/rampV02DataClean.csv",sep=",")
#d0<-read.csv(file.choose(),header=TRUE)
d<-d0
d[d==""] <- NA
d$gambleDelay<-d$gambleDelay/1000
d$binsTime=0;
#Clearing pictures
graphics.off()

#Filtering participants that have weird bins i.e. too many 5.6
removeIds=c(201:227)
for(i in removeIds){
  d<-d[!(d$uniqueid==i),]
}



#Getting rid of any trial which gamble interrupted too early
#d<-filter(d,gambleDelay<5.6)
histogram(d$gambleDelay,breaks=50,xlim=c(0,7),main="Where gambles interrupted trials. All trials",xlab="seconds into trial gamble appeared")
#histogram(d$gambleDelay,breaks=100,xlim=c(.5,6),ylim=c(0,5.2),main="Where gambles interrupted trials. All trials",xlab="seconds into trial gamble appeared")


d=d[d$uniqueid>227,]
unique(d$uniqueid)





colnames(d)[1]<-"Trialid"
<<<<<<< HEAD
bins=9
=======
bins=8
>>>>>>> eb0d0837da006b0c1cb5fbe74d42ca0fc819cb77
ninbins=round((length(d$Trialid)/bins))
##Splitting into number of bins you want
#quantile(d$gambleDelay[!0])
#This creates a list with what should be in the binTimeCalc
delayValues=split(sort(d$gambleDelay[d$gambleDelay!=0]), ceiling(seq_along(d$gambleDelay[d$gambleDelay!=0])/ninbins))

#This is to manually hardcode the NUMBER of blocks you're using - has to match number of 'blocks' in delayValues
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
  else if (d[row,3]>=a7head&d[row,3]<=max(d$gambleDelay))
  {return(mean(c(a7head,a7tail)))}
<<<<<<< HEAD
  else if (d[row,3]>=a8head&d[row,3]<=a8tail)
  {return(mean(c(a8head,a8tail)))}
  else if (d[row,3]>=a9head&d[row,3]<=a9tail)
  {return(mean(c(a9head,a9tail)))}
=======
>>>>>>> eb0d0837da006b0c1cb5fbe74d42ca0fc819cb77
  else
  {return(999)}
  
}
#Temp code because one of the bins is to big... during V02 analysis
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
  else if (d[row,3]>=a5head&d[row,3]<=5.5999999)
  {return(mean(c(a5head,a5tail)))}
  else if (d[row,3]==5.6)
  {return(5.6)}
  else if (d[row,3]>5.6&d[row,3]<5.977)
  {return(mean(c(a7head,a8tail)))}
  else if (d[row,3]>=a9head&d[row,3]<=a9tail)
  {return(mean(c(a9head,a9tail)))}
  else
  {return(999)}
  
}

# 
# binTimeCalc<-function(row){
#   if(d[row,3]==0){
#     return(0)
#   } else if (d[row,3]>0&d[row,3]<1){
#     return(.5)
#   } else if (d[row,3]>1&d[row,3]<2.5){
#     return(1.75)
#   } else if (d[row,3]>2.5&d[row,3]<3){
#     return(2.75)  
#   } else if (d[row,3]>3&d[row,3]<3.4){
#     return(3.2)  
#   } else if (d[row,3]>3.4&d[row,3]<4.75){
#     return(4.1)
#   } else if (d[row,3]>4.75&d[row,3]<5.25){
#     return(5.1) 
#   } else if (d[row,3]>5.25&d[row,3]<5.6){
#     return(5.5) 
#   } else{
#     return(5.5)
#   }
# }
# 
# #For new data set
# binTimeCalc<-function(d,row){
#   if(d[row,3]==0){
#     return(0)
#   } else if (d[row,3]>0&d[row,3]<1){
#     return(.8)
#   } else if (d[row,3]>1&d[row,3]<1.5){
#     return(1.2)
#   } else if (d[row,3]>1.5&d[row,3]<2.5){
#     return(3)  
#   } else if (d[row,3]>2.5&d[row,3]<3){
#     return(2.8)
#   } else if (d[row,3]>3&d[row,3]<3.5){
#     return(3.2) 
#   } else if (d[row,3]>3.5&d[row,3]<4.5){
#     return(4)
#   } else if (d[row,3]>4.5&d[row,3]<4.8){
#     return(4.7) 
#   } else if (d[row,3]>4.8&d[row,3]<5){
#     return(4.9) 
#   } else if (d[row,3]>5&d[row,3]<5.2){
#     return(5.1)
#   } else if (d[row,3]>5.2&d[row,3]<5.8){
#     return(5.3) 
#   } else{
#     return(999)
#   }
# }


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

#Creating new df to see how many ended up in each bin
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

#Adding prediction errors as possible variable
d2=d[0,]
d[,13]=0
colnames(d)[13]<-"PredictionError"

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
d=d2


head(d)
participants<-unique(d$uniqueid)
nParticipants<- length(unique(d$uniqueid))
nParticipants

#Check for catch trials
dcatch<-filter(d,Trialid==75|Trialid==86)[,c(1,6,9)]
dcatch[order(dcatch$Trialid),]
#This prints off the trial ids of anyone who made a mistake
#75 should gamble; 86 should success/fail; 2 catch trials?


catch<-for(i in 1:nrow(dcatch)){
  if(dcatch[i,1]==85&dcatch[i,2]=="gamble"){
    noCatch<-c(noCatch,dcatch[i,3])
  }else if(dcatch[i,1]==75&dcatch[i,2]=="success"){
    noCatch<-c(noCatch,dcatch[i,3])
  }else if(dcatch[i,1]==75&dcatch[i,2]=="fail"){
    noCatch<-c(noCatch,dcatch[i,3])
  }
}


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
histogram(gambleRTs,main="Aggregated gamble RTs",breaks=70)
#Whenever they claimed 'boring' reward
outcomeRTs<-d$outcomeRT[d$outcomeRT!=0]
histogram(outcomeRTs,main="Aggregated outcome RTs",breaks=70)
#Printing experiment times in minutes CONFIRM PSITURK TRACKS IN MILLISECONDS
expTimes<-((unique(d$expTime,na.rm=TRUE)/1000)/60)
histogram(expTimes,main="Experiment Time (in minutes)",breaks=50)
histogram(expTimes,main="Experiment Time (in minutes)",breaks=50,xlim=c(0,60))


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

##Removing 4 since there aren't enough trials in the block to consider
#d<-filter(d,binsTime!=4|binsTime!=2)

#Adding prediction error...again
d[,13]=0
colnames(d)[13]="PredictionError"
d2=d[0,]

for (i in Participants){
  dsub=d[d$uniqueid==i,]
  for (row in 2:length(dsub$Trialid)){
    dsub[row,13]=(dsub[row,7]-dsub[(row-1),7])
  }
  d2=rbind(d2,dsub)
}
d=d2


#Always run the following block2
#This filters only trials that had a gamble appear

dgamble<-filter(d,gambleDelay!=0)

dBehavioral<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[2],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))

histogram(dBehavioral$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity to gamble; n =",toString(sum(dBehavioralTotal$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")

#Adding another column 1 if they gambled, 0 if they didn't
#This will be outcome variable in logistic regression
for (i in 1:length(dgamble$response)){
  if(dgamble$response[i]=='gamble'){
    dgamble$gambled[i]=1
  } else {
    dgamble$gambled[i]=0
  }
}



<<<<<<< HEAD


=======
>>>>>>> eb0d0837da006b0c1cb5fbe74d42ca0fc819cb77
#Logistic regression for gambled
mlog<-glm(gambled~gambleDelay+magCond+oddsCond+trialNumber+PredictionError,data=dgamble,family="binomial");
summary(mlog)



#####Breaking down sub conditions
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
plot(dlow2$seconds,dlow2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low mag; Gamble propensity; n =",toString(sum(dlow2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dlow2
#Plotting RTs with sd
dlowRT<-filter(dlow,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dlowRT$seconds<-dlowRT$binsTime



plot(dlowRT$seconds,dlowRT$medianRT,xlim = c(0,8),ylim=c(200,2500),main=paste("Low mag; median RT with sd; n =",toString(sum(dlow2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#arrows(dlow2$seconds[1],dlow2[1,6]+dlow2[1,7],dlow2$seconds[1],dlow[1,6]-dlow2[1,7])
#for(i in 1:length(dlow2$seconds)){
#  arrows(as.numeric(dlow2$seconds[i]),as.numeric(dlow2[i,6]+(as.numeric(dlow2[i,7])/5)),as.numeric(dlow2$seconds[i]),as.numeric(dlow2[i,6]-(as.numeric(dlow2[i,7])/5)),length=0.05, angle=90, code=3)

#}

#Printing off Reaction time histograms by bin
#a<-filter(dlow,binsTime<5.75)
#for(i in sort(unique(dlow$binsTime))){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("Low mag: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
#  print(i)
#}




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
dmidRT<-filter(dmid,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dmidRT$seconds<-dmidRT$binsTime




plot(dmidRT$seconds,dmidRT$medianRT,xlim = c(0,8),ylim=c(0,2500),main=paste("Mid mag; median RT with sd; n =",toString(sum(dmid2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dmid2$seconds)){
#  arrows(as.numeric(dmid2$seconds[i]),as.numeric(dmid2[i,6]+(as.numeric(dmid2[i,7])/5)),as.numeric(dmid2$seconds[i]),as.numeric(dmid2[i,6]-(as.numeric(dmid2[i,7])/5)),length=0.05, angle=90, code=3)

#}

#Printing off Reaction time histograms by bin
#a<-filter(dmid,binsTime<5.75)
#for(i in sort(unique(dmid$binsTime))){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("Mid mag: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
#  print(i)
#}






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
dhighRT<-filter(dhigh,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dhighRT$seconds<-dhighRT$binsTime



plot(dhighRT$seconds,dhighRT$medianRT,xlim = c(0,8),ylim=c(0,2500),main=paste("High mag; median RT with sd; n =",toString(sum(dhigh2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dhigh2$seconds)){
#  arrows(as.numeric(dhigh2$seconds[i]),as.numeric(dhigh2[i,6]+(as.numeric(dhigh2[i,7])/5)),as.numeric(dhigh2$seconds[i]),as.numeric(dhigh2[i,6]-(as.numeric(dhigh2[i,7])/5)),length=0.05, angle=90, code=3)

#}

#Printing off Reaction time histograms by bin
#a<-filter(dhigh,binsTime<5.75)
#for(i in sort(unique(dhigh$binsTime))){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("High mag: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
#  print(i)
#}





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
            percentageGambled=round(gambleCount/trials*100)
  )
dlowp2$seconds<-dlowp2$binsTime
plot(dlowp2$seconds,dlowp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low odds; Gamble propensity; n =",toString(sum(dlowp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dlowp2
#Plotting RTs with sd
dlowRT<-filter(dlowp,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dlowRT$seconds<-dlowRT$binsTime


plot(dlowRT$seconds,dlowRT$medianRT,xlim = c(0,8),ylim=c(0,2500),main=paste("Low Odds; median RT with sd; n =",toString(sum(dlowp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dlowp2$seconds)){
#  arrows(as.numeric(dlowp2$seconds[i]),as.numeric(dlowp2[i,6]+(as.numeric(dlowp2[i,7])/5)),as.numeric(dlowp2$seconds[i]),as.numeric(dlowp2[i,6]-(as.numeric(dlowp2[i,7])/5)),length=0.05, angle=90, code=3)
#  
#}

#Printing off Reaction time histograms by bin
#a<-filter(dlowp,binsTime<5.75)
#c<-sort(unique(dlowp$binsTime))[c(TRUE, FALSE)]

#for(i in c){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("Low odds: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
#  print(i)
#}






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
            percentageGambled=round(gambleCount/trials*100)
  )
dmidp2$seconds<-dmidp2$binsTime
plot(dmidp2$seconds,dmidp2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Mid odds; Gamble propensity; n =",toString(sum(dmidp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dmidp2
#Plotting RTs with sd
dmidRT<-filter(dmidp,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dmidRT$seconds<-dmidRT$binsTime

plot(dmidRT$seconds,dmidRT$medianRT,xlim = c(0,8),ylim=c(0,2500),main=paste("Mid Odds; median RT with sd; n =",toString(sum(dmidp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dmidp2$seconds)){
#  arrows(as.numeric(dmidp2$seconds[i]),as.numeric(dmidp2[i,6]+(as.numeric(dmidp2[i,7])/5)),as.numeric(dmidp2$seconds[i]),as.numeric(dmidp2[i,6]-(as.numeric(dmidp2[i,7])/5)),length=0.05, angle=90, code=3)

#}
#Printing off Reaction time histograms by bin
#a<-filter(dmidp,binsTime<5.75)
#for(i in sort(unique(dmidp$binsTime))){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("Mid odds: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
#  print(i)
#}










# High payout
dhighp<-filter(dgamble,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)

dBehavioral<-dhighp %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[1],
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
dhighRT<-filter(dhighp,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dhighRT$seconds<-dhighRT$binsTime




plot(dhighRT$seconds,dhighRT$medianRT,xlim = c(0,8),ylim=c(200,2500),main=paste("High Odds; Reaction time with sd; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
#for(i in 1:length(dhighp2$seconds)){
#  arrows(as.numeric(dhighp2$seconds[i]),as.numeric(dhighp2[i,6]+(as.numeric(dhighp2[i,7])/5)),as.numeric(dhighp2$seconds[i]),as.numeric(dhighp2[i,6]-(as.numeric(dhighp2[i,7])/5)),length=0.05, angle=90, code=3)
#  
#}

#Printing off Reaction time histograms by bin
#a<-filter(dhighp,binsTime<5.75)
#c<-sort(unique(dhighp$binsTime))[c(TRUE, FALSE)]

#for(i in c){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("High odds: RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))
#  print(i)
#}



<<<<<<< HEAD
#High PredictionError only
dhighPE<-filter(dgamble,PredictionError>0)

dBehavioral<-dhighPE %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[1],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioral
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,70),xlim=c(-5,100),main=paste("Propensity to gamble on high RPEs; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dhighPE$gambleRT,0),breaks=50,main="Reaction Time for high RPEs",xlab="Reaction time")

dhighPE2<-dhighPE %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=round(mean(gambleRT)),
            sdRT=sd(gambleRT))
dhighPE2$seconds<-dhighPE2$binsTime
plot(dhighPE2$seconds,dhighPE2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("High RPE; Gamble propensity; n =",toString(sum(dhighPE2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dhighPE2
#Plotting RTs with sd
dhighPERT<-filter(dhighPE,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dhighPERT$seconds<-dhighPERT$binsTime




plot(dhighPERT$seconds,dhighPERT$medianRT,xlim = c(0,8),ylim=c(200,2500),main=paste("High RPE; Reaction time with sd; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)


#Low PredictionError only
dlowPE<-filter(dgamble,PredictionError<0)

dBehavioral<-dlowPE %>% 
  group_by(uniqueid) %>% 
  summarise(experimentTime=unique(expTime)[1],
            trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
dBehavioral
histogram(dBehavioral$percentageGambled,breaks=50,ylim=c(0,70),xlim=c(-5,100),main=paste("Propensity to gamble on low RPEs; n =",toString(sum(dBehavioral$trials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled")
histogram(setdiff(dlowPE$gambleRT,0),breaks=50,main="Reaction Time for low RPEs",xlab="Reaction time")

dlowPE2<-dlowPE %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            medianRT=round(mean(gambleRT)),
            sdRT=sd(gambleRT))
dlowPE2$seconds<-dlowPE2$binsTime
plot(dlowPE2$seconds,dlowPE2$percentageGambled,xlim = c(0,8),ylim = c(0,100),
     main=paste("Low RPE; Gamble propensity; n =",toString(sum(dlowPE2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
dlowPE2
#Plotting RTs with sd
dlowPERT<-filter(dlowPE,gambleRT!=0) %>%
  group_by(binsTime) %>%
  summarise(medianRT=mean(gambleRT),
            sdRT=sd(gambleRT))
dlowPERT$seconds<-dlowPERT$binsTime




plot(dlowPERT$seconds,dlowPERT$medianRT,xlim = c(0,8),ylim=c(200,2500),main=paste("Low RPE; Reaction time with sd; n =",toString(sum(dhighp2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)



=======
>>>>>>> eb0d0837da006b0c1cb5fbe74d42ca0fc819cb77
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
d2RT<-filter(d,gambleRT!=0) %>% 
  group_by(binsTime) %>% 
  summarise(meanRT=mean(gambleRT),
            sdRT=sd(gambleRT))

d2RT$seconds<-d2RT$binsTime


plot(d2RT$seconds,d2RT$meanRT,xlim = c(0,8),ylim=c(0,2500),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d2$trials)),"trials;"),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)

mRT<-lm(d2RT$meanRT~d2RT$seconds)
abline(mRT)
summary(mRT)
for(i in 1:length(d2RT$seconds)){
  arrows(as.numeric(d2RT$seconds[i]),as.numeric(d2RT[i,6]+(as.numeric(d2RT[i,7])/5)),as.numeric(d2RT$seconds[i]),as.numeric(d2RT[i,6]-(as.numeric(d2RT[i,7])/5)),length=0.05, angle=90, code=3)
  
}
#arrows(as.numeric(d2$seconds[1]),as.numeric(d2[1,6]+(as.numeric(d2[1,7])/5)),as.numeric(d2$seconds[1]),as.numeric(d2[1,6]-(as.numeric(d2[1,7])/5)),length=0.05, angle=90, code=3)
#histogram(dgamble$gambleRT,breaks=50,xlim=c(25,2500))


#Number of trials per participant 
dTrials<-d %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/ntrials*100))
dTrials
histogram(dTrials$ntrials,breaks=50,xlim=c(0,120),ylim=c(0,30),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")


###Repeat regression plot but only WITHIN each participant

#Setting up definitions so we can break down each participant per cond. mag and odds

####The following is to just get one participant's data
dsub<-d[d$uniqueid==6,]
d3<-dsub %>% 
  group_by(binsTime) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
d3$seconds<-d3$binsTime-.5
d3<-filter(d3,seconds>-.5)
d3
par(ps = 12, cex = 1, cex.main = 1)
plot(d3$seconds,d3$percentageGambled,xlim = c(0,7),ylim = c(0,100),
     main=paste("Individual participant data; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Percentage Gambled")
mtemp<-lm(d3$percentageGambled~d3$seconds)
summary(mtemp)
#One participant RT
d3RT<-filter(dsub,gambleRT!=0) %>% 
  group_by(binsTime) %>% 
  summarise(meanRT=mean(gambleRT),
            sdRT=sd(gambleRT))

d3RT$seconds<-d3RT$binsTime


plot(d3RT$seconds,d3RT$meanRT,xlim = c(0,8),ylim=c(0,2500),main=paste("All gambles; Reaction time with sd; n =",toString(sum(d3$trials)),"trials;","participant:",toString(unique(dsub$uniqueid))),
     xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)

msubRT<-lm(d3RT$meanRT~d3RT$seconds)
abline(msubRT)
summary(msubRT)

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
  print(summary(mtemp)$coefficients[8])
  #This checks to see if any participant is ramping
  if(summary(mtemp)$coefficients[8]<.05 & summary(mtemp)$coefficients[2]>0){
    intN<-c(intN,i)
  }
}



#Printing off Reaction time histograms by bin
#a<-filter(dgamble,binsTime<5.75)
#for(i in sort(unique(dgamble$binsTime))){
#  atemp<-filter(a,binsTime==i&gambleRT!=0)
#  print(head(atemp))
#  print(histogram(atemp$gambleRT,breaks=50,main=paste("RT by interruption, Gamble interrupted around ~ ",toString(i)," seconds")))#,"Number of trials:",toString(length(atemp$gambleRT)))))
#  print(i)
#}

#Looking into participants that ramp
for(i in intN){
}
###Breaking down cond. individual participant (p)
p<-6

dsubMagCond<-filter(d,uniqueid==p|magCond!="catch") %>% 
  group_by(magCond) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
barplot(dsubMagCond$percentageGambled,names.arg=dsubMagCond$magCond,beside=T,ylim=c(0,50),ylab="Number of trials",main="Participant 6, propensity to gamble across magnitude")

dsubOddsCond<-filter(d,uniqueid==p|oddsCond!="catch") %>% 
  group_by(oddsCond) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            didNotGamble=sum(response=="fail"|response=="success"),
            percentageGambled=round(gambleCount/trials*100))
barplot(dsubOddsCond$percentageGambled,names.arg=dsubOddsCond$oddsCond,beside=T,ylim=c(0,100),ylab="Number of trials",main="Participant 6, propensity to gamble across odds")
