###Ramp analysis; instrumental after rewrite of progress bar
###Rewritten/organized 3.23.2019


#Things to do

##Loading packages
#install.packages('mosaic')
#install.packages('plotrix')
#install.packages('plotrix', dependencies=TRUE, repos='http://cran.rstudio.com/')

require(mosaic)
require(plotrix)
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/reg_fns.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/gamblePlotFun.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/gambleRtPlotFun.R")

#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/ignorePlotFun.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/ignoreRtPlotFun.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/oddsScoreMeanFun.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/oddsScoreEbFun.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/totalPlotFun.R")
#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/rtPlotFun.R")

#source("/Users/Guillaume/Documents/GitHub/rampAnalysis/correctRThist.R")

##Loading data
#d0<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")
#d0<-read.csv(file="C:/Users/gpagn/Documents/GitHub/rampAnalysis/instrumentalRamp50.csv",sep=",")
d0<-read.csv(file="/Users/Guillaume/Documents/GitHub/rampAnalysis/instrumentalRamp50.csv",sep=",")

#Cleaning data
d0<-d0[130:length(d0$viewTime),]
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
write.csv(bonusAmounts,'bonusAmountsInstrumental50.csv')

#If you want to see survey results
dsurvey<-d0 %>% 
  group_by(uniqueid) %>% 
  summarise(engagement=unique(engagement)[2],
            difficulty=unique(difficulty)[2],
            length=unique(length)[2],
            design=unique(design)[2],
            gender=unique(gender)[2],
            interest=unique(interest)[2],
            understand=unique(understand)[2],
            comments=unique(comments)[2])
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
#comments
dsurvey$comments


#Need to replace uniqueid with numbers and clean out columns that aren't useful
d<-d0[,c("Trialid","gambleDelay","gambleRT","outcomeRT","response","standardGamble","trialNumber","uniqueid","recordedPresses","recordedTimes","switchTimes")]
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
seed=101
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

#For 4 bins #Use this for RM NEW
a1head<-.5
a1tail<-1.25
a2head<-1.25001
a2tail<-2
a3head<-2.00001
a3tail<-2.75
a4head<-2.75001
a4tail<-3.5
a5head<-3.50001
a5tail<-4.25

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
  else if (d[row,'gambleDelay']>=a5head&d[row,'gambleDelay']<=a5tail)
  {return(mean(c(a5head,a5tail)))}
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
d$outcomeRTz<-NULL

#Length of totally unfiltered participants (minus debug)
Participants<-unique(d$uniqueid)
length(Participants)
for (i in Participants){
  dsub<-filter(d,uniqueid==i)
  print(i)
  #if(length(dsub$gambled)==133){
  #  acfdf<-rbind(acfdf,dsub$gambled)
  #}
}

#Adding vector to filter out fast RTers
fastRTers<-NULL
fewTrials<-NULL
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
  
  if(sum(dsub$response=='failOutcome'||dsub$response=='gambleFail')>30){
    failures<-c(failures,unique(dsub$uniqueid))
    next()
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
  print(i)
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
  meanOutcomeZ=mean(dsub$outcomeRT[dsub$outcomeRT!=0])
  sdOutcomeZ=sd(dsub$outcomeRT[dsub$outcomeRT!=0])
  
  for(row in 1:length(dsub$Trialid)){
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

#If there were any trials were they gambled and it overlapped a little bit into the outcome, the outcome RT gets messed up
nrow(d[which(d$outcomeRT>1500),])

#########################################################################################################################################
#This should be ready to run now without any artificial filtering of participants for low RT etc.

nParticipants<- length(unique(d$uniqueid))
nParticipants

###Behavioral analyses
##Reaction time
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
hist(gambleRTs,main="Aggregated gamble RTs",breaks=70,xlim=c(0,1600))

#Outcome of whenever they claimed 'boring' reward and didn't gamble
successOutcomeRTs<-d$outcomeRT[d$response=='success']
hist(successOutcomeRTs,main=c("Aggregated outcome RTs; number of responses:",length(successOutcomeRTs)),breaks=70)

#Outcome of whenever they gambled and confirmed
#Outcome of whenever they claimed 'boring' reward
gambleOutcomeRTs<-d$outcomeRT[d$response=='gamble']
hist(gambleOutcomeRTs,main=c("Aggregated outcome RTs; number of responses:",length(gambleOutcomeRTs)),breaks=70)


#Removing participants who gambled too much/not enough
dgamble0<-filter(d,gambleDelay!=0,Trialid!=75|86)

dBehavioralTotal<-dgamble0 %>% 
  group_by(uniqueid) %>% 
  summarise(trials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=="success"),
            percentageGambled=round(gambleCount/trials*100),
            failedTrials=sum(response=='gambleFail'|response=='failOutcome'))
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

#By GambleDelay
d2<-filter(dgamble) %>% 
  group_by(binsTime) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='gambleFail'|response=='failOutcome'),
            percentageGambled=round(gambleCount/ntrials*100))

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
