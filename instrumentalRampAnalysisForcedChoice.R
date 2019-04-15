###Ramp analysis; instrumental after rewrite of progress bar
###Rewritten/organized 3.23.2019


#Things to do
#unfactor, remove comma from d$switchTimes, d$recordedKeys, d$recordedPresses
#does switch cost decrease as it goes on
#Add new column of length of responseKeys (which should match length of responseTimes)
#Correlate pbGamble with gambleDelay - should be close to exact match
#

##Loading packages
#install.packages('mosaic')
#install.packages('plotrix')
#install.packages('plotrix', dependencies=TRUE, repos='http://cran.rstudio.com/')

require(mosaic)
require(plotrix)
require(lme4)
require(plotrix)
source("/Users/Guillaume/Documents/GitHub/rampAnalysis/gamblePlotFun.R")
source("/Users/Guillaume/Documents/GitHub/rampAnalysis/gambleRtPlotFun.R")
source("/Users/Guillaume/Documents/GitHub/rampAnalysis/oddsScoreMeanFun.R")
source("/Users/Guillaume/Documents/GitHub/rampAnalysis/oddsScoreEbFun.R")
source("/Users/Guillaume/Documents/GitHub/rampAnalysis/switchPlot.R")
source("/Users/Guillaume/Documents/GitHub/rampAnalysis/totalPlotFun")


##Loading data
#d0<-read.csv(file="C:/Users/lab/Documents/GitHub/rampAnalysis/Totalrampv02.csv",sep=",")
#d0<-read.csv(file="C:/Users/gpagn/Documents/GitHub/rampAnalysis/instrumentalRamp50.csv",sep=",")
d0<-read.csv(file="/Users/Guillaume/Documents/GitHub/rampAnalysis/instrumentalRampForcedChoice40.csv",sep=",")
d0<-read.csv(file="/Users/gpagn/Documents/GitHub/rampAnalysis/instrumentalRamp50.csv",sep=",")

#Cleaning data
d0<-d0[12050:length(d0$viewTime),]
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
d<-d0[,c("Trialid","gambleDelay","gambleRT","ignoreRT","outcomeRT","response","standardGamble","trialNumber","uniqueid","recordedPresses","recordedTimes","switchTimes","pbGamble")]
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
hist(d$gambleDelay,breaks=50,xlim=c(0,6),main="When did gambles interrupt the progress bar?",xlab="Seconds into trial gamble appeared",col='black',ylab="Total number of trials")
hist(d$pbGamble,xlim=c(1,100),breaks=50,main="When did gambles interrupt the progress bar?",xlab="Percentage of progress bar that was present",col='black',ylab="Total number of trials")

#pbgamble is a better indicator than gambleDelay
d<-d %>%
  mutate(gambleDelay = (pbGamble / 100)*5)

hist(d$gambleDelay,breaks=50,xlim=c(0,5),main="When did gambles interrupt the progress bar?",xlab="Seconds into trial gamble appeared",col='black',ylab="Total number of trials")

#For 12 bins #Use this for RM NEW V02
a1head<-.2
a1tail<-.7
a2head<-.75
a2tail<-1.2
a3head<-1.2001
a3tail<-1.55
a4head<-1.55001
a4tail<-2
a5head<-2.0001
a5tail<-2.5
a6head<-2.5001
a6tail<-3
a7head<-3.0001
a7tail<-3.520
a8head<-3.521
a8tail<-5

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
  else if (d[row,'gambleDelay']>=a6head&d[row,'gambleDelay']<=a6tail)
   {return(mean(c(a6head,a6tail)))}
   else if (d[row,'gambleDelay']>=a7head&d[row,'gambleDelay']<=a7tail)
   {return(mean(c(a7head,a7tail)))}
   else if (d[row,'gambleDelay']>=a8head&d[row,'gambleDelay']<=a8tail)
   {return(mean(c(a8head,a8tail)))}
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



###########################################################################################################################
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
d$baselineGambling=NULL

#RT z score
#Should I do z scores on speed or raw RT?
d$outcomeRTz<-NULL

#Length of totally unfiltered participants (minus debug)
Participants<-unique(d$uniqueid)
length(Participants)

#Adding vector to filter out fast RTers
fastRTers<-NULL
fewTrials<-NULL
noSuccess<-NULL
noGamble<-NULL
noIgnore<-NULL
failures<-NULL
#Adding RPE as a factor AND normalized RT z score
#rpe1 is the sure thing of trial t- standard gamble of t-1
#rpe2 is the sure thing of t - the average of whatever was chosen in t-1
#rpe3 is the sure thing of t - whatever was chosen in t-1 but the highest gamble option instead of average

#dbackup<-d
#d<-dbackup
for (i in Participants){
  dsub<-filter(d,uniqueid==i)

  kickOut=0
  if(nrow(dsub)<50){
    fewTrials<-c(fewTrials,unique(dsub$uniqueid))
    next()
  }
  
  if(sum(dsub$response=='failOutcome'||dsub$response=='gambleFail')>40){
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
  if(nrow(dsub[dsub$ignoreRT!=0,])<10){
    noIgnore<-c(noGamble,unique(dsub$uniqueid))
    kickOut=1
  }
  
  if(kickOut==1){
    next()
  }
  dsub[1,"rpe1"]=dsub[1,"standardGamble"]
  dsub[1,"rpe2"]=dsub[1,"standardGamble"]
  dsub[1,"rpe3"]=dsub[1,"standardGamble"]
  
  dsub$baselineGambling=(100/sum(dsub$gambleDelay!=0)*sum(dsub$response=='gamble'))
  
  
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
  
  
  #This is calculating normalized z score per dsub RT
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

#Checking for kicked out participants
print(paste("fastRTers (not kicked out):",length(fastRTers)))
print(paste("fewTrials (can not be associated with any other vector):",length(fewTrials)))

print(paste("noSuccess:",length(noSuccess)))
print(paste("noGamble:",length(noGamble)))
print(paste("noIgnore:",length(noIgnore)))
print(paste("failures:",length(failures)))

union(fastRTers,failures)

d=d2

#If there were any trials were they gambled and it overlapped a little bit into the outcome, the outcome RT gets messed up
nrow(d[which(d$outcomeRT>1500),])
for(i in which(d$outcomeRT>1500)){
  d[i,'outcomeRT']=1500
}

#Breaking up switchTimes, recordedPresses, and recorded, switchTimes into number vectors
listConvert<-function(elem){
  return(as.integer(unlist(strsplit(as.character(elem),","))[-1]))
}
  
d$switchTimes<-sapply(d$switchTimes,listConvert)
d$recordedTimes<-sapply(d$recordedTimes,listConvert)

d$recordedNumber<-NULL
for(i in 1:nrow(d)){
  d[i,'recordedNumber']=length(unlist(d[i,'recordedTimes']))
}

#########################################################################################################################################
#This should be ready to run now without any artificial filtering of participants for low RT etc.

nParticipants<- length(unique(d$uniqueid))
nParticipants

###Behavioral analyses
##Reaction time
#Whenever they gambled
gambleRTs<-d$gambleRT[d$gambleRT!=0]
hist(gambleRTs,main="Aggregated gamble RTs",breaks=70,xlim=c(0,2000))

#Whenever they ignored
ignoreRTs<-d$ignoreRT[d$ignoreRT!=0]
hist(ignoreRTs,main="Aggregated ignore RTs",breaks=70,xlim=c(0,2000))

#Outcome of whenever they claimed 'boring' reward and didn't gamble
successOutcomeRTs<-d$outcomeRT[d$response=='success']
hist(successOutcomeRTs,main=c("Aggregated outcome RTs when they ignored; number of responses:",length(successOutcomeRTs)),breaks=70)

#Outcome of whenever they gambled and confirmed
gambleOutcomeRTs<-d$outcomeRT[d$response=='gamble']
hist(gambleOutcomeRTs,main=c("Aggregated outcome RTs when they accepted gamble; number of responses:",length(gambleOutcomeRTs)),breaks=70)

#Comparison of gamble RT vs Ignore RT
boxplot(gambleRTs,ignoreRTs,at=c(1,2),names=(c("gamble RT","ignoreRT")),main="RT at gamble presentation: gamble vs ignore")
t.test(gambleRTs,ignoreRTs)

#Comparison of outcome RTs when gambled vs no gambled
boxplot(successOutcomeRTs,gambleOutcomeRTs,at=c(1,2),names=(c("outcome RT after ignoring","outcome RT after gambling")),main="outcomeRT at end of progress bar")
t.test(gambleOutcomeRTs,successOutcomeRTs)

#Number of gambled trials per participant
dgamble0<-filter(d,gambleDelay!=0,Trialid!=75|86) 
dTrials<-dgamble0 %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='outcomeFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))
head(dTrials)

#histrogram of individial gambling behavior
hist(dTrials$gambleCount,breaks=50,xlim=c(0,140),main=paste("Number of gambled trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")

#dTrials
hist(dTrials$ntrials,breaks=50,xlim=c(0,140),main=paste("Number of trials per participant; ",nParticipants,"participants"),xlab="Number of Trials per participant")

#Removing participants who gambled too much/not enough
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
length(c(noGamblers,allGamblers,lowTrials,fastRTers,failures))


removeIds<-c(noGamblers,allGamblers,lowTrials,fastRTers,failures)

nParticipants<- length(unique(d$uniqueid))
nParticipants



################################################################################################
#Statistics
#Resetting dgamble
dgamble<-filter(d,gambleDelay!=0,Trialid!=75,Trialid!=86)
#Check for any NA in any column
apply(dgamble, 2, function(x) any(is.na(x)))

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

#mlmerog<-glmer(gambled~scale(contOdds)+(scale(gambleDelay)+contOdds|uniqueid),
#               data=dgamble,family="binomial");
#summary(mlmerog)

#mlmerog1<-glmer(gambled~scale(contOdds)+scale(gambleDelay)+scale(contOdds):scale(gambleDelay)+
#                  (scale(gambleDelay)+scale(contOdds)+1|uniqueid),
#                data=dgamble,family="binomial");
#summary(mlmerog1)

#mlmerog3<-glmer(gambled~scale(contOdds)+scale(gambleDelay)+scale(contOdds):scale(gambleDelay)+
#                  (scale(gambleDelay)+scale(contOdds)+1|uniqueid),
#                data=dgamble,family="binomial");
#summary(mlmerog3)

mlmerog2<-glmer(gambled~scale(contOdds)+scale(gambleDelay)*scale(contOdds)+
                  scale(contMag)+scale(contMag):scale(gambleDelay)+
                  scale(trialNumber)+
                  scale(trialNumber):scale(gambleDelay)+
                  (scale(gambleDelay)+scale(contMag)+scale(trialNumber)+scale(trialNumber):scale(gambleDelay)+
                  scale(contOdds)+1|uniqueid),
                  data=dgamble,family="binomial");

summary(mlmerog2)

#Manual gamblePlot
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


#All data
gamblePlot(d,title="All data",ylim=c(30,50),orig=F,eb='sem',standardized=T)
gamblePlot(d,title="All data",ylim=c(30,50),orig=F,eb='sem',standardized=T)

gambleRtPlot(d,xlimit=c(0,5),ylimit=c(500,1250))
switchPlot(d,ylimit=c(0,1500))


#Expected Value sensitivity 
#EV sensitivity early mid and late within subject
compMeans<-c(oddsScoreMean(d,time='early'),oddsScoreMean(d,time='mid'),oddsScoreMean(d,time='late'))
compSds<-c(oddsScoreEb(d,type='sem',time='early'),oddsScoreEb(d,type='sem',time='mid'),oddsScoreEb(d,type='sem',time='late'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,60),
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; Within-session all data")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#EV sensitivity when did gamble interrupt: early mid and late 
compMeans<-c(oddsScoreMean(d,int='early'),oddsScoreMean(d,int='mid'),oddsScoreMean(d,int='late'))
compSds<-c(oddsScoreEb(d,type='sem',int='early'),oddsScoreEb(d,type='sem',int='mid'),oddsScoreEb(d,type='sem',int='late'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,60),
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; gambleInterruption all data")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

############################################################################################################
############################################################################################################
#Mag/odds condition
#Removing participants who gambled too much/not enough
#Low mag
dlow<-filter(d,magCond=='low',gambleDelay!=0,Trialid!=75,Trialid!=86)
dlowB<-dlow %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))

hist(dlowB$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,25),main=paste("Overall participant propensity(low mag only) to gamble; n =",toString(sum(dlowB$ntrials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')
gamblePlot(dlow,title="lowMag",ylim=c(0,50))
gambleRtPlot(dlow,xlimit=c(0,5),ylimit=c(700,1000))

hist(d$outcomeRT[d$response=='gamble'],col=rgb(1,0,0,0.5), main='RTs when accepting sure thing (after ignoring gamble)', xlab='RT',breaks=70,xlim=c(0,1300))
abline(v=median(d$outcomeRT[d$response=='gamble']),col="red",lwd=2)
hist(dlow$outcomeRT[dlow$response=='gamble'],col=rgb(0,0,1,0.5), add=T,breaks=70)
abline(v=median(dlow$outcomeRT[dlow$response=='gamble']),col="blue",lwd=2)
legend(800,55,cex=.7, bty = "n",legend=c("All trials","lowMag only"),col=c("red","blue"),title="",pch=16)

#Mid mag
dmid<-filter(d,magCond=='mid',gambleDelay!=0,Trialid!=75,Trialid!=86)
dmidB<-dmid %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))

hist(dmidB$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,35),main=paste("Overall participant propensity(mid mag only) to gamble; n =",toString(sum(dmidB$ntrials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')
gamblePlot(dmid,title="midMag",ylim=c(0,50))
gambleRtPlot(dmid,xlimit=c(0,5),ylimit=c(800,1200))

hist(d$outcomeRT[d$response=='gamble'],col=rgb(1,0,0,0.5), main='RTs when accepting sure thing (after ignoring gamble)', xlab='RT',breaks=70,xlim=c(0,1300))
abline(v=median(d$outcomeRT[d$response=='gamble']),col="red",lwd=2)
hist(dmid$outcomeRT[dmid$response=='gamble'],col=rgb(0,0,1,0.5), add=T,breaks=70)
abline(v=median(dmid$outcomeRT[dmid$response=='gamble']),col="blue",lwd=2)
legend(800,55,cex=.7, bty = "n",legend=c("All trials","midMag only"),col=c("red","blue"),title="",pch=16)

#High mag
dhigh<-filter(d,magCond=='high',gambleDelay!=0,Trialid!=75,Trialid!=86)
dhighB<-dhigh %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))

hist(dhighB$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,40),main=paste("Overall participant propensity(high mag only) to gamble; n =",toString(sum(dhighB$ntrials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')
gamblePlot(dhigh,title="highMag",ylim=c(0,50))
gambleRtPlot(dhigh,xlimit=c(0,5),ylimit=c(800,1200))

hist(d$outcomeRT[d$response=='gamble'],col=rgb(1,0,0,0.5), main='RTs when accepting sure thing (after ignoring gamble)', xlab='RT',breaks=70,xlim=c(0,1300))
abline(v=median(d$outcomeRT[d$response=='gamble']),col="red",lwd=2)
hist(dhigh$outcomeRT[dhigh$response=='gamble'],col=rgb(0,0,1,0.5), add=T,breaks=70)
abline(v=median(dhigh$outcomeRT[dhigh$response=='gamble']),col="blue",lwd=2)
legend(800,55,cex=.7, bty = "n",legend=c("All trials","highMag only"),col=c("red","blue"),title="",pch=16)


#Now value
#Low value
dlowp<-filter(d,oddsCond=='lowp',gambleDelay!=0,Trialid!=75,Trialid!=86)
dlowpB<-dlowp %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))

hist(dlowpB$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,50),main=paste("Overall participant propensity(low value only) to gamble; n =",toString(sum(dlowpB$ntrials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')
gamblePlot(dlowp,title="lowValue",ylim=c(10,40))
gambleRtPlot(dlowp,xlimit=c(0,5),ylimit=c(700,1000))

hist(d$outcomeRT[d$response=='gamble'],col=rgb(1,0,0,0.5), main='RTs when accepting sure thing (after ignoring gamble)', xlab='RT',breaks=70,xlim=c(0,1300))
abline(v=median(d$outcomeRT[d$response=='gamble']),col="red",lwd=2)
hist(dlowp$outcomeRT[dlowp$response=='gamble'],col=rgb(0,0,1,0.5), add=T,breaks=70)
abline(v=median(dlowp$outcomeRT[dlowp$response=='gamble']),col="blue",lwd=2)
legend(800,55,cex=.7, bty = "n",legend=c("All trials","lowValue only"),col=c("red","blue"),title="",pch=16)

#Mid value
dmidp<-filter(d,oddsCond=='midp',gambleDelay!=0,Trialid!=75,Trialid!=86)
dmidpB<-dmidp %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))

hist(dmidpB$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,50),main=paste("Overall participant propensity(mid value only) to gamble; n =",toString(sum(dmidpB$ntrials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')
gamblePlot(dmidp,title="midValue",ylim=c(30,50))
gambleRtPlot(dmidp,xlimit=c(0,5),ylimit=c(700,1000))

hist(d$outcomeRT[d$response=='gamble'],col=rgb(1,0,0,0.5), main='RTs when accepting sure thing (after ignoring gamble)', xlab='RT',breaks=70,xlim=c(0,1300))
abline(v=median(d$outcomeRT[d$response=='gamble']),col="red",lwd=2)
hist(dmidp$outcomeRT[dmidp$response=='gamble'],col=rgb(0,0,1,0.5), add=T,breaks=70)
abline(v=median(dmidp$outcomeRT[dmidp$response=='gamble']),col="blue",lwd=2)
legend(800,55,cex=.7, bty = "n",legend=c("All trials","midValue only"),col=c("red","blue"),title="",pch=16)

#High value
dhighp<-filter(d,oddsCond=='highp',gambleDelay!=0,Trialid!=75,Trialid!=86)
dhighpB<-dhighp %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))

hist(dhighpB$percentageGambled,breaks=50,xlim=c(-5,100),ylim=c(0,50),main=paste("Overall participant propensity(high value only) to gamble; n =",toString(sum(dhighpB$ntrials)),"trials;",nParticipants,"participants"),xlab="Percentage of time gambled",col='red')
gamblePlot(dhighp,title="highValue",ylim=c(50,70))
gambleRtPlot(dhighp,xlimit=c(0,5),ylimit=c(800,1200))

hist(d$outcomeRT[d$response=='gamble'],col=rgb(1,0,0,0.5), main='RTs when accepting sure thing (after ignoring gamble)', xlab='RT',breaks=70,xlim=c(0,1300))
abline(v=median(d$outcomeRT[d$response=='gamble']),col="red",lwd=2)
hist(dhighp$outcomeRT[dhighp$response=='gamble'],col=rgb(0,0,1,0.5), add=T,breaks=70)
abline(v=median(dhighp$outcomeRT[dhighp$response=='gamble']),col="blue",lwd=2)
legend(800,55,cex=.7, bty = "n",legend=c("All trials","highValue only"),col=c("red","blue"),title="",pch=16)


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

rtSlopes<-NULL
gambleSlopes<-NULL
run<-NULL
slopeDF<-NULL
EVdiffs<-NULL
plotRT=F
plotGD=F


#Add in knobs for different sub categories (though this number is very small....)

#Participants is default (all participants)
#If you want to run sub groups, go to bottom of script
for(i in subj){
  print(i)
  dsub<-d[d$uniqueid==i,]
  dsubgamble<-filter(dsub,gambleDelay!=0,Trialid!=75|86)
  dsubgambled<-filter(dsub,gambleDelay!=0,Trialid!=75|86,gambleRT!=0)
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

  if (plotRT){
    totalRTPlot(dsub,line=T,title=paste("Indiv graph; Participant# ",toString(unique(dsub$uniqueid))))
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
  
  #Making df of gambleSlopes, rtSlopes, and i (who was just analyzed)
  
  rtSlopes<-c(rtSlopes,summary(mtempRT)$coefficients[2])
  gambleSlopes<-c(gambleSlopes,summary(mlogtemp)$coefficients[2])
  EVdiffs<-c(EVdiffs,oddsScoreMean(dsub,int='early')-oddsScoreMean(dsub,int='late'))
  run<-c(run,i)
}

slopeDF<-data.frame(cbind(run,rtSlopes,gambleSlopes))
a<-oddsN[order(oddsN$OddsGamblingScore),]
#oddsN is the median split of people who respond well to odds
oddsN<-as.integer(a$Participant[(length(a$Participant)/2):length(a$Participant)])
#illogical Gamblers
reverseOddsN<-as.integer(a[1:(nrow(a)/4),]$Participant)

#Plot of gamble slopes vs. rtSlopes

par(bty='n')
box(which="plot",lty='solid')
plot(slopeDF$rtSlopes~slopeDF$gambleSlopes,xlab='Individual Gamble slopes (% change/second)',ylab='Individual RT Slopes (ms/second)',pch=16,cex=0.8,main=paste('Gamble slopes vs. RT slopes; n=',nrow(slopeDF)),xlim=c(-.7,.7),ylim=c(-100,100),bty='7')
abline(v=0,col="black")
abline(h=0,col="black")
#with(slopeDF, text(slopeDF$rtSlopes~slopeDF$gambleSlopes, labels = slopeDF$run,cex=.8), pos = 2)
m1<-lm(slopeDF$rtSlopes~slopeDF$gambleSlopes)


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
d5<-dgamble[dgamble$uniqueid %in% oddsN,]
d5<-dgamble[dgamble$uniqueid %in% respGamblers,] #not very many high keys
d5<-dgamble[dgamble$uniqueid %in% midGamblers,] #gambled between 40 and 60



#This is if you want intersection of two groups
#d5<-dgamble[dgamble$uniqueid %in% intersect(highGamblers,noProx),]


#This is if you want filters
summaryMagFilter=F
summaryMagCond='high'
summaryOddsFilter=F
summaryOddsCond='highp'

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
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))
#How much did each participant choose to gamble
hist(d5Behavioral$percentageGambled,breaks=50,ylim=c(0,20),xlim=c(-5,100),main=paste("Propensity to gamble; n =",toString(sum(d5Behavioral$ntrials)),"possible trials;",toString(length(unique(d5Behavioral$uniqueid))),"participants"),xlab="Percentage of time gambled")

gamblePlot(d5,orig=T,eb='sem',ylimit=c(40,60),title='midGamblers')
gambleRtPlot(d5,line=F,title="midGamblers",ylimit=c(700,1200))
switchPlot(d5,ylim=c(0,1500))
#EV sensitivity early mid and late within subject
compMeans<-c(oddsScoreMean(d5,time='early'),oddsScoreMean(d5,time='mid'),oddsScoreMean(d5,time='late'))
compSds<-c(oddsScoreEb(d5,type='sem',time='early'),oddsScoreEb(d5,type='sem',time='mid'),oddsScoreEb(d5,type='sem',time='late'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="Expected Value Sensitivity; Within-session d5")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#EV sensitivity early mid and late 
compMeans<-c(oddsScoreMean(d5,int='early'),oddsScoreMean(d5,int='mid'),oddsScoreMean(d5,int='late'))
compSds<-c(oddsScoreEb(d5,type='sem',int='early'),oddsScoreEb(d5,type='sem',int='mid'),oddsScoreEb(d5,type='sem',int='sem'))

pg9<-barplot(compMeans,names.arg = c("Early","Mid","Late"),ylim=c(0,80),
             ylab="Proportion of high value trials - low value trials",main="EV Sensitivity; When interruption happened; d5")
arrows(pg9,compMeans-compSds,pg9,compMeans+compSds,lwd=2,angle=90,code=3)

#Defining subgroups
dTrials<-dgamble %>% 
  group_by(uniqueid) %>% 
  summarise(ntrials=length(trialNumber),
            gambleCount=sum(response=="gamble"),
            successTrials=sum(response=='success'),
            failedTrials=sum(response=='fail'|response=='failOutcome'|response=='earlyFail'),
            percentageGambled=round(gambleCount/ntrials*100),
            highKeys=sum(recordedNumber>20))
head(dTrials)
respGamblers<-filter(dTrials,highKeys<50)$uniqueid
midGamblers<-filter(dTrials,percentageGambled>40&percentageGambled<60)$uniqueid
