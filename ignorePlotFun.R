#d is filtered data frame
#Do not use trialType with it 

ignorePlot<-function(data,orig=TRUE,eb=FALSE,line=FALSE,ylimit=c(0,100),title="",trialType=""){
  
  color='black'
  if(trialType=='gambleLeft'){
    data<-filter(data,trialType=='gambleLeft')
    color='purple'
  }else if(trialType=='gambleRight'){
    data<-filter(data,trialType=='gambleRight')
    color='red'
  }
  
  d2fun<-filter(data,gambleDelay!=0,oddsCond!='catch') %>% 
    group_by(binsTime) %>% 
    summarise(trials=length(trialNumber),
              ignoreCount=sum(ignoreRT!=0),
              didNotIgnore=sum(response=="fail"|response=="gamble"),
              percentageIgnored=round(ignoreCount/trials*100),
              medianRT=median(setdiff(ignoreRT,0)))
  d2fun$seconds<-d2fun$binsTime
  if(orig){
      plot(d2fun$seconds,d2fun$percentageIgnored,xlim = c(0,8),ylim = ylimit,
       main=paste("Ignore propensity",title, "n =",toString(length(data$ignoreRT[data$ignoreRT!=0])),
                  "ignored trials;",toString(length(unique(data$uniqueid))),"participants"),
       xlab="Seconds into trial",ylab="Percentage Ignored",pch=19,col=color)
    if(line){
      abline(lm(d2fun$percentageIgnored~d2fun$seconds))
    }
    summary(lm(d2fun$percentageIgnored~d2fun$seconds))
  }else{
    d2pfun<-filter(data,gambleDelay!=0) %>% 
      group_by(binsTime,uniqueid) %>% 
      summarise(trials=length(trialNumber),
                ignoreCount=sum(ignoreRT!=0),
                didNotIgnore=sum(response=="fail"|response=="ignore"),
                percentageIgnored=round(ignoreCount/trials*100),
                medianRT=median(setdiff(ignoreRT,0)),
                semRT=sd(setdiff(ignoreRT,0))/sqrt(length(setdiff(ignoreRT,0))))
    
    d2pfun$seconds<-d2pfun$binsTime
    dTestfun<-d2pfun %>%
      group_by(seconds) %>%
      summarise(meanPercentageIgnored=mean(percentageIgnored),
                medianPercentageIgnored=median(percentageIgnored),
                sdPercentageIgnored=sd(percentageIgnored),
                stdPercentageIgnored=std.error(percentageIgnored))
    
    plot(dTestfun$seconds,dTestfun$meanPercentageIgnored,xlim = c(0,8),ylim = ylimit,
         main=paste("Gamble propensity",title,";", "n =",toString(length(data$response[data$response=='gamble'])),
                    "trials;",toString(length(unique(data$uniqueid))),"participants"),
         xlab="Seconds into trial",ylab="Percentage Ignored",pch=19,bty='l',col=color)
    summary(lm(d2fun$percentageIgnored~d2fun$seconds))
    if(line){
      abline(lm(dTestfun$meanPercentageIgnored~dTestfun$seconds))
    }
    
  if(eb=='sem'){
    for(i in 1:length(dTestfun$seconds)){
      arrows(as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageIgnored']+(as.numeric(dTestfun[i,'stdPercentageIgnored']))),as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageIgnored']-(as.numeric(dTestfun[i,'stdPercentageIgnored']))),length=0.05, angle=90, code=3)
    }
  }else if(eb=='std'){
    for(i in 1:length(dTestfun$seconds)){
      arrows(as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageIgnored']+(as.numeric(dTestfun[i,'sdPercentageIgnored']))),as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageIgnored']-(as.numeric(dTestfun[i,'sdPercentageIgnored']))),length=0.05, angle=90, code=3)
    }
  }
  
  }

}

