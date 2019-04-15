#d is filtered data frame
#group is boolean - indicates if averaging across participants(true) also know as orig
#EB is on eof 3 options: false, sem, std
#Line says if you want best fit line or not

gamblePlot<-function(data,orig=TRUE,eb='',line=FALSE,ylimit=c(0,100),title="",trialType="",standardized=F){
  if(trialType==""){
    color="black"
    data<-filter(data,gambleDelay!=0,Trialid!=75|86)
    d2fun<-data %>% 
    group_by(binsTime) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))}
  else if(trialType=="gambleLeft"){
    color="purple"
    data<-filter(data,gambleDelay!=0,Trialid!=75|86,trialType=='gambleLeft')
    d2fun<-data %>% 
    group_by(binsTime) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))}
  else if(trialType=="gambleRight"){
    color="red"
    data<-filter(data,gambleDelay!=0,Trialid!=75|86,trialType=='gambleRight')
    d2fun<-data %>% 
      group_by(binsTime) %>% 
      summarise(trials=length(trialNumber),
                gambleCount=sum(response=="gamble"),
                didNotGamble=sum(response=="fail"|response=="success"),
                percentageGambled=round(gambleCount/trials*100))
  }
  
  
  
  d2fun$seconds<-d2fun$binsTime
  if(orig){
      plot(d2fun$seconds,d2fun$percentageGambled,xlim = c(0,4),ylim = ylimit,
       main=paste("Gamble propensity",title, "n =",toString(length(data$response[data$response=='gamble'])),
                  "gambled trials;",toString(length(unique(data$uniqueid))),"participants"),
       xlab="Seconds into trial",ylab="Percentage Gambled",pch=19,col=color)
    if(line){
      abline(lm(d2fun$percentageGambled~d2fun$seconds))
    }
    summary(lm(d2fun$percentageGambled~d2fun$seconds))
  }else{
    d2pfun<-filter(data,gambleDelay!=0) %>% 
      group_by(binsTime,uniqueid) %>% 
      summarise(trials=length(trialNumber),
                gambleCount=sum(response=="gamble"),
                didNotGamble=sum(response=="fail"|response=="success"),
                percentageGambled=round(gambleCount/trials*100),
                baselineGambling=unique(baselineGambling),
                standardizedGamblingPercentage=percentageGambled-baselineGambling)
    
    d2pfun$seconds<-d2pfun$binsTime
    dTestfun<-d2pfun %>%
      group_by(seconds) %>%
      summarise(meanPercentageGambled=mean(percentageGambled),
                medianPercentageGambled=median(percentageGambled),
                sdPercentageGambled=sd(percentageGambled),
                stdPercentageGambled=std.error(percentageGambled),
                meanStandardizedGambled=mean(standardizedGamblingPercentage),
                sdStandardizedGambled=sd(standardizedGamblingPercentage),
                stdStandardizedGambled=std.error(standardizedGamblingPercentage))
    
    if(standardized){
      plot(dTestfun$seconds,dTestfun$meanStandardizedGambled,xlim = c(0,5),ylim=c(-5,5),
           main=paste("Standardized Gamble Propensity",";", "n =",toString(length(data$response[data$response=='gamble'])),
                      "gambled trials;",toString(length(unique(data$uniqueid))),"participants"),
           xlab="Seconds into trial",ylab="Percentage Gambled",pch=19,bty='l',col=color)
      if(eb=='sem'){
        for(i in 1:length(dTestfun$seconds)){
          arrows(as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanStandardizedGambled']+(as.numeric(dTestfun[i,'stdStandardizedGambled']))),as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanStandardizedGambled']-(as.numeric(dTestfun[i,'stdStandardizedGambled']))),length=0.05, angle=90, code=3)
        }
      }
    }else{
      plot(dTestfun$seconds,dTestfun$meanPercentageGambled,xlim = c(0,4),ylim = ylimit,
           main=paste("Gamble propensity",title,";", "n =",toString(length(data$response[data$response=='gamble'])),
                      "gambled trials;",toString(length(unique(data$uniqueid))),"participants"),
           xlab="Seconds into trial",ylab="Percentage Gambled",pch=19,bty='l',col=color)
      summary(lm(d2fun$percentageGambled~d2fun$seconds))
    }
    
    if(line){
      abline(lm(dTestfun$meanPercentageGambled~dTestfun$seconds))
    }
    
  if(eb=='sem'){
    for(i in 1:length(dTestfun$seconds)){
      arrows(as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageGambled']+(as.numeric(dTestfun[i,'stdPercentageGambled']))),as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageGambled']-(as.numeric(dTestfun[i,'stdPercentageGambled']))),length=0.05, angle=90, code=3)
    }
  }else if(eb=='std'){
    for(i in 1:length(dTestfun$seconds)){
      arrows(as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageGambled']+(as.numeric(dTestfun[i,'sdPercentageGambled']))),as.numeric(dTestfun$seconds[i]),as.numeric(dTestfun[i,'meanPercentageGambled']-(as.numeric(dTestfun[i,'sdPercentageGambled']))),length=0.05, angle=90, code=3)
    }
  }
  
  }

}
