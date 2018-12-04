#d is filtered data frame
#group is boolean - indicates if averaging across participants(true) also know as orig
#EB is on eof 3 options: false, sem, std
#Line says if you want best fit line or not

gamblePlot<-function(data,orig=TRUE,eb=FALSE,line=FALSE,ylimit=c(0,100),title=""){
  d2fun<-filter(data,gambleDelay!=0,Trialid!=75|86) %>% 
    group_by(binsTime) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  d2fun$seconds<-d2fun$binsTime
  if(orig){
      plot(d2fun$seconds,d2fun$percentageGambled,xlim = c(0,8),ylim = ylimit,
       main=paste("Gamble propensity",title, "n =",toString(length(data$response[data$response=='gamble'])),
                  "trials;",toString(length(unique(data$uniqueid))),"participants"),
       xlab="Seconds into trial",ylab="Percentage Gambled",pch=19)
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
                percentageGambled=round(gambleCount/trials*100))
    
    d2pfun$seconds<-d2pfun$binsTime
    dTestfun<-d2pfun %>%
      group_by(seconds) %>%
      summarise(meanPercentageGambled=mean(percentageGambled),
                medianPercentageGambled=median(percentageGambled),
                sdPercentageGambled=sd(percentageGambled),
                stdPercentageGambled=std.error(percentageGambled))
    
    plot(dTestfun$seconds,dTestfun$meanPercentageGambled,xlim = c(0,8),ylim = ylimit,
         main=paste("Gamble propensity",title,";", "n =",toString(length(data$response[data$response=='gamble'])),
                    "trials;",toString(length(unique(data$uniqueid))),"participants"),
         xlab="Seconds into trial",ylab="Percentage Gambled",pch=19,bty='l')
    summary(lm(d2fun$percentageGambled~d2fun$seconds))
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
