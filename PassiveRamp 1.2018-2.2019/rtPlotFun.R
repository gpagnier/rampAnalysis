gambleRtPlot<-function(data,type='raw',eb=FALSE,line=FALSE,ylimit=c(900,1200),title="",ylabel="Reaction Time (ms)",trialType=""){
  
  color='black'
  if(trialType=='gambleLeft'){
    data<-filter(data,trialType=='gambleLeft')
    color='purple'
  }else if(trialType=='gambleRight'){
    data<-filter(data,trialType=='gambleRight')
    color='red'
  }
  dRT<-filter(data,response=='gamble',gambleDelay!=0,gambleRT!=0,Trialid!=75|86) %>%
    group_by(binsTime) %>%
    summarise(medianRT=median(gambleRT),
              stderrRT=std.error(gambleRT),
              sdRT=sd(gambleRT),
              #medianSpeed=median(NgambleRT),
              #stderrSpeed=std.error(NgambleRT),
              #sdSpeed=sd(NgambleRT),
              #medianRtz=median(gambleRTz),
             # sdRtz=sd(gambleRTz)
              )
  dRT$seconds<-dRT$binsTime
  
  if(type=='raw'){
    plot(dRT$seconds,dRT$medianRT,xlim = c(0,4),ylim=ylimit,
         main=paste("RT vs. gamble interruption time;",title),
        # main=paste("Reaction Time vs. gamble interruption time",title,";", "n =",toString(length(data$response[data$response=='gamble'])),
        #            "trials;",toString(length(unique(data$uniqueid))),"participants"),
         xlab="Seconds into trial",ylab=ylabel,pch=19,col=color)
    if(eb=='sd'){
      for(i in 1:length(dRT$seconds)){
        arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianRT']+(as.numeric(dRT[i,'sdRT']))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianRT']-(as.numeric(dRT[i,'sdRT']))),length=0.05, angle=90, code=3)
      }
    }
    if(eb=='stderr'){
      for(i in 1:length(dRT$seconds)){
        arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianRT']+(as.numeric(dRT[i,'stderrRT']))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianRT']-(as.numeric(dRT[i,'stderrRT']))),length=0.05, angle=90, code=3)
      }
    }
    if(line){
      abline(lm(dRT$medianRT~dRT$seconds))
    }
    summary(lm(dRT$medianRT~dRT$seconds)) 
  }
  if(type=='speed'){
    plot(dRT$seconds,dRT$medianSpeed,xlim = c(0,4),ylim=c(0.0005,.0025),
         main=paste("RT speed",title,";", "n =",toString(length(data$response[data$response=='gamble'])),
                    "trials;",toString(length(unique(data$uniqueid))),"participants"),
         xlab="Seconds into trial",ylab="Speed",pch=19,col=color)
    if(eb=='sd'){
      for(i in 1:length(dRT$seconds)){
        arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianSpeed']+(as.numeric(dRT[i,'sdSpeed']))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianSpeed']-(as.numeric(dRT[i,'sdSpeed']))),length=0.05, angle=90, code=3)
      }
    }
    if(eb=='stderr'){
      for(i in 1:length(dRT$seconds)){
        arrows(as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianSpeed']+(as.numeric(dRT[i,'stderrSpeed']))),as.numeric(dRT$seconds[i]),as.numeric(dRT[i,'medianSpeed']-(as.numeric(dRT[i,'stderrSpeed']))),length=0.05, angle=90, code=3)
      }
    }
    if(line){
      abline(lm(dRT$medianSpeed~dRT$seconds))
    }
   summary(lm(dRT$medianSpeed~dRT$seconds)) 
  }  
  
  
  
  
  
  
  
  
  
}
