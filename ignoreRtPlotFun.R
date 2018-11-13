ignoreRtPlot<-function(data,type='raw',eb=FALSE,line=FALSE,ylimit=c(400,900),title="",ylabel="Reaction Time (ms)"){
  dRT<-filter(data,response=='success',gambleDelay!=0) %>%
    group_by(binsTime) %>%
    summarise(medianRT=median(ignoreRT),
              stderrRT=std.error(ignoreRT),
              sdRT=sd(ignoreRT),
              medianSpeed=median(NignoreRT),
              stderrSpeed=std.error(NignoreRT),
              sdSpeed=sd(NignoreRT),
              medianRtz=median(ignoreRTz),
              sdRtz=sd(ignoreRTz))
  dRT$seconds<-dRT$binsTime
  
  if(type=='raw'){
    plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=ylimit,
         main=paste("Reaction Time vs. ignore interruption time;",title),
        # main=paste("Reaction Time vs. ignore interruption time",title,";", "n =",toString(length(data$response[data$response=='success'])),
        #            "trials;",toString(length(unique(data$uniqueid))),"participants"),
         xlab="Seconds into trial",ylab=ylabel,pch=19)
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
    plot(dRT$seconds,dRT$medianSpeed,xlim = c(0,8),ylim=c(0.0005,.0025),
         main=paste("IgnoreRT speed",title,";", "n =",toString(length(data$response[data$response=='ignore'])),
                    "trials;",toString(length(unique(data$uniqueid))),"participants"),
         xlab="Seconds into trial",ylab="Speed",pch=19)
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
