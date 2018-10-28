rtPlot<-function(data,type='raw',eb=FALSE,line=FALSE,ylimit=c(0,100),title=""){
  dRT<-filter(data,response=='gamble') %>%
    group_by(binsTime) %>%
    summarise(medianRT=mean(gambleRT),
              stdRT=std.error(gambleRT),
              sdRT=sd(gambleRT),
              medianSpeed=mean(NgambleRT),
              stdSpeed=std.error(NgambleRT),
              sdSpeed=sd(NgambleRT),
              medianRtz=median(gambleRTz),
              sdRtz=sd(gambleRTz))
  dRT$seconds<-dRT$binsTime
  
  if(type=='raw'){
    plot(dRT$seconds,dRT$medianRT,xlim = c(0,8),ylim=c(400,900),main=paste("Group data;  median RT with sd; n =",toString(sum(dlow2$gambleCount)),"trials;"),
         xlab="Seconds into trial",ylab="Reaction time (seconds)",pch=19)
    
    
  }
  }