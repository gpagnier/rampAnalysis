#Plots ignore and gamble propensities on same plot
totalRTPlot<-function(data,line=FALSE,ylimit=c(700,1400),title=""){
  data<-filter(data,gambleDelay!=0,Trialid!=75|86)
  dgambleRT<-filter(data,gambleRT!=0) %>%
    group_by(binsTime) %>%
    summarise(medianRT=median(gambleRT),
              stderrRT=std.error(gambleRT),
              sdRT=sd(gambleRT))
  dgambleRT$seconds<-dgambleRT$binsTime
  
  dignoreRT<-filter(data,ignoreRT!=0) %>%
    group_by(binsTime) %>%
    summarise(medianRT=median(ignoreRT),
              stderrRT=std.error(ignoreRT),
              sdRT=sd(ignoreRT))
  dignoreRT$seconds<-dignoreRT$binsTime
  
  
  plot(dgambleRT$seconds,dgambleRT$medianRT,xlim = c(0,4),ylim=ylimit,col='blue',
       main=paste("GambleRT vs. gambleDelay;",title,";", "n =",toString(length(data$gambleRT[data$gambleRT!=0])),
                  "gambled;",toString(length(data$ignoreRT[data$ignoreRT!=0])),
                  "ignored"),xlab="Seconds into trial",ylab='RT (ms)',pch=19)
  legend(2,1100,cex=.7, bty = "n",legend=c("IgnoreRTs","GambleRTs"),col=c("orange","blue"),title="",pch=15)
  for(i in 1:length(dgambleRT$seconds)){
    arrows(as.numeric(dgambleRT$seconds[i]),as.numeric(dgambleRT[i,'medianRT']+(as.numeric(dgambleRT[i,'stderrRT']))),as.numeric(dgambleRT$seconds[i]),as.numeric(dgambleRT[i,'medianRT']-(as.numeric(dgambleRT[i,'stderrRT']))),length=0.05, angle=90, code=3)
  }
  if(line){
    abline(lm(dgambleRT$medianRT~dgambleRT$seconds),col='blue')
  }
  
  points(dignoreRT$seconds,dignoreRT$medianRT,pch=19,col='orange')
  for(i in 1:length(dignoreRT$seconds)){
    arrows(as.numeric(dignoreRT$seconds[i]),as.numeric(dignoreRT[i,'medianRT']+(as.numeric(dignoreRT[i,'stderrRT']))),as.numeric(dignoreRT$seconds[i]),as.numeric(dignoreRT[i,'medianRT']-(as.numeric(dignoreRT[i,'stderrRT']))),length=0.05, angle=90, code=3)
  }
  if(line){
    abline(lm(dignoreRT$medianRT~dignoreRT$seconds),col='orange')
  }
  
  
}
