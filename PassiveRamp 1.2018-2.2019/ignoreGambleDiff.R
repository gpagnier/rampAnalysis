#Need to fix hard code, currently only works for all participants and all conditions 
ignoreGambleDiff<-function(data,ylimit=c(-100,100),title=""){
  require(ggplot2)
  dataActive=filter(data,trialType=='activeTrial',gambleRT!=0)
  dRTActive<-dataActive %>%
    group_by(binsTime,uniqueid) %>%
    summarise(medianRT=median(gambleRT),
              stderrRT=std.error(gambleRT),
              sdRT=sd(gambleRT)
              )
  dRTActive$seconds<-dRTActive$binsTime
  
  dataIgnore=filter(data,trialType=='ignoreTrial',ignoreRT!=0)
  dRTIgnore<-dataIgnore %>%
    group_by(binsTime,uniqueid) %>%
    summarise(medianRT=median(ignoreRT),
              stderrRT=std.error(ignoreRT),
              sdRT=sd(ignoreRT)
    )

  dataTotal<-cbind(dRTActive,dRTIgnore)
  dataTotal<-dataTotal[,c(1,2,3,8,9)]
  dataTotal$diff<-dataTotal$medianRT-dataTotal$medianRT1
  data2<-filter(dataTotal,binsTime==2)
  data4<-filter(dataTotal,binsTime==4)
  data6<-filter(dataTotal,binsTime==6)
  
  
  compMeans<-c(mean(data2$diff),mean(data4$diff),mean(data6$diff))
  compSds<-c(std.error(data2$diff),std.error(data4$diff),std.error(data6$diff))
  
  plot(c(2,4,6),compMeans,xlim = c(0,8),ylim=ylimit,col='black',ylab="ms difference of blueActive - orangeIgnore trials",
       main=paste("Within subj. difference of active - ignore ;n =",
                 toString(length(unique(dataTotal$uniqueid))),"participants"),
       xlab="Seconds into trial",pch=19)
  for(i in 1:length(compSds)){
    arrows(c(2,4,6)[i],as.numeric(compMeans[i]+compSds[i]),c(2,4,6)[i],as.numeric(compMeans[i]-compSds[i]),length=0.05, angle=90, code=3)
  }
  abline(h=0,col="black")
  
}
