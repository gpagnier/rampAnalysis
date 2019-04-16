switchPlot<-function(data,line=FALSE,ylimit=c(000,1000)){
  #currently coded for 11 switches
  index=1:11
  
  switch1=NULL
  switch2=NULL
  switch3=NULL
  switch4=NULL
  switch5=NULL
  switch6=NULL
  switch7=NULL
  switch8=NULL
  switch9=NULL
  switch10=NULL
  switch11=NULL
  for(i in 1:nrow(data)){
    temp<-unlist(d[i,'switchTimes'])
    if(mean(temp)>5000){
      next()
    }
    switch1=c(switch1,temp[1])
    switch2=c(switch2,temp[2])
    switch3=c(switch3,temp[3])
    switch4=c(switch4,temp[4])
    switch5=c(switch5,temp[5])
    switch6=c(switch6,temp[6])
    switch7=c(switch7,temp[7])
    switch8=c(switch8,temp[8])
    switch9=c(switch9,temp[9])
    switch10=c(switch10,temp[10])
    switch11=c(switch11,temp[11])
  }
  
  points<-c(mean(switch1,na.rm=TRUE),mean(switch2,na.rm=TRUE),mean(switch3,na.rm=TRUE),mean(switch4,na.rm=TRUE),mean(switch5,na.rm=TRUE),mean(switch6,na.rm=TRUE),mean(switch7,na.rm=TRUE),mean(switch8,na.rm=TRUE),mean(switch9,na.rm=TRUE),mean(switch10,na.rm=TRUE),mean(switch11,na.rm=TRUE))
  
  std<-c(std.error(switch1,na.rm=TRUE),std.error(switch2,na.rm=TRUE),std.error(switch3,na.rm=TRUE),std.error(switch4,na.rm=TRUE),std.error(switch5,na.rm=TRUE),std.error(switch6,na.rm=TRUE),std.error(switch7,na.rm=TRUE),std.error(switch8,na.rm=TRUE),std.error(switch9,na.rm=TRUE),std.error(switch10,na.rm=TRUE),std.error(switch11,na.rm=TRUE))
  plot(points,ylab="mean RT (ms)",main="RT when pushing progress bar forward",ylim=ylimit,pch=19)
  #arrows(index,points-std,index,points+std,lwd=1,angle=90,code=3)
  
  
}
