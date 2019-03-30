switchPlot<-function(data,line=FALSE,ylimit=c(300,700)){
  #currently coded for 8 switches
  index=1:8
  
  switch1=NULL
  switch2=NULL
  switch3=NULL
  switch4=NULL
  switch5=NULL
  switch6=NULL
  switch7=NULL
  switch8=NULL
  
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
  }
  
  points<-c(mean(switch1),mean(switch2),mean(switch3),mean(switch4),mean(switch5),mean(switch6),mean(switch7),mean(switch8))
  
  std<-c(std.error(switch1),std.error(switch2),std.error(switch3),std.error(switch4),std.error(switch5),std.error(switch6),std.error(switch7),std.error(switch8))
  plot(points,ylab="mean RT (ms)",main="RT when pushing progress bar forward",ylim=ylimit,pch=19)
  #arrows(index,points-std,index,points+std,lwd=1,angle=90,code=3)
  
  
}
