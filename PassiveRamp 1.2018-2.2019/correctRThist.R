correctRThist<-function(data,interruption='',time='',line=T,xlimit=c(0,1500),addknob=FALSE,title=paste("RT on correct responses; n =",toString(length(correctResp)),";","gamble interrupted:",toString(interruption))){
  color='grey'
  if(interruption=='early'){
    data=filter(data,gambleDelay<1.5)
    color=rgb(0,0,1,0.5)
  }else if(interruption=='mid'){
    data=filter(data,gambleDelay>1.5&gambleDelay<2.5)
    color=rgb(1,0,0,0.5)
  }else if(interruption=='late'){
      data=filter(data,gambleDelay>2.5)
      color=rgb(0,1,0,0.5)
  }
  if(time=='early'){
    data=filter(data,trialNumber<31)
    color=rgb(0,0,1,0.5)
  }else if(time=='mid'){
    data=filter(data,trialNumber>31&trialNumber<62)
    color=rgb(1,0,0,0.5)
  }else if(time=='late'){
    data=filter(data,trialNumber>62)
    color=rgb(0,1,0,0.5)
  }
  
  datalow<-filter(data,oddsCond=='lowp'&ignoreRT!=0)
  datahigh<-filter(data,oddsCond=='highp'&gambleRT!=0)
  
  correctResp<-c(datalow$ignoreRT,datahigh$gambleRT)

  hist(correctResp,breaks=50,xlim=c(0,1500),main=title,xlab="RT (ms)",col=color,add=addknob)
  if(line){
    abline(v=median(correctResp),col=color,lwd=2)
  }
}
