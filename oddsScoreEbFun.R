oddsScoreEb<-function(data,type=NULL,time=""){
  
  #Breaking down by subFilter to get Odds score and mag score
  data<-filter(data,gambleDelay!=0)
  if(time=='early'){
    data<-filter(data,binsTime<1)
  }
  else if(time=='mid'){
    data<-filter(data,binsTime>1&binsTime<3)
  }
  else if(time=='late'){
    data<-filter(data,binsTime>3)
  }
  else{
    data=data
  }
  
  
  d5high<-filter(data,Trialid==31|Trialid==32|Trialid==34|Trialid==35|Trialid==38|Trialid==39)
  d5low<-filter(data,Trialid==21|Trialid==22|Trialid==24|Trialid==25|Trialid==28|Trialid==29)
  d5behavioralHigh<-d5high %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  d5behavioralLow<-d5low %>% 
    group_by(uniqueid) %>% 
    summarise(trials=length(trialNumber),
              gambleCount=sum(response=="gamble"),
              didNotGamble=sum(response=="fail"|response=="success"),
              percentageGambled=round(gambleCount/trials*100))
  
  
  gambledTotal<-c(d5behavioralHigh$percentageGambled,d5behavioralHigh$percentageGambled)
  if(type=='sd'){
    eb=sd(gambledTotal)
  }
  else if (type=='sem'){
    eb=std.error(gambledTotal)
  }
  return(eb)
}
