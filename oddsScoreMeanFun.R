oddsScoreMean<-function(data,time=NULL){
  
#Breaking down by subFilter to get Odds score and mag score
  data<-filter(data,gambleDelay!=0)
  if(time=='early'){
    data<-filter(data,binsTime<3)
  }
  else if(time=='mid'){
    data<-filter(data,binsTime>3&binsTime<5)
  }
  else if(time=='late'){
    data<-filter(data,binsTime>5)
  }
  
  
  d5high<-filter(data,Trialid==31|Trialid==32|Trialid==33|Trialid==34|Trialid==35|Trialid==36)
  d5low<-filter(data,Trialid==21|Trialid==22|Trialid==23|Trialid==24|Trialid==25|Trialid==26)
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
oddsScore<-mean(d5behavioralHigh$percentageGambled)-mean(d5behavioralLow$percentageGambled)
return(oddsScore)
}
