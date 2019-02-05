oddsScoreMean<-function(data,int="",time=""){
 #int is when in the progress bar gambles interrupts
  #Time is within participant subjects
#Breaking down by subFilter to get Odds score and mag score
  data<-filter(data,gambleDelay!=0)

  if(int=='early'){
    data<-filter(data,binsTime<1.5)
  } else if(int=='mid'){
    data<-filter(data,binsTime>1.5&binsTime<2.5)
  } else if(int=='late'){
    data<-filter(data,binsTime>2.5)
  }
  
  if(time=='early'){
    data=filter(data,trialNumber<46)
  }else if(time=='mid'){
    data=filter(data,trialNumber>45&trialNumber<93)
  }else if(time=='late'){
    data=filter(data,trialNumber>93)
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
  
  oddsScore<-mean(d5behavioralHigh$percentageGambled)-mean(d5behavioralLow$percentageGambled)

  return(oddsScore)
  }
}
