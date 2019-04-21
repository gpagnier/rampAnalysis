gambleDelay=c(.04,.1311,.203,-.19-.04,.045,-.03,.04,-.266)
gdeb=c(.012,.062,.159,.04,.05,.054,.122,.05,.162)
value=c(1.03,.21,1.61,1.25,1.26,.6,1.52,1.86,.46)
veb=c(.096,.-.114,.77,.17,.16,.10,.39,.19,.15)
mag=c(-.05,-.114,-.8,.03,.08,-.05,.50,.5,-.11)
mageb=c(.28,.36,.09,.08,.1,.12,.46,.11,.11)
rt=c(-20.7,-25.8,-11.9,-8.5,-50.3,-20.5,-30.2,-24.6,-11.27)
n=c(88,141,24,35,54,45,30,43,30)



barplot(height=0,xlim=c(0,10),ylim=c(-.3,.3),col='black',space=1,
        width=.5,axes = T,main='Slope Coefficients for Gamble Ramp',ylab="Estimate of slope coefficient (% change / second)")
abline(h=0)
for(i in gambleDelay){
  barplot(gambleDelay[i],xlim=c(0,10),ylim=c(-.2,.2),,col='black',axes = T,space=1.3,width=.5,add=T)
}

barplot(height=0,xlim=c(0,10),ylim=c(-.3,.3),col='black',space=1,
        width=.5,axes = T,main='Slope Coefficients for Gamble Ramp',ylab="Estimate of slope coefficient (% change / second)")
abline(h=0)
for(i in gambleDelay){
  barplot(gambleDelay[i],xlim=c(0,10),ylim=c(-.2,.2),,col='black',axes = T,space=1.3,width=.5,add=T)
}