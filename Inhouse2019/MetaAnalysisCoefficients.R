gambleDelay=c(.1311,.04,.028,.203,-.19,-.04,.04,.04,-.26)
gdeb=c()
value=c(.987,1.03,.97,1.61,1.25,1.26,.60,1.86,.465)
veb=c()
mag=c(-.114,-.05,-.02,-.80,.03,.08,-.05,.50,-.11)
mageb=c()
rt=c(-25.8,-20.7,-18.4,-11.9,-8.5,-50.3,-20.5,-30.2,-17.9)
n=c()


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