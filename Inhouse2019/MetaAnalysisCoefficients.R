gambleDelay=c(0,.040,.131,.203,-.19,-.04,.045,-.03,.04,-.266)
gdeb=c(0,.012,.062,.159,.04,.05,.054,.122,.05,.162)
value=c(1.03,.21,1.61,1.25,1.26,.6,1.52,1.86,.46)
veb=c(.096,-.114,.77,.17,.16,.10,.39,.19,.15)
mag=c(-.05,-.114,-.8,.03,.08,-.05,.50,.5,-.11)
mageb=c(.28,.36,.09,.08,.1,.12,.46,.11,.11)
rt=c(-20.7,-25.8,-11.9,-8.5,-30.3,-20.5,-30.2,-24.6,-11.27)
n=c(0,88,141,24,35,54,45,30,43,30)


#GambleDelay
barplot(0,xlim=c(0,10),ylim=c(-.5,.5),col='black',space=1,
        width=.5,axes = T,main='Coefficient Estimate for Gamble Ramp',ylab="Coefficient Estimate for Gamble Ramp")
abline(h=0)

for(i in gambleDelay){
  print(i)
  f=barplot(gambleDelay[i],xlim=c(0,10),ylim=c(-.2,.2),,col='gray',axes = T,space=1,width=.5,add=T)
  arrows(f,gambleDelay[i]-gdeb[i],f,gambleDelay[i]+gdeb[i],lwd=2,angle=90,code=3)

}

#Value
v=barplot(value,xlim=c(0,10),ylim=c(-3,3),col='gray',space=1,
        width=.5,axes = T,main='Coefficient Estimate for Value',ylab="Coefficient Estimate for Value")
abline(h=0)
arrows(v,value-veb,v,value+veb,lwd=2,angle=90,code=3)

#Mag
m=barplot(mag,xlim=c(0,10),ylim=c(-3,3),col='gray',space=1,
          width=.5,axes = T,main='Coefficient Estimate for Magnitude',ylab="Coefficient Estimate for Magnitude")
abline(h=0)
arrows(m,mag-mageb,m,mag+mageb,lwd=2,angle=90,code=3)

#RT
rt=barplot(rt,xlim=c(0,10),ylim=c(-30,10),col='gray',space=1,
          width=.5,axes = T,main='Slope Coefficient Estimate for RT',ylab="Slope Coefficient Estimate for RT (ms change / interruption sec)")
abline(h=0)
