gambleDelay=c(0,.040,.131,.203,-.19,-.04,.045,-.03,.04)
gdeb=c(0,.012,.062,.159,.04,.05,.054,.122,.05)
value=c(1.03,.21,1.61,1.25,1.26,.6,1.52,1.86)
veb=c(.096,-.114,.77,.17,.16,.10,.39,.19)
mag=c(-.05,-.114,-.8,.03,.08,-.05,.50,.5)
mageb=c(.28,.36,.09,.08,.1,.12,.46,.11)
rt=c(-20.7,-25.8,-11.9,-8.5,-30.3,-20.5,-30.2,-24.6)
rteb=c(5.03,5.87,8.2,4.1,7.9,3.5,9.6,4.5)
n=c(0,88,141,24,35,54,45,30,43)
dy=c(46,51,52)
dx=c(2,4,6)
deb=c(1.3,.9,.6)

dy2=c(780,700,680)
rteb2=c(22,23,26)

#GambleDelay
barplot(0,xlim=c(0,10),ylim=c(-.5,.5),col='black',space=1.5,
        width=.5,axes = T,ylab="Coefficient Estimate for Gamble Ramp")
abline(h=0)

for(i in gambleDelay){
  f=barplot(gambleDelay[i],xlim=c(0,10),ylim=c(-.2,.2),,col='gray91',axes = T,space=1.5,width=.5,add=T)
  arrows(f,gambleDelay[i]-gdeb[i],f,gambleDelay[i]+gdeb[i],lwd=2,angle=90,code=3)

}

#Value
v=barplot(value,xlim=c(0,10),ylim=c(-3,3),col='gray91',space=1.5,
        width=.5,axes = T,main='Effect of Value on Gamble Propensity',ylab="Coefficient Estimate for Value")
abline(h=0)
arrows(v,value-veb,v,value+veb,lwd=2,angle=90,code=3)

#Mag
m=barplot(mag,xlim=c(0,10),ylim=c(-3,3),col='gray91',space=1.5,
          width=.5,axes = T,main='Effect of Magnitude on Gamble Propensity',ylab="Coefficient Estimate for Magnitude")
abline(h=0)
arrows(m,mag-mageb,m,mag+mageb,lwd=2,angle=90,code=3)

#RT
rt2=barplot(rt,xlim=c(0,10),ylim=c(-40,10),col='gray91',space=1.5,
          width=.5,axes = T,ylab="Estimate of slope coefficient (ms change / interruption sec)")
abline(h=0)
arrows(rt2,rt-rteb,rt2,rt+rteb,lwd=2,angle=90,code=3)

#sfn poster remake gambleDelay
plot(dy~dx,ylim=c(40,55),xlim=c(0,7),pch=16,cex=2,cex.axis=1.5)
for(i in 1:length(dy)){
  arrows(dx[i],dy[i]-deb[i],dx[i],dy[i]+deb[i],lwd=2,angle=90,code=3)
}
abline(lm(dy~dx),lwd=2)

#sfn poster remake gambleDelay
plot(dy2~dx,ylim=c(600,800),xlim=c(0,7),pch=16,cex=2,cex.axis=1.5)
for(i in 1:length(dy)){
  arrows(dx[i],dy2[i]-rteb[i],dx[i],dy2[i]+rteb[i],lwd=2,angle=90,code=3)
}
abline(lm(dy2~dx),lwd=2)

