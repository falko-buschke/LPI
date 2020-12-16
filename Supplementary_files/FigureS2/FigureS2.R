
png(filename="Figure S2.png",width=22,height=22,units="cm",res=300)

par(mai=c(0.5,0.6,0.25,0.05))

par(mfrow=c(3,3))

set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^0.4)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=1))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")
mtext("Small fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkgreen")
mtext("a",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^0.4)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=4))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")
mtext("Medium fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkgreen")
mtext("b",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^0.4)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=7))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")
mtext("Large fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkgreen")
mtext("c",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


####################################33
set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^1)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=1))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkblue",lwd=2,xlab="Year", shade.col=rgb(0,0,1,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="darkblue")
mtext("Small fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkblue")
mtext("d",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^1)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=4))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkblue",lwd=2,xlab="Year", shade.col=rgb(0,0,1,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="darkblue")
mtext("Medium fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkblue")
mtext("e",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^1)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=7))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkblue",lwd=2,xlab="Year", shade.col=rgb(0,0,1,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="darkblue")
mtext("Large fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkblue")
mtext("f",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


###########
set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^2.5)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=1))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="purple",lwd=2,xlab="Year", shade.col=rgb(0.5,0,1,0.25),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="purple")
mtext("Small fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="purple")
mtext("g",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^2.5)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=4))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="purple",lwd=2,xlab="Year", shade.col=rgb(0.5,0,1,0.25),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="purple")
mtext("Medium fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="purple")
mtext("h",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


set.seed(33)
years <- 1970:2020
x <- seq(0,1,l=length(years))
vect_50 <- ((60*(1 - x^2.5)) + 40)
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=7))
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="purple",lwd=2,xlab="Year", shade.col=rgb(0.5,0,1,0.25),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)
points(log10(N)~years,pch=1,type="p",cex=1, col="purple")
mtext("Large fluctuations",cex=1, side = 1, adj = 0.5, line = -2)
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="purple")
mtext("i",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)


dev.off()

