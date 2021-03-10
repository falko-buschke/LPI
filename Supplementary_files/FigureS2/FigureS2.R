# Install and load package for GAM model
install.packages("mgcv")
library(mgcv)

# Set up the plot filename and dimensions
png(filename="Figure S2.png",width=22,height=22,units="cm",res=300)

# Set plot margins
par(mai=c(0.5,0.6,0.25,0.05))

# Define a 3 X 3 panel outlay
par(mfrow=c(3,3))

######################
#                    #
#      Panel A       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^0.4)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=1))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")

# Label the treatment
mtext("Small fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkgreen")

# Label the panel
mtext("a",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel B       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^0.4)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=4))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")

# Label the treatment
mtext("Medium fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkgreen")

# Label the panel
mtext("b",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel C       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^0.4)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=7))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")

# Label the treatment
mtext("Large fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkgreen")

# Label the panel
mtext("c",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel D       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^1)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=1))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkblue",lwd=2,xlab="Year", shade.col=rgb(0,0,1,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="darkblue")

# Label the treatment
mtext("Small fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkblue")

# Label the panel
mtext("d",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel E       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^1)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=4))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkblue",lwd=2,xlab="Year", shade.col=rgb(0,0,1,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="darkblue")

# Label the treatment
mtext("Medium fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkblue")

# Label the panel
mtext("e",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel F       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^1)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=7))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="darkblue",lwd=2,xlab="Year", shade.col=rgb(0,0,1,0.5),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="darkblue")

# Label the treatment
mtext("Large fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="darkblue")

# Label the panel
mtext("f",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel G       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^2.5)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=1))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="purple",lwd=2,xlab="Year", shade.col=rgb(0.5,0,1,0.25),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="purple")

# Label the treatment
mtext("Small fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="purple")

# Label the panel
mtext("g",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel H       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^2.5)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=4))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="purple",lwd=2,xlab="Year", shade.col=rgb(0.5,0,1,0.25),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="purple")

# Label the treatment
mtext("Medium fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="purple")

# Label the panel
mtext("h",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

######################
#                    #
#      Panel I       #
#                    #
######################

# Set random seed for stochastic process
set.seed(33)

# Define duration of time-series
years <- 1970:2020
# Standardise the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# define the deterministic population trend
vect_50 <- ((60*(1 - x^2.5)) + 40)
# Add random noise to time series
N <- ceiling(vect_50 + rnorm(length(vect_50),mean=0,sd=7))

# Define the GAM model
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Make the plot, starting with the GAM prediction 
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(1.45,2.05),
 col="purple",lwd=2,xlab="Year", shade.col=rgb(0.5,0,1,0.25),residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the points from the time-series
points(log10(N)~years,pch=1,type="p",cex=1, col="purple")

# Label the treatment
mtext("Large fluctuations",cex=1, side = 1, adj = 0.5, line = -2)

# Add reference lines
abline(h=c(predict(rr.gam)[1],predict(rr.gam)[51]), lty=1,col="red")
abline(h=log10(c(100,40)), lty=2,col="purple")

# Label the panel
mtext("i",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

# Close plot device and save file
dev.off()

