# Install and load package for GAM model
install.packages("mgcv")
library(mgcv)

# Set random seed to replicate stochastic process
set.seed(33)

# Define the time duration of the simulation
years <- 1970:2020
# Standaridise the time-series years between 0 and 1
x <- seq(0,1,l=length(years))

# Simulate the deterministic time-series for a population declining from 100 to 20 individuals
vect_100 <- ((80*(1 - x^0.25)) + 20)

# Add some random noise to the time-series
N <- ceiling(vect_50 + rnorm(length(vect_100),mean=0,sd=4))

# Define the plot and its dimensions
png(filename="FigureS1.png",width=24,height=16,units="cm",res=300)

# Set plot margins
par(mai=c(0.65,0.65,0.05,0.05))

# Set 2 X 3 panel layout
par(mfrow=c(2,3))

#########################
#                       # 
#       Panel A         #
#                       #
#########################

# Make a plot of population through time
plot(years,N ,type="o",cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0),ylim=c(0,120), col="darkgreen",lwd=1.5,xlab="Year",ylab="Population",las=1)

# Add a label
mtext("Step 1: Single population trend",cex=1, side = 3, adj = 0.5, line = -2)

#########################
#                       # 
#       Panel B         #
#                       #
#########################

# Set up a GAM model on th elog-transformed population data
rr.gam <- gam (log10(N)~ s(years, k=ceiling(length(years)/2)) ,method="REML" )

# Start by making the plot with the smoothed GAM time-series (log axis)
plot(rr.gam,pages=0,select=1,cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0), shift=mean(log10(vect_50)),ylim=c(0,3), 
col="darkgreen",lwd=2,xlab="Year", shade.col=rgb(0,1,0,0.5), residuals=F,rug=F,shade=T,ylab="log10(Population)",las=1)

# Add the population data points
points(log10(N)~years,pch=1,type="p",cex=1, col="darkgreen")

# Add a label
mtext("Step 2: Model trend using GAM",cex=1, side = 3, adj = 0.5, line = -2)

#########################
#                       # 
#       Panel C         #
#                       #
#########################

# Make a plot with GAM prediction
plot(years,10^predict(rr.gam) ,type="l",cex.axis=1, cex.lab= 1.3, mgp=c(2.5,0.6,0),ylim=c(0,120),
 col="darkgreen",lwd=1.5,xlab="Year",ylab="Population",las=1)

# Add vertical and horizontal sequential time-points as grey lines
for (k in 1:length(predict(rr.gam))){
    lines(c(years[k],years[k]),c(0,10^predict(rr.gam)[k]), col="grey", lwd=0.5)
    lines(c(0,years[k]),c(10^predict(rr.gam)[k],10^predict(rr.gam)[k]), col="grey", lwd=0.5)
}

# Add a label
mtext("Step 3: Sequential change",cex=1, side = 3, adj = 0.5, line = -2)

#########################
#                       # 
#       Panel D         #
#                       #
#########################

# Calcualte incremental lambdas (log-ratio of sequential populations) from GAM prediction
lambda <- log10((10^predict(rr.gam)[-51]+diff(10^predict(rr.gam))) / 10^predict(rr.gam)[-51])

# Plot the lambdas
plot(years[-1],(lambda) ,type="l",cex.axis=1, cex.lab= 1.3, mgp=c(2.5,0.6,0),ylim=c(-0.025,0.025),
 col="darkgreen",lwd=1.5,xlab="Year",ylab=expression(lambda),las=1)

# Add a baseline
abline(h=0,col="grey")

# Add a label
mtext("Step 4: Rate of change",cex=1, side = 3, adj = 0.5, line = -2)

# Add the lambda equation
text(1995, 0.0125, expression(paste(lambda, "= log ", bgroup("(",frac(N[t+1],N[t]),")"))), cex=1.5)


#########################
#                       # 
#       Panel E         #
#                       #
#########################

# Plot the standardised trend from the lambdas
plot(years,c(1,1*(10^(cumsum(lambda)))) ,type="l",cex.axis=1.1, cex.lab= 1.3, 
 mgp=c(2.5,0.6,0),ylim=c(0,2), col="darkgreen",lwd=1.5,xlab="Year",ylab= "LPI (1970 = 1)",las=1)

# Add baseline
abline(h=1,col="grey")

# Add a label
mtext("Step 5: Standardise declines",cex=1, side = 3, adj = 0.5, line = -2)

#########################
#                       # 
#       Panel F         #
#                       #
#########################

# Make blank plot
plot(0,0,type="n", ylab="", xlab="", axes=F, ylim=c(0,100), xlim=c(0,100))
# Include bounding box
box()

# Add a label
mtext("Steps 6-9: Aggregate data",cex=1, side = 3, adj = 0.5, line = -2)

# Describe the steps
text(50,80, "Geometric mean across populations", cex=1.2, col="darkgreen")
text(50,60, "Geometric mean across vetebrate\nclasses (with weighting)", cex=1.2, col=rgb(0.4,0.7,0,1))
text(50,40, "Geometric mean across biogeographical\nregions (with weighting)", cex=1.2, col=rgb(0.7,0.4,0,1))
text(50,20, "Geometric mean across realms\n(equal weighting)", cex=1.2, col="red")

# Close plot device and save file
dev.off()
