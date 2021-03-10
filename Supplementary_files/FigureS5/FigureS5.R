# Define plot name and dimensions
png(filename="FigureS5.png",width=24,height=16,units="cm",res=300)

# Set plot margins
par(mai=c(0.65,0.65,0.3,0.05))

# Define a 2 X 2 panel layout
par(mfrow=c(2,2))

# Set the random sed for the stochasic process
set.seed(33)

# Define the duration of the time-series
years <- seq(1970,2020, by =5)

# Standardise the time valiable between 0 and 1
x <- seq(0,1,l=length(years))

# Define the the deteminstic populations trend
vect_50 <- ((60*(1 - x^0.3)) + 40)

# Add some random noise
N <- ceiling(vect_50)

####################
#                  #
#     Panel A      #
#                  #
####################

# Plot the time-series
plot(years,N ,type="o",cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0),ylim=c(0,120),cex=2,col="darkgreen",lwd=1.5,xlab="Year",ylab="Population",las=1)

# Add explanatory text
mtext("Population trend",cex=1, side = 3, adj = 0.5, line = -2)

# Label the panel
mtext("a",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

# Use a loop to add lines for incremental changes between time-points
for (k in 1:length(N)){
  lines(c(0,years[k]),c(N[k],N[k]), col="grey", lwd=0.5)
}

####################
#                  #
#     Panel B      #
#                  #
####################

# Plot the histogram of delta values
hist(diff(N),breaks=12,main="",xlab=expression(Delta),cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0),las=1);box()

# Label the panel
mtext("b",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

####################
#                  #
#     Panel C      #
#                  #
####################

# Randomly sample the delta values, without replacement
deltas <- sample(diff(N),length(diff(N)),replace=F)

# Plot the reshuffled time-series
plot(years,c(100,100+cumsum(deltas)) ,type="o",cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0),ylim=c(0,120),cex=2,col="darkgreen",lwd=1.5,xlab="Year",ylab="Population",las=1)

# Add explanatory text
mtext("Reshuffled trend",cex=1, side = 3, adj = 0.5, line = -2)

# Include reference line
abline(h=c(100,40),col="grey")

# Label the panel
mtext("c",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

####################
#                  #
#     Panel D      #
#                  #
####################

# Plot one reshuffled time-series
plot(years,c(100,100+cumsum(deltas)) ,type="l",cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0),ylim=c(0,120),cex=2,col=rgb(0,1,0,0.2),lwd=1.5,xlab="Year",ylab="Population",las=1)

# Add explanatory text
mtext("Iterated 100 times",cex=1, side = 3, adj = 0.5, line = -2)

# Create a blank holder matric for the 100 iterations
delta.mat <- matrix(NA,nrow=100,ncol=length(years))

# Use a loop to repeat the reshuffling 100 time, adding a line each time
for(k in 1:100) {
  # Sample from the delta values randomly
  deltas <- sample(diff(N),length(diff(N)),replace=F)
  # Simulate a reshuffled time-series
  simval <- c(100,100+cumsum(deltas))
  # Add the line to the plot
  lines(years,simval,col=rgb(0,0.5,0,0.05))
  # Save the iteration to the holder matrix
  delta.mat[k,] <- simval
}

# Add a darker line for the average of the 100 iterations
lines(years,apply(delta.mat,2,mean), lty=2, lwd=2,col="darkgreen")

# Add a reference line
abline(h=c(100,40),col="grey")

# Label the panel
mtext("d",cex=1.3, side = 3, adj = -0.1, line = 0.6,font=2)

# Add a legend
legend("bottom", ncol=2, lty=c(1,2), lwd=c(1,2), col="darkgreen", c("One iteration", "Mean trend"))

# Turn off the plot device and save to file
dev.off()
