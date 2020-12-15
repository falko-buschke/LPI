# Open  dataset. This must be downloaded from http://stats.livingplanetindex.org/
lpi <- read.csv("LPR2020data_public.csv", na.strings = "NULL")

# Separate the portion f the dataset that includes the population counts
pop.mat <- lpi[,30:98] 

# Create a vector for the years in the dataset (x-axis in plot)
years <- 1950:2018

# Create four blank vectors that will hold infomation on the first year of monitoring, the last year of monitoring, the population at the start of monitoring and the time-series length.
# Some of this is not used specifically in the plot, but can be valuable for exploratory analysis.
start.year <- rep(NA,dim(pop.mat)[1])
last.year <- rep(NA,dim(pop.mat)[1])
start.N <- rep(NA,dim(pop.mat)[1])
ts.length <- rep(NA,dim(pop.mat)[1])

# Run a loop for each time-series in the dataset.
for (k in 1:dim(pop.mat)[1]) {
  
  if (length(which(!is.na(pop.mat[k,])==TRUE))>0){
    # First year in dataset
    start.year[k] <- as.numeric(years[which(!is.na(pop.mat[k,])==TRUE)[1]])
    # Last year in dataset
    last.year[k] <- max(as.numeric(years[which(!is.na(pop.mat[k,])==TRUE)]))
    # Starting population size
    start.N[k] <- as.numeric(pop.mat[k,!is.na(pop.mat[k,])][1])
    Length of time-series
    ts.length[k] <- as.numeric(length(pop.mat[k,!is.na(pop.mat[k,])]))
  }
}

# Save the dependent and independent variables. 
DV <- start.N[which(start.N > 0)]
IV <- start.year[which(start.N > 0)]

# Set up plot and dimensions
png(filename="Figure2.png",width=16,height=16,units="cm",res=300)

# Set up plot margins
par(mai=c(0.75,0.9,0.1,0.1))

# Make a plot, using a log-transfromation for the y-axis.
plot(DV~IV,	pch=16,col=rgb(0,0,0,0.1), log="y",ylim=c(1e-6,1e8), ylab="", xlab="", las=1, xlim=c(1950,2020),cex.axis=1.1, cex.lab= 1.3, mgp=c(3,0.6,0))

# Add the axes labels to the plot
mtext("First year in database",1,2.4, cex=1.3)
mtext("Starting population",2,3.4, cex=1.3)

# Perform a linear regression on log-transformed dependent variable
lm.reg <- lm(log(DV)~IV)

# Predict the curve from the regression
y.pred <- predict(lm.reg,interval="confidence", newdata=data.frame(IV=c(1940:2020)))

# Add a polygon for the confirndece intervals around the regression prediction.
# This is not really visible due to the narrow confidence interval (i.e. large sample size)
polygon(c(c(1940:2020),rev(c(1940:2020))), c(exp(y.pred[,2]),rev(exp(y.pred[,3]))), col=rgb(1,0,0,0.25),border=NA)

# Add a red regression line
lines(1940:2020,exp(y.pred[,1]), col="red", lwd=1)

#Save figure and close plotting device
dev.off()
