# Open  dataset after downloading it from: http://stats.livingplanetindex.org/
lpi <- read.csv("LPR2020data_public.csv", na.strings = "NULL")
 
# Create a new object that only includes the population data, without any metadata. 
# This includes data from 1950 to 2018
pop.mat <- lpi[,30:98] 
years <- 1950:2018

# Create blank vector for the length of the sampled time-series
length.val <- rep(NA, dim(pop.mat)[1])

# Create blank vector for the slope from regression
linear.slope <- rep(NA, dim(pop.mat)[1])

# Create blank vector for mean lambda
lam.mean <- rep(NA, dim(pop.mat)[1])

# Create blank vector for addive lambda values
lam.val <- rep(NA, dim(pop.mat)[1])
 
# Run a loop for each population time-series
for (i in 1:dim(pop.mat)[1]){
    # Population of species i
    pop.vect <- pop.mat[i,]
    # Remove missing data
    val.vect <- pop.vect[!is.na(pop.vect)]
    # Add a low dummy value for extinct populations to prevent devision by zero
    val.vect[which(val.vect==0)] <- 1e-99
    # Isolate the years with sampling data
    year.vect <- years[!is.na(pop.vect)]
    # Save the length of the time-series to the blank vector
    length.val[i] <- length(val.vect)
 
    # Only calculate change when there are more than one time-points
    if (length(val.vect)>1){
        # Get the slope from a liinear regression
        linear.slope[i] <- lm(log10(val.vect)~year.vect)$coefficients[2]
        # Calculate the lambda values for pair of sequential time-points
        lambda <- log10((val.vect[-length(val.vect)]+diff(val.vect)) / val.vect[-length(val.vect)])
        # Get the mean of the lambdas
        lam.mean[i] <- mean(lambda)
        # Calculate the additive change from a baseline = 1 for all the lambdas
        lam.val[i] <- (1*10^cumsum(lambda))[length(lambda)]
    } else {
        # Add default value for time-series with single measurements
        linear.slope[i] <- 0
        lambda <- 0
        lam.mean[i] <- 0
        lam.val[i] <- 1
    }
}
 

# These are the colour of the histograms. Note the 60% transparency
col3t <- rgb(1,0.7,0,0.6)
col2t <- rgb(0,0.8,1,0.6)
col1t <- rgb(0,0,0.5,0.6)


# Save the population changes as histograms
png(filename="FigureS8.png",width=24,height=8,units="cm",res=300)
par(mai=c(0.5,0.65,0.4,0.10))
par(mfrow=c(1,3))
 
###################################
#                                 #
#           Panel A               #
#                                 #
################################### 
 
# Define the breaks to visualise the histogram
brk <- c(min(linear.slope)-1,seq(-2,2,by=0.025),max(linear.slope)+1)

# Plot the histogram
hist(linear.slope, breaks=brk, las=1, ylim=c(0,8),xlim=c(-0.75,0.75), main="", 
    xlab="Regression slope", col=col1t, border=NA,
    cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
    
# Add a line for "no-change"
abline(v=0, lty=2, lwd=2); box()

# Label the panel
mtext("a",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)


###################################
#                                 #
#           Panel B               #
#                                 #
################################### 
 
# Define the breaks to visualise the histogram
brk <- c(min(lam.mean)-1,seq(-2,2,by=0.025),max(lam.mean)+1)

# Plot the histogram
hist(lam.mean, breaks=brk, las=1, ylim=c(0,8),xlim=c(-0.75,0.75), main="", 
    xlab=expression("Mean "~lambda), col=col2t, border=NA,
    cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
    
# Add a line for "no-change"
abline(v=0, lty=2, lwd=2); box()

# Label the panel
mtext("b",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)

mtext(expression(paste(lambda, "= log ", bgroup("(",frac(N[t+1],N[t]),")"))), cex=1,
 side = 3, adj = 0.92, line = -6)

###################################
#                                 #
#           Panel C               #
#                                 #
################################### 
 
# Define the breaks to visualise the histogram
brk <- c(seq(0,5,by=0.1),max(lam.val)+1)
# Plot the histogram
hist(lam.val, breaks=brk, las=1,  main="", 
    xlab=expression("Cumulative "~lambda), col=col3t, xlim=c(0,5), border=NA,
    cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
    
# Add a line for "no-change"
abline(v=1, lty=2, lwd=2); box()
# Label the panel
mtext("c",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)

mtext(expression(paste(lambda, "= log ", bgroup("(",frac(N[t+1],N[t]),")"))), cex=1,
 side = 3, adj = 0.92, line = -6)

# Save plot
dev.off()
 
