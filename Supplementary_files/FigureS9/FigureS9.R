# Create a year vector
years <- 1970:2020

# Standardise years between 0 and 1
x <- seq(0,1,l=length(years))

# Simulate a population that decline from 100 to 40 along a concave-up trajectory
pop<- ((60*(1 - x^.25)) + 40)
 
#Create blank vectors for population changes for increase time-sereis lengths (minimum of 2 years)
slope <- rep(NA, length(years)-1)     # Regression slope
lam <- rep(NA, length(years)-1)       # Mean lambda
cum.lam <- rep(NA, length(years)-1)   # Cumulative lambda
 
# Run a loop to calcualte population changes fro increasing time-series length
for (i in 2:length(years)){
    # Slope from regression
    slope[i] <- lm(log10(pop[1:i])~years[1:i])$coefficients[2]
    # Lambda values
    lambda <- log10((pop[1:i][-length(pop[1:i])]+diff(pop[1:i])) / pop[1:i][-length(pop[1:i])])
    # Mean Lambda
    lam[i] <- mean(lambda)
    # Cumualtive lambda
    cum.lam[i] <- (1*10^cumsum(lambda))[length(lambda)]
}
 
# Set baseline for cumulative lambda
cum.lam[1] <- 1

# Length of time-series
ts.length <- 1:length(years)
 
# Plot how sample completeness affects popualtion change estimates
png(filename="FigureS9.png", width=20,height=14,units="cm",res=300)

#Set panel layout
layout (matrix(c(1,1,1,2,1,1,1,3,1,1,1,4), nrow = 3, ncol = 4, byrow = TRUE))
# Set plot margins
par(mai=c(0.5,0.55,0.05,0.05))

###################################
#                                 #
#           Panel A               #
#                                 #
###################################

# Save the simulated population time-series as a panel
plot(pop~years, las=1, type="o",pch=16,cex=2, col="grey", ylim=c(0,120), xlab="Year",  ylab="Population abundance",cex.axis=1.3, cex.lab= 1.5,  mgp=c(2.8,0.9,0))

# Label panel
mtext("a",cex=1.5, side = 3, adj = 0.05, line = -2,font=2)


###################################
#                                 #
#           Panel B               #
#                                 #
###################################

# Define the colour palette
col3t <- rgb(1,0.7,0,0.6)
col2t <- rgb(0,0.8,1,0.6)
col1t <- rgb(0,0,0.5,0.6)

# Adjust plot pargins
par(mai=c(0.5,0.5,0.05,0.05))
 
# Plot the population slopes
plot(ts.length,slope,col=col1t, las=1,ylim=c(-0.15,0), pch=16,cex=1.5,  xlab="Time-series length", ylab="",cex.axis=1.0, cex.lab= 1.2, mgp=c(1.8,0.6,0))

# Label panel
mtext("b",cex=1.3, side = 1, adj = 0.9, line = -2,font=2)

# Add axis text
mtext("Regression slope",cex=0.8, side = 2, adj = 0.5, line = 2.8)

###################################
#                                 #
#           Panel C               #
#                                 #
###################################

# Plot mean lambda
plot(ts.length,lam,col=col2t, las=1, pch=16,cex=1.5, ylim=c(-0.15,0), xlab="Time-series length", ylab="",cex.axis=1.0, cex.lab= 1.2, mgp=c(1.8,0.6,0))

# Label panel
mtext("c",cex=1.3, side = 1, adj = 0.9, line = -2,font=2)

# Add axis text
mtext(expression("Mean "~lambda),cex=0.8, side = 2, adj = 0.5, line = 2.8)
 
###################################
#                                 #
#           Panel D               #
#                                 #
###################################

# Plot cumulative lambda
plot(ts.length,cum.lam,col=col3t, las=1, pch=16,cex=1.5, ylim=c(0,1), xlab="Time-series length", ylab="",cex.axis=1.0, cex.lab= 1.2, mgp=c(1.8,0.6,0))

# Label panel
mtext("d",cex=1.3, side = 1, adj = 0.9, line = -2,font=2)

# Add axis text
mtext(expression("Cumulative "~lambda),cex=0.8, side = 2, adj = 0.5, line = 2.8)

# save plot
dev.off()

