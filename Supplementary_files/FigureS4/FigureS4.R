# First instal the package 'devtools' needed to install the 'rlpi' package from GitHub
install.packages("devtools")

# Load 'devtools' package
library(devtools)

# Install from main ZSL repository online
install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
# Load the 'rlpi' package
library(rlpi)

###########################################################################

# Set the random seed so that the stochastic process is identical to the main manuscript
set.seed(42)

# Define the duration of the time series
years <- 1970:2020

# Define the number of species time-series in the simulation
S <- 500

# Create a dummy variable that rescale the duration of the time-series between 0 and 1
x <- seq(0,1,l=length(years))

# Randomly generate noise in a low- and and high-fluctuation scenario. Set the first and last year to have no noise
N.low <- matrix(rnorm(n=S*length(years),mean=0,sd=1), nrow=S,ncol=length(years)) ; N.low[,c(1,length(years))] <- 0
N.high <- matrix(rnorm(n=S*length(years),mean=0,sd=7), nrow=S,ncol=length(years)) ; N.high[,c(1,length(years))] <- 0

# Here I used the same naming conventionas in Figure 1. However, here the code '50' refers to a concave-up trajectory, '100' is a linear trajectory, and '150' is a concave-down trajectory
vect_50 <- ((60*(1 - x^0.2)) + 40)
vect_100 <- ((60*(1 - x^1)) + 40)
vect_150 <- ((60*(1 - x^5)) + 40)

# Add the noise to the trajectories to make 500 unique populations for the low ('l') fluctuation scenario
N_50l <- sweep(N.low, MARGIN=2, vect_50, '+')
N_100l <- sweep(N.low, MARGIN=2, vect_100, '+')
N_150l <- sweep(N.low, MARGIN=2, vect_150, '+')

# Calculate the average for the 500 low fluctuation populations
Ave_50l <- apply(N_50l,2,function(x) {mean(x,na.rm=T)})
Ave_100l <- apply(N_100l,2,function(x) {mean(x,na.rm=T)})
Ave_150l <- apply(N_150l,2,function(x) {mean(x,na.rm=T)})


# Add the noise to the trajectories to make 500 unique populations for the high ('h') fluctuation scenario
N_50h <- sweep(N.high, MARGIN=2, vect_50, '+')
N_100h <- sweep(N.high, MARGIN=2, vect_100, '+')
N_150h <- sweep(N.high, MARGIN=2, vect_150, '+')

# Calculate the average for the 500 high fluctuation populations
Ave_50h <- apply(N_50h,2,function(x) {mean(x,na.rm=T)})
Ave_100h <- apply(N_100h,2,function(x) {mean(x,na.rm=T)})
Ave_150h <- apply(N_150h,2,function(x) {mean(x,na.rm=T)})

############################################################

# Define unique integer ID for each time-series
ID <- 1:S

# Define unique integer ID for each species
Species <- as.factor(1:S)

# Combine the ID and species name to the simulated low-fluctuation populations
Pop_50l <- cbind(ID,Species,N_50l)
Pop_100l <- cbind(ID,Species,N_100l)
Pop_150l <- cbind(ID,Species,N_150l)

# Add the default column names as required by the 'rlpi' package
colnames(Pop_50l) <- (c("ID","Binomial",paste0("X",as.factor(years))))
colnames(Pop_100l) <- (c("ID","Binomial",paste0("X",as.factor(years))))
colnames(Pop_150l) <- (c("ID","Binomial",paste0("X",as.factor(years))))


# Combine the ID and species name to the simulated high-fluctuation populations
Pop_50h <- cbind(ID,Species,N_50h)
Pop_100h <- cbind(ID,Species,N_100h)
Pop_150h <- cbind(ID,Species,N_150h)

# Add the default column names as required by the 'rlpi' package
colnames(Pop_50h) <- (c("ID","Binomial",paste0("X",as.factor(years))))
colnames(Pop_100h) <- (c("ID","Binomial",paste0("X",as.factor(years))))
colnames(Pop_150h) <- (c("ID","Binomial",paste0("X",as.factor(years))))


############################################################################################


# This is just an index vector for wich time-series should be included in calculations
# Here, we include all time series (Note: this command in needed when you want to use a subset of the data)
index_vector <- rep(TRUE, S)

#This creates an 'infile' for calcualting the LPI for the low-fluctuation populations
# Note: you need a folder named 'LPI_files' in your working directory
infile_50l <- create_infile(as.data.frame(Pop_50l), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_50l")
lpi_50l <- LPIMain(infile_50l, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_100l <- create_infile(as.data.frame(Pop_100l), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_100l")
lpi_100l <- LPIMain(infile_100l, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_150l <- create_infile(as.data.frame(Pop_150l), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_150l")
lpi_150l <- LPIMain(infile_150l, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)


# This creates an 'infile' for calcualting the LPI for the high-fluctuation polulations
# Note: you need a folder names 'LPI_files' in your working directory
infile_50h <- create_infile(as.data.frame(Pop_50h), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_50h")
lpi_50h <- LPIMain(infile_50h, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_100h <- create_infile(as.data.frame(Pop_100h), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_100h")
lpi_100h <- LPIMain(infile_100h, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_150h <- create_infile(as.data.frame(Pop_150h), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_150h")
lpi_150h <- LPIMain(infile_150h, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

#############################################

# The simulated null model is generated by the 'NullModel.R' script.
# Import the outputs here

output.mat <-  read.table("IterationOutputIncrease.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

#############################################

# Set up the colour ramps

# Start with the main colours
col3 <- rgb(1,0.7,0,1)
col2 <- rgb(0,0.8,1,1)
col1 <- rgb(0,0,0.5,1)

# The same colours for the time-series lines, with a 98 % transparency
col3l <- rgb(1,0.7,0,0.02)
col2l <- rgb(0,0.8,1,0.02)
col1l <- rgb(0,0,0.5,0.02)

# The same colours for the LPI confidence intervals, with a 60 % transparency
col3t <- rgb(1,0.7,0,0.4)
col2t <- rgb(0,0.8,1,0.4)
col1t <- rgb(0,0,0.5,0.4)

###############################################

# Set up the plot and dimensions
png(filename="FigureS4.png",width=14,height=21,units="cm",res=300)

# A 3 X 2 panel arrangement, with set plot margins
par(mfrow=c(3,2))
par(mai=c(0.5,0.65,0.35,0.08))


#########################
#			                  #
#	        Panel A		    #
#			                  #
#########################

# Plot the axes
plot(0,0,ylim=c(80,180),  type="n", xlim=c(1970,2020), ylab= "Population", xlab="Year",las=1,  cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
# Set the baseline
abline(h=100,col="grey")

# Plot each of the 500 low-fluctuation populatons
for(k in 1:S){
	lines(years,N_50l[k,], col=col1l)
	lines(years,N_100l[k,], col=col2l)
	lines(years,N_150l[k,], col=col3l)
}

# Add the solid lines for the average population
lines(years,Ave_50l,col=col1, lwd=2)
lines(years,Ave_100l,col=col2, lwd=2)
lines(years,Ave_150l,col=col3, lwd=2)

# Label the panel
mtext("a",cex=1.5, side = 3, adj = -0.15, line = 0.8, font=2)

# Add a reference line
abline(h=160,col="grey",lty=2)


#########################
#			                  #
#	        Panel B		    #
#			                  #
#########################

# Plot the axes
plot(0,0,ylim=c(80,180),  type="n", xlim=c(1970,2020), ylab= "Population", xlab="Year",las=1,  cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
# Set the baseline
abline(h=100,col="grey")

# Plot each of the 500 high-fluctuation populatons
for(k in 1:S){
	lines(years,N_50h[k,], col=col1l)
	lines(years,N_100h[k,], col=col2l)
	lines(years,N_150h[k,], col=col3l)
}

# Add the solid lines for the average population
lines(years,Ave_50h,col=col1, lwd=2)
lines(years,Ave_100h,col=col2, lwd=2)
lines(years,Ave_150h,col=col3, lwd=2)

# Label the panel
mtext("b",cex=1.5, side = 3, adj = -0.15, line = 0.8,font=2)

# Add a reference line
abline(h=160,col="grey",lty=2)


#########################
#			                  #
#	        Panel C		    #
#			                  #
#########################

# Plot the axes
plot(0,0,type="n",las=1,xlim=c(1970,2020), ylim=c(0.8,1.8),ylab="LPI (1970 = 1)",xlab="Year", cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
# Add baseline
abline(h=1,col="grey")


# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)), c(lpi_50l$CI_low,rev(lpi_50l$CI_high)),col=col1t,border=NA)
# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)), c(lpi_100l$CI_low,rev(lpi_100l$CI_high)),col=col2t,border=NA)
# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)), c(lpi_150l$CI_low,rev(lpi_150l$CI_high)),col=col3t,border=NA)

# Add mean line
lines(c(1970:2020),lpi_50l$LPI_final,col=col1,lwd=2)
lines(c(1970:2020),lpi_100l$LPI_final,col=col2,lwd=2)
lines(c(1970:2020),lpi_150l$LPI_final,col=col3,lwd=2)

# Label the panel
mtext("c",cex=1.5, side = 3, adj = -0.15, line = 0.8,font=2)

# Add a reference line
abline(h=1.6,col="grey",lty=2)


#########################
#			                  #
#	        Panel D		    #
#			                  #
#########################

# Plot the axes
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0.8,1.8),ylab="LPI (1970 = 1)",xlab="Year", cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))
# Add baseline
abline(h=1,col="grey")


# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)),c(lpi_50h$CI_low,rev(lpi_50h$CI_high)),col=col1t,border=NA)
# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)),c(lpi_100h$CI_low,rev(lpi_100h$CI_high)),col=col2t,border=NA)
# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)),c(lpi_150h$CI_low,rev(lpi_150h$CI_high)),col=col3t,border=NA)

# Add mean line
lines(c(1970:2020),lpi_50h$LPI_final,col=col1,lwd=2)
lines(c(1970:2020),lpi_100h$LPI_final,col=col2,lwd=2)
lines(c(1970:2020),lpi_150h$LPI_final,col=col3,lwd=2)

# Label the panel
mtext("d",cex=1.5, side = 3, adj = -0.15, line = 0.8,font=2)

# Add a reference line
abline(h=1.6,col="grey",lty=2)


#########################
#			                  #
#	        Panel E		    #
#			                  #
#########################

# Plot the axes
plot(0,0,type="n",xlim=c(1.4,1.65), ylim=c(0,50),las=1, main="",ylab="Density", xlab="LPI in 2020 (1970 = 1)",	cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))

# Add the 100 iterations to the plot as a density distibution.
polygon(density(output.mat[,2],bw=0.01), col=col1t,border=NA)
polygon(density(output.mat[,3],bw=0.01), col=col2t,border=NA)
polygon(density(output.mat[,1],bw=0.01), col=col3t,border=NA)

# Add vertical lines for the measures LPI
abline(v=lpi_50l$LPI_final[length(lpi_50l$LPI_final)],lwd=2,lty=1,col=col1)
abline(v=lpi_100l$LPI_final[length(lpi_100l$LPI_final)],lwd=2,lty=1,col=col2)
abline(v=lpi_150l$LPI_final[length(lpi_150l$LPI_final)],lwd=2,lty=1,col=col3)

# Add a vertical line with the actual known LPI
abline(v=1.6,lwd=2,lty=2,col="black")

# Label the panel
mtext("e",cex=1.5, side = 3, adj = -0.15, line = 0.8,font=2)


#########################
#			                  #
#	        Panel F		    #
#			                  #
#########################

# Plot the axes
plot(0,0,type="n",xlim=c(1.4,1.65), ylim=c(0,50),las=1, main="",ylab="Density", xlab="LPI in 2020 (1970 = 1)",cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))

# Add the 100 iterations to the plot as a density distibution.
polygon(density(output.mat[,5],bw=0.01), col=col1t,border=NA)
polygon(density(output.mat[,6],bw=0.01), col=col2t,border=NA)
polygon(density(output.mat[,4],bw=0.01), col=col3t,border=NA)

# Add vertical lines for the measures LPI
abline(v=lpi_50h$LPI_final[length(lpi_50h$LPI_final)],lwd=2,lty=1,col=col1)
abline(v=lpi_100h$LPI_final[length(lpi_100h$LPI_final)],lwd=2,lty=1,col=col2)
abline(v=lpi_150h$LPI_final[length(lpi_150h$LPI_final)],lwd=2,lty=1,col=col3)

# Add a vertical line with the actual known LPI
abline(v=1.6,lwd=2,lty=2,col="black")

# Label the panel
mtext("f",cex=1.5, side = 3, adj = -0.15, line = 0.8,font=2)

# Close plot device and save image
dev.off()


