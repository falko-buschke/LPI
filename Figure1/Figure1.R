# First instal the package 'devtools' needed to install the 'RLPI' package from GitHub
install.packages("devtools")

# Load 'devtools' package
library(devtools)

# Install from main ZSL repository online
install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
# Load the 'rlpi' package
library(rlpi)

###########################################################################

# Set the random seed so that the stochastic process is identical to the main manuscript
set.seed(41)

# Years of the simulation
years <- 1970:2020

# Number of species in the simulation
S <- 500

# This is the average fluctuation from a random Poisson distribution
Poisson.lambda <- 2

# This can be ignored for now. It is a logical object to be used if the goal is for fluctuations to be proportional to starting population size
rel.sim <- FALSE

# Create three vectors for each community, with different starting populations: 50, 100, 150
N_50 <- matrix(NA, nrow=S,ncol=length(years)) ; N_50[,1] <- rep(50,S)
N_100 <- matrix(NA, nrow=S,ncol=length(years)) ; N_100[,1] <- rep(100,S)
N_150 <- matrix(NA, nrow=S,ncol=length(years)) ; N_150[,1] <- rep(150,S)

# Simulate the populations as they fluctuat according to a random walk
for (i in 2:length(years)){

# This first section is if the fluctutation should be proportional to the starting population. Can be ignored (see line 27)
  if (rel.sim==TRUE) {
    flucts1 <- sample(c(-1,1),S,replace=T) * rpois(S,1)
    flucts2 <- sample(c(-1,1),S,replace=T) * rpois(S,2)
    flucts3 <- sample(c(-1,1),S,replace=T) * rpois(S,3)

    N_50[,i] <- N_50[,i-1] + flucts1
    N_100[,i] <- N_100[,i-1] + flucts2
    N_150[,i] <- N_150[,i-1] + flucts3

# Randomly draw annual fluctuations from a random Poisson distribution. Randomly assign positive and negative values
  } else {
    flucts <- sample(c(-1,1),S,replace=T) * rpois(S,Poisson.lambda)

# Simulate the fluctuatins for each species
    N_50[,i] <- N_50[,i-1] + flucts
    N_100[,i] <- N_100[,i-1] + flucts
    N_150[,i] <- N_150[,i-1] + flucts
  }
}


# This is not relevant to this figure, but this is a block of code that removes negative population values if a population goes extinct due to the demographic fluctuation.
# become relevant if the fluctuation is very large relative to the starting population

# Remove all the extinct populations for N_50
col.ID <- apply(N_50,1,function (x) {(which(x<=0))[1]})
row.ID <- which(!is.na(apply(N_50,1,function (x) {(which(x<=0))[1]})))
if (length(row.ID>0)){
	for (j in row.ID) {
		N_50[j,c(col.ID[j]:length(years))] <- NA
	}
}

# Remove all the extinct populations for N_100
col.ID <- apply(N_100,1,function (x) {(which(x<=0))[1]})
row.ID <- which(!is.na(apply(N_100,1,function (x) {(which(x<=0))[1]})))
if (length(row.ID>0)){
	for (j in row.ID) {
		N_100[j,c(col.ID[j]:length(years))] <- NA
	}
}

# Remove all the extinct populations for N_150
col.ID <- apply(N_150,1,function (x) {(which(x<=0))[1]})
row.ID <- which(!is.na(apply(N_150,1,function (x) {(which(x<=0))[1]})))
if (length(row.ID>0)){
	for (j in row.ID) {
		N_150[j,c(col.ID[j]:length(years))] <- NA
	}
}

# This summarises the average population across the 500 species, to confirm that the community is stable on average.
Ave_50 <- apply(N_50,2,function(x) {mean(x,na.rm=T)})
Ave_100 <- apply(N_100,2,function(x) {mean(x,na.rm=T)})
Ave_150 <- apply(N_150,2,function(x) {mean(x,na.rm=T)})


############################################################
This block of code is simply to show how lambda is non-linear for different populations (used in Panel c)

fluct <- -20:20
lam_50 <- log10((50+fluct)/50)
lam_100 <- log10((100+fluct)/100)
lam_150 <- log10((150+fluct)/150)

###########################################################

# This code is to calculate the Living Planet Index for the simulated data.

# !!! Important, you must have a folder named "LPI_files" in your working directory for this to work. The 'rlpi' code save the population infiles in this folder !!!

# Give each time-series a unique integer ID
ID <- 1:S
# Give each species a unique integer ID
Species <- as.factor(1:S)

# Add the time-series ID to the simulated population data
Pop_50 <- cbind(ID,Species,N_50)
Pop_100 <- cbind(ID,Species,N_100)
Pop_150 <- cbind(ID,Species,N_150)

# Add column names to the dataset. The column names are the defaults needed by the 'rlpi' package.
colnames(Pop_50) <- (c("ID","Binomial",paste0("X",as.factor(years))))
colnames(Pop_100) <- (c("ID","Binomial",paste0("X",as.factor(years))))
colnames(Pop_150) <- (c("ID","Binomial",paste0("X",as.factor(years))))

# This is just an index vector for wich time-serie hould be included in calculations
# Here, we include all time series (Note: this command in needed when you want to use a subset of the data)
index_vector <- rep(TRUE, S)

#This creates an 'infile' for calcualting the LPI
# Note: you need a folder names 'LPI_files' in your working directory
infile_50 <- create_infile(as.data.frame(Pop_50), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_50")
lpi_50 <- LPIMain(infile_50, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_100 <- create_infile(as.data.frame(Pop_100), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_100")
lpi_100 <- LPIMain(infile_100, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_150 <- create_infile(as.data.frame(Pop_150), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_150")
lpi_150 <- LPIMain(infile_150, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

#############################################

# Set up three sets of colours
# These are for the solid lines
col3 <- rgb(1,0.7,0,1)
col2 <- rgb(0,0.8,1,1)
col1 <- rgb(0,0,0.5,1)

# These are for the individual population time-series. Note the 98% transparency
col3l <- rgb(1,0.7,0,0.02)
col2l <- rgb(0,0.8,1,0.02)
col1l <- rgb(0,0,0.5,0.02)

# These are for the error bands around the LPI. Note the 60% transparency
col3t <- rgb(1,0.7,0,0.4)
col2t <- rgb(0,0.8,1,0.4)
col1t <- rgb(0,0,0.5,0.4)


#Define the plot name and dimensions
png(filename="Figure1.png",width=17,height=17,units="cm",res=300)

# 2 X 2 panels, which specific margins
par(mfrow=c(2,2))
par(mai=c(0.6,0.65,0.4,0.10))

  #############################
  #                           #
  #          Panel A          #
  #                           #
  #############################

# Plot the axes and labels
plot(0,0,ylim=c(0,200),  type="n", xlim=c(1970,2020), ylab= "Population", xlab="Year",las=1,  cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))

# Add transparent lines for each population time-series.
for(k in 1:S){
lines(years,N_50[k,], col=col1l)
lines(years,N_100[k,], col=col2l)
lines(years,N_150[k,], col=col3l)
}

# Add the average of the whole community
lines(years,Ave_50,col=col1, lwd=2)
lines(years,Ave_100,col=col2, lwd=2)
lines(years,Ave_150,col=col3, lwd=2)

# Label the panel
mtext("a",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)


  #############################
  #                           #
  #          Panel B          #
  #                           #
  #############################

# Plot the axes and labels
plot(0,0,type="n",las=1,xlim=c(1970,2020),
     ylim=c(0.8,1.2),ylab="LPI (1970 = 1)",xlab="Year", cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))

# Add baseline
abline(h=1,col="grey")


# Include error bars as polygons
polygon(c(seq(1970,2020),seq(2020,1970)), c(lpi_50$CI_low,rev(lpi_50$CI_high)),col=col1t,border=NA)

polygon(c(seq(1970,2020),seq(2020,1970)), c(lpi_100$CI_low,rev(lpi_100$CI_high)),col=col2t,border=NA)

polygon(c(seq(1970,2020),seq(2020,1970)), c(lpi_150$CI_low,rev(lpi_150$CI_high)),col=col3t,border=NA)

# Add mean line
lines(c(1970:2020),lpi_50$LPI_final,col=col1,lwd=2)
lines(c(1970:2020),lpi_100$LPI_final,col=col2,lwd=2)
lines(c(1970:2020),lpi_150$LPI_final,col=col3,lwd=2)

# Add a legend
legend("topright",lty=1,col=c(col3,col2,col1),c("N = 150","N = 100", "N = 50"))

# Label the panel
mtext("b",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)


  #############################
  #                           #
  #          Panel C          #
  #                           #
  #############################

# Plot the axes and labels
plot(0,0,type="n",xlim=c(-20,20),ylim=c(-0.2,0.2),ylab= expression(lambda), xlab="Absolute fluctuation",las=1,  cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))

# Add a baseline
abline(h=0)

# Add the points for lambda estimates
points(fluct,lam_50,col=col1)
points(fluct,lam_100,col=col2)
points(fluct,lam_150,col=col3)

# Add a legend
legend("bottomright",bg="white",pch=1,col=c(col3,col2,col1),c("N = 150","N = 100", "N = 50"))

# Label the panel
mtext("c",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)


  #############################
  #                           #
  #          Panel D          #
  #                           #
  #############################

# Plot the axes and labels
plot(0,0,type="n",las=1,xlim=c(0.5,5),
     ylim=c(50,300),ylab="Starting population",xlab="Average fluctuation", cex.axis=1.1, cex.lab= 1.3, mgp=c(2.5,0.6,0))

# Read the simulated data needed for the surface plot. Be sure that the file "LPISpace.txt" is saved in your working directory
space <-  read.table("LPISpace.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Creat a series of x, y, and z valuess from the imported dataset
yval <- rep(seq(50,300,l=50),each=50)
xval <- rep(seq(.5,5,l=50),50)
zval <- as.vector(as.matrix(t(round(space,2))))

# Number of bins in the colour ramp
bins <- 101 

# The breaks in the z-values for each colour
brks <-  seq(.6, 1.011, length.out = bins+1)

# Group the data based on the breaks
grps <-  cut(zval, breaks = brks, include.lowest = TRUE)

# Define a colour ramp from black to blue to grey
col.ramp <- colorRampPalette(c("black","darkblue","blue","grey90"),interpolate="linear")(bins)

# Add squae points to the polt to creat a surface-plot effect with the appropriate colours.
points(xval,yval,pch=15,cex=0.85,col=col.ramp[grps])

# Add the diagonal lines to show proportional fluctuations as percentages.
abline(a=0,b=100,col="grey10"); text(3,290,"1%",pos=4,col="grey10")
abline(a=0,b=50,col="grey10"); text(4.8,245,"2%",pos=2,col="grey10")
abline(a=0,b=200,col="grey10"); text(1.5,290,"0.5%",pos=4,col="grey10")
abline(a=0,b=33.3333,col="grey10"); text(4.9,165,"3%",pos=2,col="grey10")
abline(a=0,b=25,col="grey10"); text(4.9,125,"4%",pos=2,col="grey10")

# Group the colours for the legend
grps2 <-  cut(c(0.6,0.7,0.8,0.9,1), breaks = brks, include.lowest = TRUE)

# Add the legend
legend(0.5,300,bg="white",title="LPI",rev(c("0.6","0.7","0.8","0.9","1.0")), pch=15, col = rev(col.ramp[grps2]))

# Label the panel
mtext("d",cex=1.3, side = 3, adj = -0.15, line = 1,font=2)

# Save the plot device
dev.off()



