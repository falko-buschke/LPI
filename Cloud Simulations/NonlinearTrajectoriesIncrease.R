# Install and load the 'devtools' package, which is needed to access the 'rlpi' package from GitHub
install.packages("devtools")
library(devtools)

# Install from main ZSL repository online
install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)

# Load the 'rlpi' package
library(rlpi)

#############################################
#                                           #    
#   Simulate the population time-series     #
#                                           #
#############################################

# Set the random seed to replicate the stochastic process in the manuscript
set.seed(42)

# Define the duration of the simulation
years <- 1970:2020

# Set the number of species in the simulation
S <- 500

# Create a dummy variable to standardise the simulation duration between 0 and 1 (used to generate non-linear trajectories)
x <- seq(0,1,l=length(years))

# Generate random noise for the low- and high-fluctuation scenarios for the 500 species. Set the first and last values to zero so all time-series share the same start and end values.
N.low <- matrix(rnorm(n=S*length(years),mean=0,sd=1), nrow=S,ncol=length(years)) ; N.low[,c(1,length(years))] <- 0
N.high <- matrix(rnorm(n=S*length(years),mean=0,sd=7), nrow=S,ncol=length(years)) ; N.high[,c(1,length(years))] <- 0

# Here I used the same naming conventionas in Figure 1. However, here the code '50' refers to a concave-up trajectory, '100' is a linear trajectory, and '150' is a concave-down trajectory
vect_50 <- ((60*(1 + x^5)) + 40)
vect_100 <- ((60*(1 + x^1)) + 40)
vect_150 <- ((60*(1 + x^0.2)) + 40)

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


#####################################################
#                                                   #    
#  Prepare the data for the Living Planet Index     #
#                                                   #
#####################################################

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

######################################
#                                    #    
#  Empirical Living Planet Index     #
#                                    #
######################################

# This is just an index vector for which time-series hould be included in calculations
# Here, we include all time series (Note: this command in needed when you want to use a subset of the data)
index_vector <- rep(TRUE, S)

#This creates an 'infile' for calcualting the LPI
# Note: you need a folder names 'LPI_files' in your working directory
infile_50l <- create_infile(as.data.frame(Pop_50l), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_50l")
lpi_50l <- LPIMain(infile_50l, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_100l <- create_infile(as.data.frame(Pop_100l), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_100l")
lpi_100l <- LPIMain(infile_100l, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_150l <- create_infile(as.data.frame(Pop_150l), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_150l")
lpi_150l <- LPIMain(infile_150l, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)


#This creates an 'infile' for calcualting the LPI
# Note: you need a folder names 'LPI_files' in your working directory
infile_50h <- create_infile(as.data.frame(Pop_50h), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_50h")
lpi_50h <- LPIMain(infile_50h, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_100h <- create_infile(as.data.frame(Pop_100h), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_100h")
lpi_100h <- LPIMain(infile_100h, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

infile_150h <- create_infile(as.data.frame(Pop_150h), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="LPI_files/lpi_150h")
lpi_150h <- LPIMain(infile_150h, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

###############################################################################


###########################################
#                                         #    
#  Function for reshuffling null model    #
#                                         #
###########################################

null.mod <- function(POP.mat){
  N.shuf <- matrix(NA,ncol=dim(POP.mat)[2],nrow=dim(POP.mat)[1])
  
  # Run a loop to reshuffle each population time-series
  for (k in 1:dim(POP.mat)[1]){
    # ISolate the time-series
	  pop.vect <- as.numeric(POP.mat[k,])
    
    # Calculate incremental changes bewtween time-points (i.e. deltas)
	  deltas <- diff(pop.vect)
    
    # Reshuffle the deltas by resamplin without replacement
	  null2 <- sample(deltas,length(deltas),replace=F)
    
    # Simulate a new reshuffled vector
	  new.vect <- c(pop.vect[1],(pop.vect[1] + cumsum(null2)))

    # If a population becomes negative, replace it with a blank value
    if(min(new.vect)<=0){
      new.vect[which(new.vect<=0)] <- NA
    } 
  
    # Assign the resuffled time-series to the new object
    N.shuf[k,] <- new.vect
  }

# Add the default columns to the reshuffled data
POP.shuf <- cbind(ID,Species,N.shuf)

# Add the default column names required by the 'rlpi' package
colnames(POP.shuf) <- (c("ID","Binomial",paste0("X",as.factor(years))))

# Generate the infile and calcualte the Living Planet Index
infile_null <- create_infile(as.data.frame(POP.shuf), start_col_name="X1970", end_col_name="X2020", index_vector=rep(TRUE,500), name="LPI_files/lpi_null")
lpi_null <- LPIMain(infile_null, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

# Identify and report the final LPI value from the reshuffled community
final.val <- lpi_null[dim(lpi_null)[1],1]
final.val
}


########################################################################

############################################
#                         	           #    
#  Iterating the reshuffling null model    #
#                               	   #
############################################

# NUmber of iterations
iterations <- 100

# Create a blank holder-matrix for the 6 assemblages (2 x Fluctuation intensity, 3 x trajectory shape)
output.mat <- matrix(NA,nrow=iterations, ncol=6)

# Iterate the null model in a loop
# The text just adds a laple to the automatically generated plot to keep track of the simulation
for (p in 1:iterations){
	output.mat[p,1] <- null.mod(N_50l)
		text(1995,1.9,paste("iteration = ",p, "val = 1"))
	output.mat[p,2] <- null.mod(N_100l)
		text(1995,1.9,paste("iteration = ",p, "val = 2"))
	output.mat[p,3] <- null.mod(N_150l)
		text(1995,1.9,paste("iteration = ",p, "val = 3"))
	output.mat[p,4] <- null.mod(N_50h)
		text(1995,1.9,paste("iteration = ",p, "val = 4"))
	output.mat[p,5] <- null.mod(N_100h)
		text(1995,1.9,paste("iteration = ",p, "val = 5"))
	output.mat[p,6] <- null.mod(N_150h)
		text(1995,1.9,paste("iteration = ",p, "val = 6"))
}

# Add column names to the iterated output
colnames(output.mat) <- c("ConcaveL", "LinearL","ConvexL","ConcaveH", "LinearH","ConvexH")

# Write the output to file, which can be imported to replicate Figure 4
write.table(output.mat,file= "IterationOutputIncrease.txt",quote=T,sep="\t",row.names=F,col.names=T)
