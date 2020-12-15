# Install package 'devtools' to allow us to instal 'rlpi package from Github
install.packages("devtools")

# Load 'devtools' package
library(devtools)

# Install from main ZSL repository online
install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)

# Load 'rlpi' package
library(rlpi)

# Set the years for the simulation
years <- 1970:2020

# Set the number of species in the simulation
S <- 500

# These are the increments in population size used in the simulation (y-axis in Figure 1d)
N_vect <- seq(50,300,l=50)

# These are the increments in fluctuation size used in the simulation (x-axis in Figure 1d)
Poisson.lambda <- seq(0.5,5,l=50)

# Number of interations
iterations <- 25

# Start by creating a matrix of LPI values for each comnination of startiing population and fluctuation
LPI.mat <- matrix(NA, nrow=length(N_vect),ncol=length(Poisson.lambda))


# Run loops for each combination of population and fluctuation, for each of the iterations.
for (k in 1:length(N_vect)) {
	for (m in 1:length(Poisson.lambda)){

  # Create temporary holder for each iteration
  lpi_iter <- rep(NA,iterations)

    for (n in 1:iterations){

      # Matrix of population values for each species across the length of the simulation
      N <- matrix(NA, nrow=S,ncol=length(years)) ; N[,1] <- rep(N_vect[k],S)

      # Run a loop for each species  
      for (i in 1:S){
        
        # Draw random fluctuations from a Poisson distribution
        flucts <- sample(c(-1,1),length(years)-1,replace=T) * rpois(length(years)-1,Poisson.lambda[m])
        
        # Simulate the time-series
        N[i,] <- c(N_vect[k],N_vect[k] + cumsum(flucts))

        # If the fluctuation causes the population to go extinct, remove negative values to denote extirpation
        if(min(N[i,])<=0) { 
          N[i,c(which(N[i,]<=0)[1]:length(N[i,]))] <- NA
        }
      }

###########################################################

    # Once you have simulate one iteration of S species, calculate the Living Planet Index

    # Assign each time-series a unique ID
    ID <- 1:S
    
    # Assing each species a unique integer ID
    Species <- as.factor(1:S)
    
    # Add the ID and species names as columns to the simulated data
    Pop_50 <- cbind(ID,Species,N)

    # Define the default column names required by the 'rlpi' package
    colnames(Pop_50) <- (c("ID","Binomial",paste0("X",as.factor(years))))

    # This is just an index vector for wich time-series hould be included in calculations
    # Here, we include all time series (Note: this command in needed when you want to use a subset of the data)
    index_vector <- rep(TRUE, S)

    #This creates an 'infile' for calcualting the LPI
    # Note: you need a folder names 'LPI_files' in your working directory
    infile_sim <- create_infile(as.data.frame(Pop_50), start_col_name="X1970", end_col_name="X2020", index_vector=index_vector, name="Surface_infiles/infile_sim")
    lpi_sim <- LPIMain(infile_sim, REF_YEAR = 1970, PLOT_MAX = 2019, BOOT_STRAP_SIZE = 100, VERBOSE=FALSE, plot_lpi=TRUE)

    # The following includes lables to the dedault plots so that you can keep track of the simulation progress
    text(1995,1.9,paste("iteration = ",n))
    text(1995,1.75,paste("Lambda = ",Poisson.lambda[m]))
    text(1995,1.6,paste("N = ",N_vect[k]))

    # This saves the LPI in the final year in the temporarty vector for the iterations
    lpi_iter[n] <- lpi_sim$LPI_final[51]
    }
 
 # This averages the final LPI across the 25 interations and saves it to the output object
 LPI.mat[k,m] <- mean(lpi_iter)
 
 # Add column names to the output object
 colnames(LPI.mat) <- Poisson.lambda

 # Save the output to a file names "LPISpace.txt", which will be used in Figure 1d
 write.table(LPI.mat,file= "LPISpace.txt", quote=T,sep="\t",row.names=F,col.names=T)
 }
}
