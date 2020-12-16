# Start by installing and loading 'devtools' package, which is needed to instal the 'rlpi' package from GitHub
install.packages("devtools")
library(devtools)

# Install from main ZSL repository online
#install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)
# Load the 'rlpi' package
library(rlpi)

# Open  dataset. This needs to be downloaded from "stats.livingplanetindex.org"
lpi.orig <- read.csv("LPR2020data_public.csv", na.strings = "NULL")

# Define the number of model iterations (note: this will take >24 hours to run!)
iterations <- 100

#########################################################################################

# Create blank-holder for simulation outputs of the Global Living Planet Index
# Here, 47 refers to the number of years in the empirical LPI: 1970-2016.
Global.LPI <- matrix(NA,nrow=iterations,ncol=47)

# Create blank-holders for simulation outputs of the three planetary systems
Terrestrial.LPI <- matrix(NA,nrow=iterations,ncol=47)
Freshwater.LPI <- matrix(NA,nrow=iterations,ncol=47)
Marine.LPI <- matrix(NA,nrow=iterations,ncol=47)

# Create blank-holders for simulation outputs of the five biogeographical realms
Afro.LPI <- matrix(NA,nrow=iterations,ncol=47)
Near.LPI <- matrix(NA,nrow=iterations,ncol=47)
Neo.LPI <- matrix(NA,nrow=iterations,ncol=47)
Pale.LPI <- matrix(NA,nrow=iterations,ncol=47)
Indo.LPI <- matrix(NA,nrow=iterations,ncol=47)

#########################################################################################

#################################
#                               #
#       Run the simulation      #
#                               #
#################################

# Run the iterations as a loop
for (p in 1:iterations) {

  # Isolate the population data from the LP dataset
  pop.mat <- lpi.orig[,30:98]

  # Create a holder matrix for the reshuffled assemblage
  sim.mat <- matrix(NA,ncol=dim(pop.mat)[2],nrow=dim(pop.mat)[1])

  # Run a loop for each population in the LP dataset
  for (k in 1:dim(pop.mat)[1]){

    # Only consider time-series with positive abundance
    if (length(which(!is.na(pop.mat[k,])==TRUE))>1){

        # Identify each time-series incrementally
        pop.vect <- as.numeric(pop.mat[k,])
        
        # Identify an index vector with population data 
        monitor.index <- which(!is.na(pop.vect))

        # Create vector with points in the time-series with population data values
        value.index <- as.numeric(pop.mat[k,monitor.index])

        # Isolate incremental popualtion changes (i.e. deltas) 
        deltas <- diff(value.index)

        # Resample the deltas to creat  a null sequence of population changes 
        null <- sample(deltas,length(deltas),replace=F)

        # Simulate a new time-series with the same starting values as the empirical dataset
        new.vect <- c(value.index[1],(value.index[1] + cumsum(null)))

        # If the population temporary declines below zero, replace negative values with blank data
        if(min(new.vect)<=0){
          new.vect[which(new.vect<=0)] <- NA
        } 
        
        # Add the reshuffled time-series to the whole matrix of population data
        sim.mat[k,monitor.index] <- new.vect
    }
  }


##########################################################################

# Duplicate the empirical LP dataset 
lpi <- lpi.orig

# Replace the empirical population data with the reshuffled time-series
lpi[,30:98] <- sim.mat

# These are the parameters for the LPI calaculation. It is easier to change them here, than in the all the individual functions below

# The maximum year in the dataset (second last year because lambda is the log-ratio with final year too)
max_val <- 2015

# This is the year where the LPI plot end (i.e. the last year in the index)
plot.end <- max_val + 1

# This is the same as the previous year, but includes the 'X', which is read by LPI calculation
end_val <- "X2016"

##########################################################################


#############################################################################
#############################################################################
##                                                                         ##        
##   The following blocks of code is to filter the LP data and group them  ##
##   in terms of taxonomic groups, biogeographical regions, and planetary  ##
##   systems. It is just a repetitive filtering of the dataset.            ##                                                 
##                                                                         ##
#############################################################################
#############################################################################


##########################################################################
# Freshwater

##########################################
# Birds
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$FW_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_birds_Pale <- create_infile(lpi, start_col_name="X1950",end_col_name=end_val, index_vector=ind_vect, name="Global/FW_birds_Pale")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$FW_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_birds_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_birds_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$FW_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_birds_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_birds_Near")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$FW_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_birds_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_birds_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$FW_realm!="Australasia" & lpi$FW_realm!="Indo-Malayan" & lpi$FW_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_birds_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_birds_Indo")


##########################################
# Mammals
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_mamm_Pale <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_mamm_Pale")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_mamm_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_mamm_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_mamm_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_mamm_Near")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_mamm_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_mamm_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Australasia" & lpi$FW_realm!="Indo-Malayan" & lpi$FW_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_mamm_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_mamm_Indo")


##########################################
# Herpetofauna
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_herp_Pale <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_herp_Pale")


ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_herp_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_herp_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_herp_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_herp_Near")


ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_herp_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_herp_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$FW_realm!="Australasia" & lpi$FW_realm!="Indo-Malayan" & lpi$FW_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_herp_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_herp_Indo")


##########################################
# Fish
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$FW_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_fish_Pale <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect,	name="Global/FW_fish_Pale")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$FW_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_fish_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_fish_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$FW_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_fish_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_fish_Near")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$FW_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_fish_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_fish_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Freshwater")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$FW_realm!="Australasia" & lpi$FW_realm!="Indo-Malayan" & lpi$FW_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
FW_fish_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/FW_fish_Indo")

###########################################

# Once you have filtered out the data and created the infile, you can calculate the LPI for the Freshwater System globally
FW_lpi <- LPIMain("Global/FW_infile.txt", PLOT_MAX=max_val, use_weightings_B=1, use_weightings=1, VERBOSE=FALSE)

# This make a plot for the LPI
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0,2),ylab="LPI (1970 = 1)",xlab="Year")
abline(h=1,col="black")
polygon(c(seq(1970,plot.end),seq(plot.end,1970)), c(FW_lpi$CI_low,rev(FW_lpi$CI_high)),col=rgb(0,0.5,1,0.5),border=NA)
lines(c(1970:plot.end),FW_lpi$LPI_final,col="white",lwd=2)
# This adds the iteration number to the plot, to keep track of the simulation
text(1995,1.75,paste(p))

#
###
#####
#######
#####
###
#

########################

# Terrestrial

##########################################
# Birds
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$T_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_birds_Pale <- create_infile(lpi, start_col_name="X1950", end_col_name="X2016", index_vector=ind_vect, name="Global/TR_birds_Pale")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$T_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_birds_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_birds_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$T_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_birds_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_birds_Near")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$T_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_birds_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_birds_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$T_realm!="Australasia" & lpi$T_realm!="Indo-Malayan" & lpi$T_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_birds_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_birds_Indo")

##########################################
# Mammals
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$T_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_mamm_Pale <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_mamm_Pale")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$T_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_mamm_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_mamm_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$T_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_mamm_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_mamm_Near")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$T_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_mamm_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_mamm_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$T_realm!="Australasia" & lpi$T_realm!="Indo-Malayan" & lpi$T_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_mamm_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_mamm_Indo")

##########################################
# Herpetofauna
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$T_realm!="Palearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_herp_Pale <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_herp_Pale")


ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$T_realm!="Afrotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_herp_Afro <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_herp_Afro")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$T_realm!="Nearctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_herp_Near <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect,name="Global/TR_herp_Near")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$T_realm!="Neotropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_herp_Neo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_herp_Neo")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Terrestrial")] <- FALSE
ind_vect[which(lpi$Class!="Amphibia" & lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$T_realm!="Australasia" & lpi$T_realm!="Indo-Malayan" & lpi$FW_realm!="Oceania")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
TR_herp_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/TR_herp_Indo")

###########################################

# Once you have filtered out the data and created the infile, you can calculate the LPI for the Teresstrial System globally
TR_lpi <- LPIMain("Global/TR_infile.txt", PLOT_MAX=max_val, use_weightings_B=1, force_recalculation=0,use_weightings=1, VERBOSE=FALSE)

# This make a plot for the LPI
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0,2),ylab="LPI (1970 = 1)",xlab="Year")
abline(h=1,col="black")
polygon(c(seq(1970,plot.end),seq(plot.end,1970)), c(TR_lpi$CI_low,rev(TR_lpi$CI_high)),col=rgb(0,0.5,1,0.5),border=NA)
lines(c(1970:plot.end),TR_lpi$LPI_final,col="white",lwd=2)
# This adds the iteration number to the plot, to keep track of the simulation
text(1995,1.75,paste(p))

#
###
#####
#######
#####
###
#

# Marine

##########################################
# Birds
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$M_realm!="Arctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_birds_Arctic <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_birds_Arctic")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_birds_ATemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_birds_ATemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic tropical and subtropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_birds_ATrop <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_birds_ATrop")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$M_realm!="Pacific north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_birds_PTemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_birds_PTemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$M_realm!="South temperate and Antarctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_birds_STemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_birds_STemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Aves")] <- FALSE
ind_vect[which(lpi$M_realm!="Tropical and subtropical Indo-Pacific")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_birds_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_birds_Indo")

##########################################
# Mammals
##########################################
ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$M_realm!="Arctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_mamm_Arctic <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_mamm_Arctic")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_mamm_ATemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_mamm_ATemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic tropical and subtropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_mamm_ATrop <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_mamm_ATrop")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$M_realm!="Pacific north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_mamm_PTemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_mamm_PTemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$M_realm!="South temperate and Antarctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_mamm_STemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_mamm_STemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Mammalia")] <- FALSE
ind_vect[which(lpi$M_realm!="Tropical and subtropical Indo-Pacific")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_mamm_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_mamm_Indo")

###########################################

##########################################
# Reptiles
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$M_realm!="Arctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_rep_Arctic <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_rep_Arctic")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_rep_ATemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_rep_ATemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic tropical and subtropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_rep_ATrop <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_rep_ATrop")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$M_realm!="Pacific north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_rep_PTemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_rep_PTemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$M_realm!="South temperate and Antarctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_rep_STemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_rep_STemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Reptilia")] <- FALSE
ind_vect[which(lpi$M_realm!="Tropical and subtropical Indo-Pacific")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_rep_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_rep_Indo")

###########################################

##########################################
# Fish
##########################################

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$M_realm!="Arctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_fish_Arctic <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_fish_Arctic")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_fish_ATemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_fish_ATemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$M_realm!="Atlantic tropical and subtropical")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_fish_ATrop <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_fish_ATrop")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$M_realm!="Pacific north temperate")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_fish_PTemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_fish_PTemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$M_realm!="South temperate and Antarctic")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_fish_STemp <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_fish_STemp")

ind_vect <- rep(TRUE,nrow(lpi))
ind_vect[which(lpi$System!="Marine")] <- FALSE
ind_vect[which(lpi$Class!="Actinopteri" &
	lpi$Class!="Coelacanthi" &
	lpi$Class!="Dipneusti" &
	lpi$Class!="Elasmobranchii" &
	lpi$Class!="Holocephali" &
 	lpi$Class!="Myxini" &
	lpi$Class!="Petromyzonti")] <- FALSE
ind_vect[which(lpi$M_realm!="Tropical and subtropical Indo-Pacific")] <- FALSE
# This creates and save the Infile needed by the LPI function. Must be saved in a folder named "Global" in the working directory.
M_fish_Indo <- create_infile(lpi, start_col_name="X1950", end_col_name=end_val, index_vector=ind_vect, name="Global/M_fish_Indo")

###########################################

# Once you have filtered out the data and created the infile, you can calculate the LPI for the Marine System globally
MR_lpi <- LPIMain("Global/MR_infile.txt", PLOT_MAX=max_val, use_weightings_B=1, use_weightings=1, VERBOSE=FALSE)

# This make a plot for the LPI
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0,2),ylab="LPI (1970 = 1)",xlab="Year")
abline(h=1,col="black")
polygon(c(seq(1970,plot.end),seq(plot.end,1970)), c(MR_lpi$CI_low,rev(MR_lpi$CI_high)),col=rgb(0,0.5,1,0.5),border=NA)
lines(c(1970:plot.end),MR_lpi$LPI_final,col="white",lwd=2)
# This adds the iteration number to the plot, to keep track of the simulation
text(1995,1.75,paste(p))

###########################################
###########################################
###########################################
###########################################

# Once you have filtered out the data and created the infile, you can calculate the Global LPI
Global_lpi <- LPIMain("Global/Global_infile.txt", PLOT_MAX=max_val, use_weightings_B=1,use_weightings=1, VERBOSE=FALSE)

# This make a plot for the LPI
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0,2),ylab="LPI (1970 = 1)",xlab="Year")
abline(h=1,col="black")
polygon(c(seq(1970,plot.end),seq(plot.end,1970)), c(Global_lpi$CI_low,rev(Global_lpi$CI_high)),col=rgb(0,0.5,1,0.5),border=NA)
lines(c(1970:plot.end),Global_lpi$LPI_final,col="white",lwd=2)
# This adds the iteration number to the plot, to keep track of the simulation
text(1995,1.75,paste(p))

###########################################
###########################################
###########################################
###########################################

# Once you have filtered out the data and created the infile, you can calculate the five Biogeographical Realms

Afro_lpi <- LPIMain("Global/Afro_infile.txt", PLOT_MAX=max_val, use_weightings_B=0,use_weightings=1, VERBOSE=FALSE)
Near_lpi <- LPIMain("Global/Near_infile.txt", PLOT_MAX=max_val, use_weightings_B=0,use_weightings=1, VERBOSE=FALSE)
Neo_lpi <- LPIMain("Global/Neo_infile.txt", PLOT_MAX=max_val, use_weightings_B=0,use_weightings=1, VERBOSE=FALSE)
Pale_lpi <- LPIMain("Global/Pale_infile.txt", PLOT_MAX=max_val, use_weightings_B=0,use_weightings=1, VERBOSE=FALSE)
Indo_lpi <- LPIMain("Global/Indo_infile.txt", PLOT_MAX=max_val, use_weightings_B=0,use_weightings=1, VERBOSE=FALSE)

###########################################
###########################################
###########################################
###########################################

# Save the final LPI estimates for each iteration of the Global LPI
Global.LPI[p,]  <- Global_lpi$LPI_final

# Save the final LPI estimates for each iteration of the three planetary systems
Terrestrial.LPI[p,] <- TR_lpi$LPI_final
Freshwater.LPI[p,] <- FW_lpi$LPI_final
Marine.LPI[p,] <- MR_lpi$LPI_final

# Save the final LPI estimates for each iteration of the five biogeographical regions
Afro.LPI[p,] <- Afro_lpi$LPI_final
Near.LPI[p,] <- Near_lpi$LPI_final
Neo.LPI[p,] <- Neo_lpi$LPI_final
Pale.LPI[p,] <- Pale_lpi$LPI_final
Indo.LPI[p,] <- Indo_lpi$LPI_final
	
###########################################
###########################################
###########################################
###########################################	

# Write the global LPI to a text file.
write.table(Global.LPI,file= "LPI_global_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)

# Write all the LPI values to file for the three planetary systems

write.table(Terrestrial.LPI,file= "LPI_terrestrial_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
write.table(Freshwater.LPI,file= "LPI_freshwater_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
write.table(Marine.LPI,file= "LPI_marine_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)

# Write all the LPI values to file for the five biogeographical realms
write.table(Afro.LPI,file= "LPI_Afro_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
write.table(Near.LPI,file= "LPI_Near_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
write.table(Neo.LPI,file= "LPI_Neo_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
write.table(Pale.LPI,file= "LPI_Pale_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
write.table(Indo.LPI,file= "LPI_Indo_null.txt",	quote=T,sep="\t",row.names=F,col.names=T)
}
