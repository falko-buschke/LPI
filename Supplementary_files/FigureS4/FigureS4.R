# Install and load the 'vioplot' package
install.packages("vioplot")
library(vioplot)

###############################

#   Global data

###############################

# Load the Global Living Planet Index
Global_lpi <-  read.table("Outputs/LPI_global_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global1 <-  read.table("Outputs/LPI_global_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global3 <-  read.table("Outputs/LPI_global_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global5<-  read.table("Outputs/LPI_global_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)


###############################

#   Planetary systems

###############################

# Load the Terrestrial Living Planet Index
TR_lpi <-  read.table("Outputs/LPI_terrestrial_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
TR1 <-  read.table("Outputs/LPI_terrestrial_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
TR3 <-  read.table("Outputs/LPI_terrestrial_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
TR5 <-  read.table("Outputs/LPI_terrestrial_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the Freshwater Living Planet Index
FW_lpi <-  read.table("Outputs/LPI_freshwater_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
FW1 <-  read.table("Outputs/LPI_freshwater_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
FW3 <-  read.table("Outputs/LPI_freshwater_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
FW5 <-  read.table("Outputs/LPI_freshwater_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the Marine Living Planet Index
MR_lpi <-  read.table("Outputs/LPI_marine_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
MR1 <-  read.table("Outputs/LPI_marine_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
MR3 <-  read.table("Outputs/LPI_marine_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
MR5 <-  read.table("Outputs/LPI_marine_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)


###############################

#   Biogeographical regions

###############################

# Load the Afrotropics Living Planet Index
Afro_lpi <-  read.table("Outputs/LPI_Afro_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Afro1 <-  read.table("Outputs/LPI_Afro_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Afro3 <-  read.table("Outputs/LPI_Afro_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Afro5 <-  read.table("Outputs/LPI_Afro_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the Nearctic Living Planet Index
Near_lpi <-  read.table("Outputs/LPI_Near_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Near1 <-  read.table("Outputs/LPI_Near_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Near3 <-  read.table("Outputs/LPI_Near_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Near5 <-  read.table("Outputs/LPI_Near_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the Neotropic Living Planet Index
Neo_lpi <-  read.table("Outputs/LPI_Neo_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Neo1 <-  read.table("Outputs/LPI_Neo_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Neo3 <-  read.table("Outputs/LPI_Neo_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Neo5 <-  read.table("Outputs/LPI_Neo_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the Indo-Pacific Living Planet Index
Indo_lpi <-  read.table("Outputs/LPI_Indo_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Indo1 <-  read.table("Outputs/LPI_Indo_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Indo3 <-  read.table("Outputs/LPI_Indo_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Indo5 <-  read.table("Outputs/LPI_Indo_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the Palearctic Living Planet Index
Pale_lpi <-  read.table("Outputs/LPI_Pale_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Pale1 <-  read.table("Outputs/LPI_Pale_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Pale3 <-  read.table("Outputs/LPI_Pale_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Pale5 <-  read.table("Outputs/LPI_Pale_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)


# Combine all the final LPI values from the empirical data in a single object
Obs <- cbind(Global_lpi$LPI_final[dim(Global_lpi)[1]],
			TR_lpi$LPI_final[dim(Global_lpi)[1]],
			FW_lpi$LPI_final[dim(Global_lpi)[1]],
			MR_lpi$LPI_final[dim(Global_lpi)[1]],
			Near_lpi$LPI_final[dim(Global_lpi)[1]],
			Neo_lpi$LPI_final[dim(Global_lpi)[1]],
			Pale_lpi$LPI_final[dim(Global_lpi)[1]],
			Afro_lpi$LPI_final[dim(Global_lpi)[1]],
			Indo_lpi$LPI_final[dim(Global_lpi)[1]])

# Combine all the final LPI values from the 1 % drift data in a single object
Drift1 <- cbind(Global1[,dim(Global)[2]],
			TR1[,dim(TR)[2]],
			FW1[,dim(FW)[2]],
			MR1[,dim(MR)[2]],
			Near1[,dim(Near)[2]],
			Neo1[,dim(Neo)[2]],
			Pale1[,dim(Pale)[2]],
			Afro1[,dim(Afro)[2]],
			Indo1[,dim(Indo)[2]])

# Combine all the final LPI values from the 3 % drift data in a single object
Drift3 <- cbind(Global3[,dim(Global)[2]],
			TR3[,dim(TR)[2]],
			FW3[,dim(FW)[2]],
			MR3[,dim(MR)[2]],
			Near3[,dim(Near)[2]],
			Neo3[,dim(Neo)[2]],
			Pale3[,dim(Pale)[2]],
			Afro3[,dim(Afro)[2]],
			Indo3[,dim(Indo)[2]])

# Combine all the final LPI values from the 5 % drift data in a single object
Drift5 <- cbind(Global5[,dim(Global)[2]],
			TR5[,dim(TR)[2]],
			FW5[,dim(FW)[2]],
			MR5[,dim(MR)[2]],
			Near5[,dim(Near)[2]],
			Neo5[,dim(Neo)[2]],
			Pale5[,dim(Pale)[2]],
			Afro5[,dim(Afro)[2]],
			Indo5[,dim(Indo)[2]])

# Add columns names to the dirft objects
colnames(Drift1) <- c("Global","Terrestrial","Freshwater","Marine","Nearctic", "Neotropical", "Palearctic", "Afrotropical", "Indo-Pacific")
colnames(Drift3) <- c("Global","Terrestrial","Freshwater","Marine", "Nearctic", "Neotropical", "Palearctic", "Afrotropical", "Indo-Pacific")
colnames(Drift5) <- c("Global","Terrestrial","Freshwater","Marine",	"Nearctic", "Neotropical", "Palearctic", "Afrotropical", "Indo-Pacific")


# Define the colour ramp
cols <- colorRampPalette(c(rgb(0,0,0.5,1),rgb(0,0.8,0.8,1),rgb(1,0.5,0,1)),interpolate="linear")(9)

# Define plot name and dimensions
png(filename="FigureS4.png",width=20,height=20,units="cm",res=300)

# Set 3 X 1 panel outlay
par(mfrow=c(3,1))

# Set margins
par(mai=c(0.3,0.8,0.4,0.1))

# Define axis font sizes
par(cex.axis=1.2,cex.lab=1.4)

####################
#                  #
#     Panel A      #
#                  #
####################

# Make violen plot for 1 % drift
vioplot(Drift1, ylim=c(0,1.1),las=1,col=cols, main="",ylab="LPI in 2016 (1970 = 1)",cex.axis=1.1, cex.lab= 1.4, mgp=c(2.5,0.6,0),border=NA)

# Add the empirical LPI as diamond points
points(1:9,Obs, pch=23, col="black",bg=rgb(1,1,1,0.85), cex=2)

# Add baselines and divider lines
abline(v=c(1.5,4.5)); abline(h=1, col="grey",lty=2)

# Label panel
mtext("a",cex=1.5, side = 3, adj = -0.03, line = 0.8, font=2)

# Add legend
legend("bottomright", "Empirical LPI", pch=23, col="black",bg=rgb(1,1,1,0.85), cex=1.4)


####################
#                  #
#     Panel B      #
#                  #
####################

# Make violen plot for 3 % drift
vioplot(Drift3, ylim=c(0,1.1),las=1,col=cols, main="",ylab="LPI in 2016 (1970 = 1)",cex.axis=1.1, cex.lab= 1.4, mgp=c(2.5,0.6,0),border=NA)

# Add the empirical LPI as diamond points
points(1:9,Obs, pch=23, col="black",bg=rgb(1,1,1,0.85), cex=2)

# Add baselines and divider lines
abline(v=c(1.5,4.5)); abline(h=1, col="grey",lty=2)

# Label panel
mtext("b",cex=1.5, side = 3, adj = -0.03, line = 0.8, font=2)

# Add legend
legend("bottomright", "Empirical LPI", pch=23, col="black",bg=rgb(1,1,1,0.85), cex=1.4)


####################
#                  #
#     Panel C      #
#                  #
####################

# Make violen plot for 5 % drift
vioplot(Drift5, ylim=c(0,1.1),las=1,col=cols, main="",ylab="LPI in 2016 (1970 = 1)",cex.axis=1.1, cex.lab= 1.4, mgp=c(2.5,0.6,0),border=NA)

# Add the empirical LPI as diamond points
points(1:9,Obs, pch=23, col="black",bg=rgb(1,1,1,0.85), cex=2)

# Add baselines and divider lines
abline(v=c(1.5,4.5)); abline(h=1, col="grey",lty=2)

# Label panel
mtext("c",cex=1.5, side = 3, adj = -0.03, line = 0.8, font=2)

# Add legend
legend("bottomright", "Empirical LPI", pch=23, col="black",bg=rgb(1,1,1,0.85), cex=1.4)

# Close plot device and save figure to file
dev.off()


