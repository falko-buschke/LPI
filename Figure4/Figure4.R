# Begin by installing and loading the 'vioplot' package to make violin plots
install.packages("vioplot")
library(vioplot)

#####################################
#                                   #
#             Global data           #
#                                   #
#####################################

# Load the data for the empirical and reshuffled global LPI
Global_lpi <-  read.table("LPI_global_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global <-  read.table("LPI_global_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)


#####################################
#                                   #
#         Planetary systems         #
#                                   #
#####################################

# Load the data for the empirical and reshuffled terrestrial LPI
TR_lpi <-  read.table("LPI_terrestrial_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
TR <-  read.table("LPI_terrestrial_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the data for the empirical and reshuffled freshwater LPI
FW_lpi <-  read.table("LPI_freshwater_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
FW <-  read.table("LPI_freshwater_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the data for the empirical and reshuffled marine LPI
MR_lpi <-  read.table("LPI_marine_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
MR <-  read.table("LPI_marine_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)


#####################################
#                                   #
#     Biogeographical realms        #
#                                   #
#####################################

# Load the data for the empirical and reshuffled Afrotropical LPI
Afro_lpi <-  read.table("LPI_Afro_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Afro <-  read.table("LPI_Afro_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the data for the empirical and reshuffled Nearctic LPI
Near_lpi <-  read.table("LPI_Near_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Near <-  read.table("LPI_Near_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the data for the empirical and reshuffled Neotropical LPI
Neo_lpi <-  read.table("LPI_Neo_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Neo <-  read.table("LPI_Neo_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the data for the empirical and reshuffled INdo-Pacific LPI
Indo_lpi <-  read.table("LPI_Indo_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Indo <-  read.table("LPI_Indo_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Load the data for the empirical and reshuffled Palearctic LPI
Pale_lpi <-  read.table("LPI_Pale_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Pale <-  read.table("LPI_Pale_null1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

#########################################################################

# Combine the final LPI as columns of the simulated data 

Nulls <- cbind(Global[,dim(Global)[2]],
			TR[,dim(TR)[2]],
			FW[,dim(FW)[2]],
			MR[,dim(MR)[2]],
			Near[,dim(Near)[2]],
			Neo[,dim(Neo)[2]],
			Pale[,dim(Pale)[2]],
			Afro[,dim(Afro)[2]],
			Indo[,dim(Indo)[2]])

# Add columns names to the simulated data
colnames(Nulls) <- c("Global","Terrestrial","Freshwater","Marine",
	"Nearctic", "Neotropical", "Palearctic", "Afrotropical", "Indo-Pacific")


#########################################################################

# Combine the final LPI as columns of the empirical (i.e. Observed) data 

Obs <- cbind(Global_lpi$LPI_final[dim(Global_lpi)[1]],
			TR_lpi$LPI_final[dim(Global_lpi)[1]],
			FW_lpi$LPI_final[dim(Global_lpi)[1]],
			MR_lpi$LPI_final[dim(Global_lpi)[1]],
			Near_lpi$LPI_final[dim(Global_lpi)[1]],
			Neo_lpi$LPI_final[dim(Global_lpi)[1]],
			Pale_lpi$LPI_final[dim(Global_lpi)[1]],
			Afro_lpi$LPI_final[dim(Global_lpi)[1]],
			Indo_lpi$LPI_final[dim(Global_lpi)[1]])

nulls.med <- apply(Nulls,2,median)
emp.val <- Obs

dif <- round(nulls.med-emp.val,digits=3)

# Define the colour ramp
cols <- colorRampPalette(c(rgb(0,0,0.8,1),rgb(0,0.8,0.8,1),rgb(1,0.5,0,1)),interpolate="linear")(9)


# Set up the plot and dimensions
png(filename="Figure4.png",width=28,height=12,units="cm",res=300)

# Define plot margins
par(mai=c(0.4,0.8,0.1,0.1))

# Make the base violin plot of the simulated data (replicates are iterations)
vioplot(Nulls, ylim=c(0,1.4),las=1,col=cols, main="",ylab="LPI in 2016 (1970 = 1)",cex.axis=1, cex.lab= 1.6, mgp=c(1.6,0.6,0),border=NA)

# Add the empirical values as diamonds
points(1:9,Obs, pch=23, col="black",bg=rgb(1,1,1,0.85), cex=2)

# Add vertical lines to separate the components of the plot
abline(v=c(1.5,4.5)); abline(h=1, col="grey",lty=2)

# Label the sub-panels
text (c(0.3,1.65,4.65),c(1.3,1.3,1.3), c("a","b","c"),cex=1.5, font=2,pos=3)

# Add a legend
legend("topright", "Empirical LPI", pch=23, col="black",bg=rgb(1,1,1,0.85), cex=1.2)
text(1:9,rep(0,9),dif, col=ifelse(dif>0,"blue","red"))
# Close plot device and save image to file
dev.off()
