# Import the data from the empirical LPI, and the ouputs from the drift models
Global_lpi <-  read.table("LPI_global_empirical.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global1 <-  read.table("LPI_global_drift1.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global3 <-  read.table("LPI_global_drift3.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
Global5<-  read.table("LPI_global_drift5.txt",header=TRUE,sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Set up the colour ramps used for the plot
col3l <- rgb(1,0.7,0,0.02)
col2l <- rgb(0,0.8,1,0.02)
col1l <- rgb(0,0,0.5,0.02)

col3t <- rgb(1,0.7,0,0.4)
col2t <- rgb(0,0.8,1,0.4)
col1t <- rgb(0,0,0.5,0.4)

col3 <- rgb(1,0.7,0,1)
col2 <- rgb(0,0.8,1,1)
col1 <- rgb(0,0,0.5,1)

# Set up the plot and dimensions
png(filename="Figure3.png",width=12,height=24,units="cm",res=300)

# Set the layout, margins and font sizes 
par(mfrow=c(2,1))
par(mai=c(0.8,0.8,0.4,0.1))
par(cex.axis=1.2,cex.lab=1.4)

###############################
#                             #
#           Panel A           #
#                             #
###############################

# Plot the axes
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0,1),ylab="LPI (1970 = 1)",xlab="Year",cex.axis=1.1, cex.lab= 1.4, mgp=c(2.5,0.6,0))

# Add a grey polygon, which shows the dimensions of the next panel
polygon(c(1970,1970,2020,2020),c(0.9,1.02,1.02,0.9), border=NA, col=rgb(0,0,0,0.1))

# Add a polygon for the confidence interval of the empirical LPI, add the mean LPI as a white line.
polygon(c(seq(1970,2016),seq(2016,1970)), c(Global_lpi$CI_low,rev(Global_lpi$CI_high)),col=rgb(0,0.0,0.5,1),border=NA)
lines(c(1970:2016),Global_lpi$LPI_final,col="white",lwd=2)

# Add annotations showing the magnitude of declines
text(2014.5,Global_lpi$LPI_final[length(Global_lpi$LPI_final)],pos=4, "-64%")

# Add the mean LPI for the three drift models: 1%, 3% and 5%. The mean is the average of 100 iterations.
null.mean <- apply(Global1,2,mean)
lines(c(1970:2016),null.mean,col="black",lwd=2, lty=3)

null.mean <- apply(Global3,2,mean)
lines(c(1970:2016),null.mean,col="black",lwd=2, lty=2)

null.mean <- apply(Global5,2,mean)
lines(c(1970:2016),null.mean,col="black",lwd=2, lty=1)

# Add a legend
legend("bottomleft", cex=1.2,col="black", lty=c(3,2,1), c("1% fluctuations","3% fluctuations", "5% fluctuations"))

# Label the panel
mtext("a",cex=1.8, side = 3, adj = -0.1, line = 0.8, font=2)


###############################
#                             #
#           Panel B           #
#                             #
###############################

# Plot the axes
plot(0,0,type="n",las=1,xlim=c(1970,2020),ylim=c(0.9,1),ylab="LPI (1970 = 1)",xlab="Year",cex.axis=1.1, cex.lab= 1.4, mgp=c(2.5,0.6,0))

# Calculate the mean and the inter-quartile range for the 5% drift model
null.mean <- apply(Global5,2,mean)
null.upper <- apply(Global5,2,function(x) {quantile(x,0.75)})
null.lower <- apply(Global5,2,function(x) {quantile(x,0.25)})

# Plot the polygon of the interquartile range, the mean value, and annotate the decline.
polygon(c(seq(1970,2016),seq(2016,1970)), c(null.upper,rev(null.lower)),col=col1t,border=NA)
lines(c(1970:2016),null.mean,col=col1, lty=1,lwd=2)
text(2014.5,null.mean[length(null.mean)],pos=4, "-4.9%")


# Calculate the mean and the inter-quartile range for the 3 % drift model
null.mean <- apply(Global3,2,mean)
null.upper <- apply(Global3,2,function(x) {quantile(x,0.75)})
null.lower <- apply(Global3,2,function(x) {quantile(x,0.25)})

# Plot the polygon of the interquartile range, the mean value, and annotate the decline.
polygon(c(seq(1970,2016),seq(2016,1970)), c(null.upper,rev(null.lower)),col=col2t,border=NA)
lines(c(1970:2016),null.mean,col=col2, lty=2,lwd=2)
text(2014.5,null.mean[length(null.mean)],pos=4, "-1.5%")


# Calculate the mean and the inter-quartile range for the 1 % drift model
null.mean <- apply(Global1,2,mean)
null.upper <- apply(Global1,2,function(x) {quantile(x,0.75)})
null.lower <- apply(Global1,2,function(x) {quantile(x,0.25)})

# Plot the polygon of the interquartile range, the mean value, and annotate the decline.
polygon(c(seq(1970,2016),seq(2016,1970)), c(null.upper,rev(null.lower)),col=col3t,border=NA)
lines(c(1970:2016),null.mean,col=col3, lty=3,lwd=2)
text(2014.5,null.mean[length(null.mean)],pos=4, "-0.2%")

# Add a legend
legend("bottomleft", cex=1.2,col=c(col3t,col2t,col1t), pch=15, c("1% fluctuations","3% fluctuations", "5% fluctuations"))

# Label the panel
mtext("b",cex=1.8, side = 3, adj = -0.1, line = 0.8, font=2)

# Close the plot device and save the plot
dev.off()

