
# Check the current working directory
getwd()

# Set to the new working directory where raw data files located.
setwd('E:/R/RawData')

# Read txt file of Beijing station regarding latitude info.
bjLat = read.table('BJFS-Lat.txt');
head(bjLat)

# Name each column
names(bjLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(bjLat)

# Read txt file of Beijing station regarding longitute info.
bjLon = read.table('BJFS-Lon.txt');
head(bjLon)

# Name each column
names(bjLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(bjLon)

# Read txt file of Beijing station regarding height info.
bjRad = read.table('BJFS-Rad.txt');
head(bjRad)

# Name each column
names(bjRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(bjRad)

# Combine columns together
BJ <- cbind(bjLat, bjLon, bjRad)
head(BJ)

###comb <- cbind(bjLat, bjLon, bjRad, flLat, flLon, flRad)
###head(comb)

# Remove duplicate columns in BJ 
BJ <- BJ[, !duplicated(colnames(BJ))]
head(BJ)

# Remove unwanted column in BJ 
BJ$Coordinate <- NULL
head(BJ)

# Reorder columns in BJ
BJ <- BJ[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(BJ)

# Plot figures separately.
plot(BJ$Time, BJ$Estimated_Rad)
plot(BJ$Time, BJ$Estimated_Lat)
plot(BJ$Time, BJ$Estimated_Lon)

# Plot figures together.

#par(col.lab = "darkred") # darkred for x and y labels 
#par(col.axis = "black") # black for axis annotation
#par(bg="gray")          # background color

require(graphics)
par(col.axis="black",col.lab="orange",col.main="darkred", bg="gray")
par(pch=20, col="red") # plotting symbol and color 
plot(BJ$Time, BJ$Estimated_Rad, xlab='Time', ylab='Estimated (cm)', ylim = c(-17,18))
par(new=T) # The par(new=T) tells R to make the second plot without cleaning the first.
par(pch=20, col="blue") # plotting symbol and color
plot(BJ$Time, BJ$Estimated_Lat, xlab='Time', ylab='Estimated (cm)', ylim = c(-17,18))
par(new=T)
par(pch=20, col="black") # plotting symbol and color
plot(BJ$Time, BJ$Estimated_Lon, xlab='Time', ylab='Estimated (cm)', ylim = c(-17,18))
par(new=F)
# add a title 
title("Variation for BJFS")

#plot(BJ$Time, BJ$Estigroupvars=c("Height","Latitute","Longitute")，mated_Rad, BJ$Time, BJ$Estimated_Lat, BJ$Time, BJ$Estimated_Lon, main = "Estimation for BJ Statioin", xlab = "Time (year)",   ylab = "Estimated (cm)")

library(ggplot2)
#par(xlab='year', ylab='length (cm)')


ggplot(BJ, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) + 

    
# Define the top and bottom of the errorbars
limits <- aes(ymax = BJ$Estimated_Rad + BJ$Uncertainty_Rad, ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad)

ggplot(BJ, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_errorbar(limits, width=.05)





ggplot(BJ, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) + 
    geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), width=.05)       +
    geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), width=.05)       +
    geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), width=.05)


ggplot(BJ, aes(x=Time, y=Estimated_Rad, colour='blue')) + geom_point(colour="blue", size=3) +
geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), colour="blue", width=.05)

par(new=T)

ggplot(BJ, aes(x=Time, y=Estimated_Lat, colour="red")) + geom_point(colour="red", size=3) +
geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), colour="red", width=.05)

par(new=T)

ggplot(BJ, aes(x=Time, y=Estimated_Lon, colour="green")) + geom_point(colour="green", size=3) +
geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), colour="green", width=.05)


#geom_point() +  xlab='year', ylab='length (cm)', )   + position=pd, 
