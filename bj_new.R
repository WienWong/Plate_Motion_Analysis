
# Check the current working directory
getwd()

# Set to the new working directory where raw data files located.
setwd('D:/Coursera_R/RawData')

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

# Remove duplicate columns in BJ 
BJ <- BJ[, !duplicated(colnames(BJ))]
head(BJ)

# Remove unwanted column in BJ 
BJ$Coordinate <- NULL
head(BJ)

# Reorder columns in BJ
BJ <- BJ[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(BJ)

# Plot variation in Longitute
plot(BJ$Time, BJ$Estimated_Lon, xlab="Year", ylab="Variation (cm)", main="Estimated variation in Longitute", col="blue", pch=16)

# Variation in Longitute
max(BJ$Estimated_Lon)
min(BJ$Estimated_Lon)
diff_Lon <- max(BJ$Estimated_Lon) - min(BJ$Estimated_Lon)
dif_Lon_yr <- diff_Lon/(2013-2003)                # deviation in W-E direction is 33.01 / 10 cm 
fit_Lon <- lm(BJ$Estimated_Lon ~ BJ$Time)
abline(fit_Lon, col="red")
legend( x="topleft",                              # places a legend at the appropriate place 
        legend=c("Estimated", "Linear fitted"),   # puts text in the legend
        col=c("blue", "red"), lwd=1, lty=c(NA, 1),# gives the legend appropriate symbols (dots, lines) and color
        pch=c(16,NA) )


# Plot variation in Latitute
plot(BJ$Time, BJ$Estimated_Lat, xlab="Year", ylab="Variation (cm)", main="Estimated variation in Latitute", col="cyan", pch=16)

# Variation in Latitute
max(BJ$Estimated_Lat)
min(BJ$Estimated_Lat)
diff_Lat <- max(BJ$Estimated_Lat) - min(BJ$Estimated_Lat)
dif_Lat_yr <- diff_Lat/(2013-2003)                # deviation in S-N direction is 1.127 cm
fit_Lat <- lm(BJ$Estimated_Lat ~ BJ$Time)
abline(fit_Lat, col="red")

legend( x="topright",                             # places a legend at the appropriate place 
        legend=c("Estimated", "Linear fitted"),   # puts text in the legend
        col=c("cyan", "red"), lwd=1, lty=c(NA, 1),# gives the legend appropriate symbols (dots, lines) and color
        pch=c(16,NA) )

# Plot variation in Height
plot(BJ$Time, BJ$Estimated_Rad, xlab="Year", ylab="Variation (cm)", main="Estimated variation in Height", col="green", pch=16, ylim=c(-3, 3))

# Variation in Height
max(BJ$Estimated_Rad)
min(BJ$Estimated_Rad)
diff_Rad <- max(BJ$Estimated_Rad) - min(BJ$Estimated_Rad)
dif_Rad_yr <- diff_Rad /(2013-2003)                # deviation in height direction is 0.307 cm
fit_Rad <- lm(BJ$Estimated_Rad ~ BJ$Time)
abline(fit_Rad, col="red")
legend( x="bottomleft",                            # places a legend at the appropriate place 
        legend=c("Estimated", "Linear fitted"),    # puts text in the legend
        col=c("green", "red"), lwd=1, lty=c(NA, 1),# gives the legend appropriate symbols (dots, lines) and color
        pch=c(16,NA) )

###
dat1 = read.csv(text = " Site, longitude, latitude
                        BJFS, 115.53,    39.36
                        ONSA,  11.55,    57.23
                        NVSK,  83.14,    54.50
                        MIZU, 141.08,    39.08
                ")

#library(maps)
#library(mapdata)
library(ggplot2)
library(ggmap)
library(grid)
map <- get_map(location='asian', zoom=2, maptype="terrain") 
mp <- ggmap(map)
mp

xendBJ <- dat1$longitude[1] + dif_Lon_yr
xendBJ
yendBJ <- dat1$latitude[1] - dif_Lat_yr
yendBJ

mp + geom_point(data=dat1, aes(x=dat1$longitude,y=dat1$latitude,label=dat1$Site), colour='blue', pch = 20) + geom_text(data = dat1, aes(x=dat1$longitude,y=dat1$latitude,label=dat1$Site), cex = 4, vjust=0, colour="red")  + 
geom_segment(aes(dat1$longitude[1], dat1$latitude[1], xend = xendBJ, yend = yendBJ), color="cyan", arrow = arrow(angle = 30, type="closed",ends="last",length=unit(0.1,"cm")) ) 
# I don't know why the arrow looks like that shape?






# This is an easy way to plot the moving directions
library(rworldmap)
newmap <- getMap(resolution="low")
plot(newmap)

coordinate <- matrix(data=NA, nrow=8, ncol=2)
coordinate[, 1] <- c(115.53, 11.55, 83.14, 141.08, -101.58, -76.07, -119.31, -104.01) #longitude
coordinate[, 2] <- c(39.36, 57.23, 54.50, 39.08, 54.43, 38.35, 33.15, 30.40)  #latitude
coor <- data.frame(coordinate)  # need data frame not matrix format
names(coor) <- c('longitude','latitude') # add names 
class(coor$longitude)          # check if numeric format
coor
sitesName <- c("BJFS", "ONSA", "NVSK", "MIZU", "FLIN", "HNPT", "SNI1", "MDO1")
coor <- cbind(coor, sitesName)
coor


points(coor$longitude, coor$latitude, col = "red", pch=20, cex = 2)
arrows(coordinate[, 1][1], coordinate[, 2][1], coordinate[, 1][1]+dif_Lon_yr, coordinate[, 2][1]-dif_Lat_yr,length=0.1,lwd=3,col="blue")



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

###


ggplot(BJ, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 
    #geom_text(aes(x='Year', y='Length (cm'), color=coordinate)

## Above codes work well


#ggplot(BJ, aes(x=Time, y=Estimated, color = coordinate)) 

ggplot() + 
    geom_point(data=BJ, mapping=aes(x=BJ$Time, y = BJ$Estimated_Rad), col = "blue") + 
    geom_errorbar(data=BJ, mapping=aes(x=BJ$Time, ymax = BJ$Estimated_Rad + BJ$Uncertainty_Rad, ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad), color="blue", width=.05)  +
    geom_point(data=BJ, mapping=aes(x=BJ$Time, y = BJ$Estimated_Lon), col = "red") + 
    geom_errorbar(data=BJ, mapping=aes(x=BJ$Time, ymax = BJ$Estimated_Lon + BJ$Uncertainty_Lon, ymin=BJ$Estimated_Lon - BJ$Uncertainty_Lon), color="red", width=.05)  +
    geom_point(data=BJ, mapping=aes(x=BJ$Time, y = BJ$Estimated_Lat), col = "green") + 
    geom_errorbar(data=BJ, mapping=aes(x=BJ$Time, ymax = BJ$Estimated_Lat + BJ$Uncertainty_Lat, ymin=BJ$Estimated_Lat - BJ$Uncertainty_Lat), color="green", width=.05)  + 
ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at BJFS")



## Above codes work well

### Below codes work well

ggplot(BJ, aes(x=Time, y=Estimated_Rad, colour='blue')) + geom_point(colour="blue", size=3) +
    geom_errorbar(aes(ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad), colour="blue", width=.05)

par(new=T)

ggplot(BJ, aes(x=Time, y=Estimated_Lat, colour="red")) + geom_point(colour="red", size=3) +
    geom_errorbar(aes(ymin=BJ$Estimated_Lat- BJ$Uncertainty_Lat, ymax=BJ$Estimated_Lat + BJ$Uncertainty_Lat), colour="red", width=.05)

par(new=T)

ggplot(BJ, aes(x=Time, y=Estimated_Lon, colour="green")) + geom_point(colour="green", size=3) +
    geom_errorbar(aes(ymin=BJ$Estimated_Lon - BJ$Uncertainty_Lon, ymax=BJ$Estimated_Lon + BJ$Uncertainty_Lon), colour="green", width=.05)


