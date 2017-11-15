
# Satellite HW2-1


# Check the current working directory
getwd()

# Set to the new working directory where raw data files located.
setwd('E:/R/RawData')

# Read txt file of Beijing station regarding latitude info.
bjLat = read.table('BJFS-Lat.txt');
# Check the first few rows info
head(bjLat)
# Name each column
names(bjLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
# Each columns were given proper names 
head(bjLat)

# Read txt file of Beijing station regarding longitute info. (Similar processing procedure here)
bjLon = read.table('BJFS-Lon.txt');
head(bjLon)
names(bjLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(bjLon)

# Read txt file of Beijing station regarding height info. (Similar processing procedure here)
bjRad = read.table('BJFS-Rad.txt');
head(bjRad)
names(bjRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(bjRad)

# Combine all columns together
BJ <- cbind(bjLat, bjLon, bjRad)
head(BJ)

# Remove duplicate columns in BJ station 
BJ <- BJ[, !duplicated(colnames(BJ))]
head(BJ)

# Remove unwanted column in BJ station
BJ$Coordinate <- NULL
head(BJ)

# Reorder columns in BJ station
BJ <- BJ[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(BJ)

# Now we can analysis the data set

# Plot variation in Longitute
plot(BJ$Time, BJ$Estimated_Lon, xlab="Year", ylab="Variation (cm)", main="Estimated variation in Longitute", col="blue", pch=16)

ymin_Lon=BJ$Estimated_Lon - BJ$Uncertainty_Lon
ymax_Lon=BJ$Estimated_Lon + BJ$Uncertainty_Lon

library(Hmisc)
#arrows(BJ$Time, ymin_Lon, BJ$Time, ymax_Lon, length=0.05, angle=90, code=3)
errbar(BJ$Time, BJ$Estimated_Lon, ymax_Lon, ymin_Lon, lwd=1, pch=16, col='blue')


# Variation in Longitute
max(BJ$Estimated_Lon)
min(BJ$Estimated_Lon)
diff_Lon <- max(BJ$Estimated_Lon) - min(BJ$Estimated_Lon)
diff_Lon   # +33.01 cm
# Linear fitting to acquire the coefficients
fit_Lon <- lm(BJ$Estimated_Lon ~ BJ$Time)
fit_Lon
#Coefficients:
#    (Intercept)      BJ$Time  
#     -6333.642        3.154
# Access the slope value
fit_Lon$coeff[2]
# Calculate the angle in radians
angle_Lon=atan(fit_Lon$coeff[2])
# Express in degree (72.40807)
atan(fit_Lon$coeff[2])*180/pi  
# Line fitting
abline(fit_Lon, col="red")
# Legend info
legend( x="topleft",                              # places a legend at the appropriate place 
        legend=c("Estimated", "Linear fitted"),   # puts text in the legend
        col=c("blue", "red"), lwd=1, lty=c(NA, 1),# gives the legend appropriate symbols (dots, lines) and color
        pch=c(16,NA) )


# Plot variation in Latitute (Similar processing procedure here)
plot(BJ$Time, BJ$Estimated_Lat, xlab="Year", ylab="Variation (cm)", main="Estimated variation in Latitute", col="cyan", pch=16)

ymin_Lat=BJ$Estimated_Lat - BJ$Uncertainty_Lat
ymax_Lat=BJ$Estimated_Lat + BJ$Uncertainty_Lat

errbar(BJ$Time, BJ$Estimated_Lat, ymax_Lat, ymin_Lat, lwd=1, pch=16, col='cyan')

# Variation in Latitute
max(BJ$Estimated_Lat)
min(BJ$Estimated_Lat)
diff_Lat <- max(BJ$Estimated_Lat) - min(BJ$Estimated_Lat)
diff_Lat  # +11.27 cm
fit_Lat <- lm(BJ$Estimated_Lat ~ BJ$Time)
fit_Lat
# Access the slope value
fit_Lat$coeff[2]
# Calculate the angle in radians
angle_Lat=atan(fit_Lat$coeff[2])
atan(fit_Lat$coeff[2])*180/pi  # -47.12204  degree
# Line fitting
abline(fit_Lat, col="red")

legend( x="topright", legend=c("Estimated", "Linear fitted"), col=c("cyan", "red"), lwd=1, lty=c(NA, 1), pch=c(16,NA) )

# Plot variation in Height (Similar processing procedure here)
plot(BJ$Time, BJ$Estimated_Rad, xlab="Year", ylab="Variation (cm)", main="Estimated variation in Height", col="green", pch=16, ylim=c(-3, 3))

ymin_Rad=BJ$Estimated_Rad - BJ$Uncertainty_Rad
ymax_Rad=BJ$Estimated_Rad + BJ$Uncertainty_Rad

errbar(BJ$Time, BJ$Estimated_Rad, ymax_Rad, ymin_Rad, lwd=1, pch=16, col='green')

# Variation in Height
max(BJ$Estimated_Rad)
min(BJ$Estimated_Rad)
diff_Rad <- max(BJ$Estimated_Rad) - min(BJ$Estimated_Rad)
diff_Rad   # +3.07 cm
fit_Rad <- lm(BJ$Estimated_Rad ~ BJ$Time)
fit_Rad
abline(fit_Rad, col="red")

legend( x="bottomleft",                             
        legend=c("Estimated", "Linear fitted"),   
        col=c("green", "red"), lwd=1, lty=c(NA, 1),
        pch=c(16,NA) )

### This calculate the BJFS moving direction but not on the map.

theta1=angle_Lon    # 1.263759    in radian
theta2=angle_Lat    #-0.8224347
L1=diff_Lon          # in cm
L2=diff_Lat
x0=c(0,0)
y0=c(0,0)
x1=c(L1*cos(theta1), L1*sin(theta1))
y1=c(L2*cos(theta2), L2*sin(theta2))
magSq <- function(xx) sum(xx^2)
fc=sqrt( magSq(x1) + magSq(y1) + 2*sqrt(magSq(x1)) * sqrt(magSq(y1)) * cos(theta1-theta2) )
fc

Yh=L1*cos(theta1) + L2*cos(theta2)
Xh=L1*sin(theta1) + L2*sin(theta2)
Fh=sqrt(Xh^2 + Yh^2)
Fh

dir=atan( (sqrt(magSq(x1))*sin(theta1) + sqrt(magSq(y1))*sin(theta2) ) / (sqrt(magSq(x1))*cos(theta1) + sqrt(magSq(y1))*cos(theta2)) )
#dir=atan(Yh/Xh)
dir
z0=c(0,0)
z1=c(fc*cos(dir),fc*sin(dir))
ax<-c(-15,35)
plot(ax,ax,main="Test", xlab="Long", ylab="Lat")
arrows(0,0, x1[1], x1[2],lwd=4,col="blue")
arrows(0,0, y1[1], y1[2],lwd=4,col="blue")
arrows(0,0, z1[1], z1[2],lwd=4,col="red")


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
arrows(coordinate[, 1][1], coordinate[, 2][1], coordinate[, 1][1]+z1[1], coordinate[, 2][1]+z1[2],length=0.1,lwd=4,col="red")

# coordinate[, 1][1]+z1[1]  # gives 133.1753




# Plate tectonics movement for the eight selected stations.

#dat = read.csv(text = " Site, longitude, latitude
#                        BJFS, 115.53,    39.36
#                        ONSA,  11.55,    57.23
#                        NVSK,  83.14,    54.50
#                        MIZU, 141.08,    39.08
#                        FLIN, -101.58,   54.43
#                        HNPT, -76.07,    38.35
#                        SNI1, -119.31,   33.15
#                        MDO1, -104.01,   30.40
#               ")
#Warning messages:
#1: Removed 4 rows containing missing values (geom_point). 
#2: Removed 4 rows containing missing values (geom_text). 

dat1 = read.csv(text = " Site, longitude, latitude
                BJFS, 115.53,    39.36
                ONSA,  11.55,    57.23
                NVSK,  83.14,    54.50
                MIZU, 141.08,    39.08
                ")

library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(grid)   # for arrow plot 

map <- get_map(location='eurasian', zoom=3, maptype="terrain") 
mp <- ggmap(map)
mp

class(dat1)
class(dat1$longitude)
class(dat1[, 1])

mp + geom_point(data=dat1, aes(x=dat1$longitude,y=dat1$latitude,label=dat1$Site), colour='blue', pch = 20) + geom_text(data = dat1, aes(x=dat1$longitude,y=dat1$latitude,label=dat1$Site), cex = 4, vjust=0, colour="red")   

dat2 = read.csv(text = "Site, longitude, latitude
                        FLIN, -101.58,   54.43
                        HNPT, -76.07,    38.35
                        SNI1, -119.31,   33.15
                        MDO1, -104.01,   30.40
               ")

map2 <- get_map(location='state', zoom=3, maptype="terrain") 
mp2 <- ggmap(map2)

mp2 + geom_point(data=dat2, aes(x=dat2$longitude,y=dat2$latitude,label=dat2$Site), colour='blue', pch = 20) + geom_text(data = dat2, aes(x=dat2$longitude,y=dat2$latitude,label=dat2$Site), cex = 4, vjust=0, colour="red")  

mp_america <- mp2 + geom_point(data=dat2, aes(x=dat2$longitude,y=dat2$latitude,label=dat2$Site), colour='blue', pch = 20) + geom_text(data = dat2, aes(x=dat2$longitude,y=dat2$latitude,label=dat2$Site), cex = 4, vjust=0, colour="red") 
