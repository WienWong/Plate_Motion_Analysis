
###
### Weihua Wang 2015-12-16
### Updated version with user defined functions involved.
### Investigate the plate tectonics movement for the eight selected stations. 
###

# Check the current directory
getwd()
dirpath <- setwd('E:/R/RawData')
dirpath

# Import necessary packages
library(xlsx)
library(ggplot2)
library(ggmap)
library(grid)

# paste (getwd(), "/filename", sep = "", collapse = NULL)
# Read the xlsx file in 'D:/Coursera_R/RawData/totalData.xlsx' of 'Sheet1'
total <- read.xlsx(paste(dirpath, "/totalData.xlsx", sep = "", collapse = NULL), "Sheet1")
# Check the first fifth rows
head(total, 5)
class(total)
# "data.frame"

iniYr = 2003
endYr = 2013

# Put all the 8 site names in a string verctor
StationNameVec <- c("BJFS", "MIZU", "NVSK", "ONSA", "FLIN", "HNPT", "SNI1", "MDO1")
StationNameVec[1]
# "BJFS"

fit_Lon <- lm(BJ$Estimated_Lon ~ BJ$Time)
abline(fit_Lon, col="red")

siteMovePerYr <- function(StationName) {
    # Input:  station name in character format; 
    # Return: roughly estimated movements of longitute and latitute in centimeter.
    siteSubSet <- total[which(total$Site == StationName), ]
    # head(siteSubSet, 5)
    max(siteSubSet$Estimated_Lon)
    min(siteSubSet$Estimated_Lon)
    dif_siteSubSet_Lon <- max(siteSubSet$Estimated_Lon) - min(siteSubSet$Estimated_Lon)
    dif_siteSubSet_Lon_yr <- dif_siteSubSet_Lon/(endYr - iniYr) 
    max(siteSubSet$Estimated_Lat)
    min(siteSubSet$Estimated_Lat)
    dif_siteSubSet_Lat <- max(siteSubSet$Estimated_Lat) - min(siteSubSet$Estimated_Lat)
    dif_siteSubSet_Lat_yr <- dif_siteSubSet_Lat/(endYr - iniYr)  
    siteMove_yr <- c(dif_siteSubSet_Lon_yr, dif_siteSubSet_Lat_yr)
    return (siteMove_yr)
    
}

# Function tests
siteMovePerYr(StationNameVec[1])
# 3.301 1.127
siteMovePerYr(StationNameVec[2])
# 31.753 16.859

# Make all station's movement components into a matrix
siteMoveYr <- matrix(c(siteMovePerYr(StationNameVec[1]), siteMovePerYr(StationNameVec[2]), siteMovePerYr(StationNameVec[3]), siteMovePerYr(StationNameVec[4]), siteMovePerYr(StationNameVec[5]), siteMovePerYr(StationNameVec[6]), siteMovePerYr(StationNameVec[7]), siteMovePerYr(StationNameVec[8]) ), nrow=8, ncol=2,  byrow = TRUE)
# Check the matrix
siteMoveYr

# Subseting the data set according to each station's name
# I think this is required for ggplot layer recall. Otherwise I always saw, for example
# plotLLH(StationNameVec[2])
# Error in eval(expr, envir, enclos) : object 'siteSubSet' not found
subSet <- function(StationName){
    
    SubSetOfSite <- total[which(total$Site == as.character(StationName)), ]
    return (SubSetOfSite)
}

# Function test
siteSubSet <- subSet(StationNameVec[2])

# Plot function according to each station' name
plotLLH <- function(StationName){
    # siteSubSet <- subSet(StationName)
    ggplot(siteSubSet) + 
        aes(x=Time, y=Estimated, col=coordinate) + 
        geom_point(aes(x=siteSubSet$Time, y=siteSubSet$Estimated_Rad, col="Height")) + 
        geom_point(aes(x=siteSubSet$Time, y=siteSubSet$Estimated_Lat, col="Latitute")) + 
        geom_point(aes(x=siteSubSet$Time, y=siteSubSet$Estimated_Lon, col="Longitute")) + 
        geom_errorbar(aes(x=siteSubSet$Time, y=siteSubSet$Estimated_Rad, ymax=siteSubSet$Estimated_Rad + siteSubSet$Uncertainty_Rad, ymin=siteSubSet$Estimated_Rad - siteSubSet$Uncertainty_Rad, color="Height", width=.05)) + 
        geom_errorbar(aes(x=siteSubSet$Time, y=siteSubSet$Estimated_Lat, ymax = siteSubSet$Estimated_Lat + siteSubSet$Uncertainty_Lat, ymin=siteSubSet$Estimated_Lat - siteSubSet$Uncertainty_Lat, color="Latitute", width=.05)) + 
        geom_errorbar(aes(x=siteSubSet$Time, y=siteSubSet$Estimated_Lon, ymax = siteSubSet$Estimated_Lon + siteSubSet$Uncertainty_Lon, ymin=siteSubSet$Estimated_Lon - siteSubSet$Uncertainty_Lon, color="Longitute", width=.05)) + 
        theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
        theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + 
        ylab(expression('Length (cm)')) + 
        xlab(expression('Year')) + 
        ggtitle(paste("Estimation with errorbar at ", StationName))
    
}

# Recall plot function for longitute, latitute and height plot with errrobar for MIZU station
plotLLH(StationNameVec[2])

# Rewrite the siteSubSet for BJFS station
siteSubSet <- subSet(StationNameVec[1])
# Longitute, latitute and height plot with errrobar for BJFS station
plotLLH(StationNameVec[1])

siteSubSet <- subSet(StationNameVec[3])
plotLLH(StationNameVec[3])

siteSubSet <- subSet(StationNameVec[4])
plotLLH(StationNameVec[4])

siteSubSet <- subSet(StationNameVec[5])
plotLLH(StationNameVec[5])

siteSubSet <- subSet(StationNameVec[6])
plotLLH(StationNameVec[6])

siteSubSet <- subSet(StationNameVec[7])
plotLLH(StationNameVec[7])

siteSubSet <- subSet(StationNameVec[8])
plotLLH(StationNameVec[8])

#

coordinate <- matrix(data=NA, nrow=8, ncol=2)
coordinate[, 1] <- c(115.53, 141.08, 83.14, 11.55, -101.58, -76.07, -119.31, -104.01) # longitude
coordinate[, 2] <- c(39.36, 39.08, 54.50, 57.23, 54.43, 38.35, 33.15, 30.40)          # latitude
coor <- data.frame(coordinate)            # need data frame not matrix format
names(coor) <- c('longitude','latitude')  # add names 
class(coor$longitude)                     # check if numeric format
coor
sitesName <- c("BJFS", "MIZU", "NVSK", "ONSA", "FLIN", "HNPT", "SNI1", "MDO1")
coor <- cbind(coor, sitesName)
coor

# World map plot with the 8 stations.    
world <- map_data("world")
worldmap <- ggplot() + 
    scale_y_continuous(breaks=(-2:2) * 30) +
    scale_x_continuous(breaks=(-4:4) * 45) +
    geom_polygon(data=world, aes(x=long, y=lat, group = group), fill="slategray3", colour="thistle") +
    geom_point(data=coor, aes(x=coor$long, y=coor$lat), color="coral1") + 
    scale_size(name="Stations") +
    geom_text(data=coor, hjust=0.5, vjust=-0.5, aes(x=coor$long, y=coor$lat, label=coor$sitesName), colour="gold2", size=4)
worldmap

# Add the arrows 
platemove <- worldmap + 
    geom_segment(aes(x=coordinate[, 1][1], y=coordinate[, 2][1], xend=coordinate[, 1][1]+siteMoveYr[1,1], yend=coordinate[, 2][1]-siteMoveYr[1,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][2], y=coordinate[, 2][2], xend=coordinate[, 1][2]+siteMoveYr[2,1], yend=coordinate[, 2][2]-siteMoveYr[2,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][3], y=coordinate[, 2][3], xend=coordinate[, 1][3]+siteMoveYr[3,1], yend=coordinate[, 2][3]-siteMoveYr[3,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][4], y=coordinate[, 2][4], xend=coordinate[, 1][4]+siteMoveYr[4,1], yend=coordinate[, 2][4]+siteMoveYr[4,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][5], y=coordinate[, 2][5], xend=coordinate[, 1][5]-siteMoveYr[5,1], yend=coordinate[, 2][5]-siteMoveYr[5,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][6], y=coordinate[, 2][6], xend=coordinate[, 1][6]-siteMoveYr[6,1], yend=coordinate[, 2][6]+siteMoveYr[6,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][7], y=coordinate[, 2][7], xend=coordinate[, 1][7]-siteMoveYr[7,1], yend=coordinate[, 2][7]+siteMoveYr[7,2]), arrow = arrow(length = unit(0.3,"cm")) )  +
    geom_segment(aes(x=coordinate[, 1][8], y=coordinate[, 2][8], xend=coordinate[, 1][8]-siteMoveYr[8,1], yend=coordinate[, 2][8]-siteMoveYr[8,2]), arrow = arrow(length = unit(0.3,"cm")) ) 

# Display the figure
platemove

#####

# An easy way to plot the stations' moving directions
library(rworldmap)
newmap <- getMap(resolution="low")
plot(newmap)

coordinate <- matrix(data=NA, nrow=8, ncol=2)
coordinate[, 1] <- c(115.53, 141.08, 83.14, 11.55, -101.58, -76.07, -119.31, -104.01) #longitude
coordinate[, 2] <- c(39.36, 39.08, 54.50, 57.23, 54.43, 38.35, 33.15, 30.40)          #latitude
coor <- data.frame(coordinate)                   # need data frame not matrix format
names(coor) <- c('longitude','latitude')         # add names 
class(coor$longitude)                            # check if numeric format
coor
sitesName <- c("BJFS","MIZU", "NVSK", "ONSA", "FLIN", "HNPT", "SNI1", "MDO1")
coor <- cbind(coor, sitesName)
coor

points(coor$longitude, coor$latitude, col = "green", pch=20, cex = 1)
text(coor$longitude, coor$latitude, labels=coor$sitesName, col = "blue", cex = 0.6)
arrows(coordinate[, 1][1], coordinate[, 2][1], coordinate[, 1][1]+siteMoveYr[1,1], coordinate[, 2][1]-siteMoveYr[1,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][2], coordinate[, 2][2], coordinate[, 1][2]+siteMoveYr[2,1], coordinate[, 2][2]-siteMoveYr[2,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][3], coordinate[, 2][3], coordinate[, 1][3]+siteMoveYr[3,1], coordinate[, 2][3]-siteMoveYr[3,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][4], coordinate[, 2][4], coordinate[, 1][4]+siteMoveYr[4,1], coordinate[, 2][4]+siteMoveYr[4,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][5], coordinate[, 2][5], coordinate[, 1][5]-siteMoveYr[5,1], coordinate[, 2][5]-siteMoveYr[5,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][6], coordinate[, 2][6], coordinate[, 1][6]-siteMoveYr[6,1], coordinate[, 2][6]+siteMoveYr[6,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][7], coordinate[, 2][7], coordinate[, 1][7]-siteMoveYr[7,1], coordinate[, 2][7]+siteMoveYr[7,2],angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][8], coordinate[, 2][8], coordinate[, 1][8]-siteMoveYr[8,1], coordinate[, 2][8]-siteMoveYr[8,2],angle = 15,length=0.1,lwd=2,col="red")


####
# Not relate with the project
# The output figure is "terrain.png"
library(RgoogleMaps)
lat <- c(-90,90)      # define our map's ylim
lon <- c(-180,180)    # define our map's xlim
center = c(mean(lat), mean(lon))   # tell what point to center on
zoom <- 2             # zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "terrain.png")  #lots of visual options, just like google maps: maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")

