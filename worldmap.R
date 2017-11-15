

###
### Below codes plot the plate tectonics movement for the eight selected stations.
###

getwd()
setwd('E:/R/RawData')
library(rJava)
library(xlsx)
library(ggplot2)
library(ggmap)
total <- read.xlsx("E:/R/RawData/totalData.xlsx", "Sheet1")
head(total, 5)
class(total)
# "data.frame"

# Subset data according to the Beijing station
BJ <- total[which(total$Site == "BJFS"), ]
head(BJ, 5)
max(BJ$Estimated_Lon)
min(BJ$Estimated_Lon)
difBJ_Lon <- max(BJ$Estimated_Lon) - min(BJ$Estimated_Lon)
difBJ_Lon_yr <- difBJ_Lon/(2013-2003) 
max(BJ$Estimated_Lat)
min(BJ$Estimated_Lat)
difBJ_Lat <- max(BJ$Estimated_Lat) - min(BJ$Estimated_Lat)
difBJ_Lat_yr <- difBJ_Lat/(2013-2003) 

#ymin_BJLat=BJ$Estimated_Lat - BJ$Uncertainty_Lat
#ymax_BJLat=BJ$Estimated_Lat + BJ$Uncertainty_Lat

ggplot(BJ)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=BJ$Time, y=BJ$Estimated_Rad, col="Height")) +
    geom_point(aes(x=BJ$Time, y=BJ$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=BJ$Time, y=BJ$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=BJ$Time, y=BJ$Estimated_Rad, ymax=BJ$Estimated_Rad + BJ$Uncertainty_Rad, ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=BJ$Time, y=BJ$Estimated_Lat, ymax = BJ$Estimated_Lat + BJ$Uncertainty_Lat, ymin=BJ$Estimated_Lat - BJ$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=BJ$Time, y=BJ$Estimated_Lon, ymax = BJ$Estimated_Lon + BJ$Uncertainty_Lon, ymin=BJ$Estimated_Lon - BJ$Uncertainty_Lon, color="Longitute", width=.05))  + 
theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at BJFS")



#ggplot() + 
#geom_point(data=BJ, mapping=aes(x=BJ$Time, y = BJ$Estimated_Rad), col = "blue") + 
#geom_errorbar(data=BJ, mapping=aes(x=BJ$Time, ymax = BJ$Estimated_Rad + BJ$Uncertainty_Rad, ymin=BJ$Estimated_Rad - BJ$Uncertainty_Rad), color="blue", width=.05)  +
#geom_point(data=BJ, mapping=aes(x=BJ$Time, y = BJ$Estimated_Lon), col = "red") + 
#geom_errorbar(data=BJ, mapping=aes(x=BJ$Time, ymax = BJ$Estimated_Lon + BJ$Uncertainty_Lon, ymin=BJ$Estimated_Lon - BJ$Uncertainty_Lon), color="red", width=.05)  +
#geom_point(data=BJ, mapping=aes(x=BJ$Time, y = BJ$Estimated_Lat), col = "green") + 
#geom_errorbar(data=BJ, mapping=aes(x=BJ$Time, ymax = BJ$Estimated_Lat + BJ$Uncertainty_Lat, ymin=BJ$Estimated_Lat - BJ$Uncertainty_Lat), color="green", width=.05)  


# Subset data according to the MIZU station
MZ <- total[which(total$Site == "MIZU"), ]
head(MZ, 5)
max(MZ$Estimated_Lon)
min(MZ$Estimated_Lon)
difMZ_Lon <- max(MZ$Estimated_Lon) - min(MZ$Estimated_Lon)
difMZ_Lon_yr <- difMZ_Lon/(2013-2003) 
max(MZ$Estimated_Lat)
min(MZ$Estimated_Lat)
difMZ_Lat <- max(MZ$Estimated_Lat) - min(MZ$Estimated_Lat)
difMZ_Lat_yr <- difMZ_Lat/(2013-2003) 

ggplot(MZ)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=MZ$Time, y=MZ$Estimated_Rad, col="Height")) +
    geom_point(aes(x=MZ$Time, y=MZ$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=MZ$Time, y=MZ$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=MZ$Time, y=MZ$Estimated_Rad, ymax=MZ$Estimated_Rad + MZ$Uncertainty_Rad, ymin=MZ$Estimated_Rad - MZ$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=MZ$Time, y=MZ$Estimated_Lat, ymax = MZ$Estimated_Lat + MZ$Uncertainty_Lat, ymin=MZ$Estimated_Lat - MZ$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=MZ$Time, y=MZ$Estimated_Lon, ymax = MZ$Estimated_Lon + MZ$Uncertainty_Lon, ymin=MZ$Estimated_Lon - MZ$Uncertainty_Lon, color="Longitute", width=.05))  + 
theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at MIZU")

# Subset data according to the NVSK station
NV <- total[which(total$Site == "NVSK"), ]
head(NV, 5)
max(NV$Estimated_Lon)
min(NV$Estimated_Lon)
difNV_Lon <- max(NV$Estimated_Lon) - min(NV$Estimated_Lon)
difNV_Lon_yr <- difNV_Lon/(2013-2003) 
max(NV$Estimated_Lat)
min(NV$Estimated_Lat)
difNV_Lat <- max(NV$Estimated_Lat) - min(NV$Estimated_Lat)
difNV_Lat_yr <- difNV_Lat/(2013-2003) 

ggplot(NV)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=NV$Time, y=NV$Estimated_Rad, col="Height")) +
    geom_point(aes(x=NV$Time, y=NV$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=NV$Time, y=NV$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=NV$Time, y=NV$Estimated_Rad, ymax=NV$Estimated_Rad + NV$Uncertainty_Rad, ymin=NV$Estimated_Rad - NV$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=NV$Time, y=NV$Estimated_Lat, ymax = NV$Estimated_Lat + NV$Uncertainty_Lat, ymin=NV$Estimated_Lat - NV$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=NV$Time, y=NV$Estimated_Lon, ymax = NV$Estimated_Lon + NV$Uncertainty_Lon, ymin=NV$Estimated_Lon - NV$Uncertainty_Lon, color="Longitute", width=.05))  + 
    theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
    theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at NVSK")

# Subset data according to the ONSA station
ON <- total[which(total$Site == "ONSA"), ]
head(ON, 5)
max(ON$Estimated_Lon)
min(ON$Estimated_Lon)
difON_Lon <- max(ON$Estimated_Lon) - min(ON$Estimated_Lon)
difON_Lon_yr <- difON_Lon/(2013-2003) 
max(ON$Estimated_Lat)
min(ON$Estimated_Lat)
difON_Lat <- max(ON$Estimated_Lat) - min(ON$Estimated_Lat)
difON_Lat_yr <- difON_Lat/(2013-2003) 

ggplot(ON)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=ON$Time, y=ON$Estimated_Rad, col="Height")) +
    geom_point(aes(x=ON$Time, y=ON$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=ON$Time, y=ON$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=ON$Time, y=ON$Estimated_Rad, ymax=ON$Estimated_Rad + ON$Uncertainty_Rad, ymin=ON$Estimated_Rad - ON$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=ON$Time, y=ON$Estimated_Lat, ymax = ON$Estimated_Lat + ON$Uncertainty_Lat, ymin=ON$Estimated_Lat - ON$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=ON$Time, y=ON$Estimated_Lon, ymax = ON$Estimated_Lon + ON$Uncertainty_Lon, ymin=ON$Estimated_Lon - ON$Uncertainty_Lon, color="Longitute", width=.05))  + 
    theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
    theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at ONSA")

# Subset data according to the FLIN station
FL <- total[which(total$Site == "FLIN"), ]
head(FL, 5)
max(FL$Estimated_Lon)
min(FL$Estimated_Lon)
difFL_Lon <- max(FL$Estimated_Lon) - min(FL$Estimated_Lon)
difFL_Lon_yr <- difFL_Lon/(2013-2003) 
max(FL$Estimated_Lat)
min(FL$Estimated_Lat)
difFL_Lat <- max(FL$Estimated_Lat) - min(FL$Estimated_Lat)
difFL_Lat_yr <- difFL_Lat/(2013-2003) 

ggplot(FL)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=FL$Time, y=FL$Estimated_Rad, col="Height")) +
    geom_point(aes(x=FL$Time, y=FL$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=FL$Time, y=FL$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=FL$Time, y=FL$Estimated_Rad, ymax=FL$Estimated_Rad + FL$Uncertainty_Rad, ymin=FL$Estimated_Rad - FL$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=FL$Time, y=FL$Estimated_Lat, ymax = FL$Estimated_Lat + FL$Uncertainty_Lat, ymin=FL$Estimated_Lat - FL$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=FL$Time, y=FL$Estimated_Lon, ymax = FL$Estimated_Lon + FL$Uncertainty_Lon, ymin=FL$Estimated_Lon - FL$Uncertainty_Lon, color="Longitute", width=.05))  + 
    theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
    theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at FLIN")


# Subset data according to the HNPT station
HN <- total[which(total$Site == "HNPT"), ]
head(HN, 5)
max(HN$Estimated_Lon)
min(HN$Estimated_Lon)
difHN_Lon <- max(HN$Estimated_Lon) - min(HN$Estimated_Lon)
difHN_Lon_yr <- difHN_Lon/(2013-2003) 
max(HN$Estimated_Lat)
min(HN$Estimated_Lat)
difHN_Lat <- max(HN$Estimated_Lat) - min(HN$Estimated_Lat)
difHN_Lat_yr <- difHN_Lat/(2013-2003) 

ggplot(HN)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=HN$Time, y=HN$Estimated_Rad, col="Height")) +
    geom_point(aes(x=HN$Time, y=HN$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=HN$Time, y=HN$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=HN$Time, y=HN$Estimated_Rad, ymax=HN$Estimated_Rad + HN$Uncertainty_Rad, ymin=HN$Estimated_Rad - HN$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=HN$Time, y=HN$Estimated_Lat, ymax = HN$Estimated_Lat + HN$Uncertainty_Lat, ymin=HN$Estimated_Lat - HN$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=HN$Time, y=HN$Estimated_Lon, ymax = HN$Estimated_Lon + HN$Uncertainty_Lon, ymin=HN$Estimated_Lon - HN$Uncertainty_Lon, color="Longitute", width=.05))  + 
    theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
    theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at HNPT")

# Subset data according to the SNI1 station
SN <- total[which(total$Site == "SNI1"), ]
head(SN, 5)
max(SN$Estimated_Lon)
min(SN$Estimated_Lon)
difSN_Lon <- max(SN$Estimated_Lon) - min(SN$Estimated_Lon)
difSN_Lon_yr <- difSN_Lon/(2013-2003) 
max(SN$Estimated_Lat)
min(SN$Estimated_Lat)
difSN_Lat <- max(SN$Estimated_Lat) - min(SN$Estimated_Lat)
difSN_Lat_yr <- difSN_Lat/(2013-2003) 

ggplot(SN)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=SN$Time, y=SN$Estimated_Rad, col="Height")) +
    geom_point(aes(x=SN$Time, y=SN$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=SN$Time, y=SN$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=SN$Time, y=SN$Estimated_Rad, ymax=SN$Estimated_Rad + SN$Uncertainty_Rad, ymin=SN$Estimated_Rad - SN$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=SN$Time, y=SN$Estimated_Lat, ymax = SN$Estimated_Lat + SN$Uncertainty_Lat, ymin=SN$Estimated_Lat - SN$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=SN$Time, y=SN$Estimated_Lon, ymax = SN$Estimated_Lon + SN$Uncertainty_Lon, ymin=SN$Estimated_Lon - SN$Uncertainty_Lon, color="Longitute", width=.05))  + 
    theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
    theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at SNI1")

# Subset data according to the MDO1 station
MD <- total[which(total$Site == "MDO1"), ]
head(MD, 5)
max(MD$Estimated_Lon)
min(MD$Estimated_Lon)
difMD_Lon <- max(MD$Estimated_Lon) - min(MD$Estimated_Lon)
difMD_Lon_yr <- difMD_Lon/(2013-2003) 
max(MD$Estimated_Lat)
min(MD$Estimated_Lat)
difMD_Lat <- max(MD$Estimated_Lat) - min(MD$Estimated_Lat)
difMD_Lat_yr <- difMD_Lat/(2013-2003) 

ggplot(MD)+
    aes(x=Time, y=Estimated, col=coordinate) +   
    geom_point(aes(x=MD$Time, y=MD$Estimated_Rad, col="Height")) +
    geom_point(aes(x=MD$Time, y=MD$Estimated_Lat, col="Latitute")) +
    geom_point(aes(x=MD$Time, y=MD$Estimated_Lon, col="Longitute")) +
    geom_errorbar(aes(x=MD$Time, y=MD$Estimated_Rad, ymax=MD$Estimated_Rad + MD$Uncertainty_Rad, ymin=MD$Estimated_Rad - MD$Uncertainty_Rad, color="Height", width=.05))     +
    geom_errorbar(aes(x=MD$Time, y=MD$Estimated_Lat, ymax = MD$Estimated_Lat + MD$Uncertainty_Lat, ymin=MD$Estimated_Lat - MD$Uncertainty_Lat, color="Latitute", width=.05))   +
    geom_errorbar(aes(x=MD$Time, y=MD$Estimated_Lon, ymax = MD$Estimated_Lon + MD$Uncertainty_Lon, ymin=MD$Estimated_Lon - MD$Uncertainty_Lon, color="Longitute", width=.05))  + 
    theme(legend.title = element_text(colour="blue", size=14, face="bold")) + 
    theme(legend.text = element_text(colour="black", size = 12, face = "bold")) + ylab(expression('Length (cm)')) + xlab(expression('Year')) + ggtitle("Estimation with errorbar at MDO1")

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
            geom_segment(aes(x=coordinate[, 1][1], y=coordinate[, 2][1], xend=coordinate[, 1][1]+difBJ_Lon_yr, yend=coordinate[, 2][1]-difBJ_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][2], y=coordinate[, 2][2], xend=coordinate[, 1][2]+difMZ_Lon_yr, yend=coordinate[, 2][2]-difMZ_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][3], y=coordinate[, 2][3], xend=coordinate[, 1][3]+difNV_Lon_yr, yend=coordinate[, 2][3]-difNV_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][4], y=coordinate[, 2][4], xend=coordinate[, 1][4]+difON_Lon_yr, yend=coordinate[, 2][4]+difON_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][5], y=coordinate[, 2][5], xend=coordinate[, 1][5]-difFL_Lon_yr, yend=coordinate[, 2][5]-difFL_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][6], y=coordinate[, 2][6], xend=coordinate[, 1][6]-difHN_Lon_yr, yend=coordinate[, 2][6]+difHN_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][7], y=coordinate[, 2][7], xend=coordinate[, 1][7]-difSN_Lon_yr, yend=coordinate[, 2][7]+difSN_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) )  +
            geom_segment(aes(x=coordinate[, 1][8], y=coordinate[, 2][8], xend=coordinate[, 1][8]-difMD_Lon_yr, yend=coordinate[, 2][8]-difMD_Lat_yr), arrow = arrow(length = unit(0.3,"cm")) ) 

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
arrows(coordinate[, 1][1], coordinate[, 2][1], coordinate[, 1][1]+difBJ_Lon_yr, coordinate[, 2][1]-difBJ_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][2], coordinate[, 2][2], coordinate[, 1][2]+difMZ_Lon_yr, coordinate[, 2][2]-difMZ_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][3], coordinate[, 2][3], coordinate[, 1][3]+difNV_Lon_yr, coordinate[, 2][3]-difNV_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][4], coordinate[, 2][4], coordinate[, 1][4]+difON_Lon_yr, coordinate[, 2][4]+difON_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][5], coordinate[, 2][5], coordinate[, 1][5]-difFL_Lon_yr, coordinate[, 2][5]-difFL_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][6], coordinate[, 2][6], coordinate[, 1][6]-difHN_Lon_yr, coordinate[, 2][6]+difHN_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][7], coordinate[, 2][7], coordinate[, 1][7]-difSN_Lon_yr, coordinate[, 2][7]+difSN_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")
arrows(coordinate[, 1][8], coordinate[, 2][8], coordinate[, 1][8]-difMD_Lon_yr, coordinate[, 2][8]-difMD_Lat_yr,angle = 15,length=0.1,lwd=2,col="red")


####
# Not relate with the project
# The output figure is "terrain.png"
library(RgoogleMaps)
lat <- c(-90,90)      # define our map's ylim
lon <- c(-180,180)    # define our map's xlim
center = c(mean(lat), mean(lon))   # tell what point to center on
zoom <- 2             # zoom: 1 = furthest out (entire globe), larger numbers = closer in
terrmap <- GetMap(center=center, zoom=zoom, maptype= "terrain", destfile = "terrain.png")  #lots of visual options, just like google maps: maptype = c("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")



