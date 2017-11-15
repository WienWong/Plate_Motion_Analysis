

library(xlsx)
total <- read.xlsx("D:/Coursera_R/RawData/totalData.xlsx", "Sheet1")

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

ggplot(BJ, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(MZ, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(NV, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(ON, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(FL, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(HN, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(SN, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

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

ggplot(MD, aes(x=Time, y=Estimated, color = coordinate))  + 
    geom_point(aes(y = Estimated_Rad, col = "Radius"))    +
    geom_point(aes(y = Estimated_Lat, col = "Latitute"))  +
    geom_point(aes(y = Estimated_Lon, col = "Longitute")) 

library(rworldmap)
newmap <- getMap(resolution="low")
plot(newmap)

coordinate <- matrix(data=NA, nrow=8, ncol=2)
coordinate[, 1] <- c(115.53, 141.08, 83.14, 11.55, -101.58, -76.07, -119.31, -104.01) #longitude
coordinate[, 2] <- c(39.36, 39.08, 54.50, 57.23, 54.43, 38.35, 33.15, 30.40)  #latitude
coor <- data.frame(coordinate)  # need data frame not matrix format
names(coor) <- c('longitude','latitude') # add names 
class(coor$longitude)          # check if numeric format
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

