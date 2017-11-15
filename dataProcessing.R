
# codes for data merge procedure

getwd()
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


# Data processing of FLIN
flLat = read.table('FLIN-Lat.txt');
head(flLat)
names(flLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(flLat)

flLon = read.table('FLIN-Lon.txt');
head(flLon)
names(flLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(flLon)

flRad = read.table('FLIN-Rad.txt');
head(flRad)
names(flRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(flRad)

# Combine columns together
FL <- cbind(flLat, flLon, flRad)
head(FL)

# Remove duplicate columns in FL
FL <- FL[, !duplicated(colnames(FL))]
head(FL)

# Remove unwanted column in FL 
FL$Coordinate <- NULL
head(FL)

# Reorder columns in FL
FL <- FL[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(FL)

# Merge BJFS and FLIN
BJ_FL <- rbind(BJ, FL)

###

# Data processing of HNPT
hnLat = read.table('HNPT-Lat.txt');
head(hnLat)
names(hnLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(hnLat)

hnLon = read.table('HNPT-Lon.txt');
head(hnLon)
names(hnLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(hnLon)

hnRad = read.table('HNPT-Rad.txt');
head(hnRad)
names(hnRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(hnRad)

# Combine columns together
HN <- cbind(hnLat, hnLon, hnRad)
head(HN)

# Remove duplicate columns in HN
HN <- HN[, !duplicated(colnames(HN))]
head(HN)

# Remove unwanted column in HN
HN$Coordinate <- NULL
head(HN)

# Reorder columns in HN
HN <- HN[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(HN)

# Merge BJFS, FLIN and HNPT
BJFL_HN <- rbind(BJ_FL, HN)


###

# Data processing of MDO1
mdLat = read.table('MDO1-Lat.txt');
head(mdLat)
names(mdLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(mdLat)

mdLon = read.table('MDO1-Lon.txt');
head(mdLon)
names(mdLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(mdLon)

mdRad = read.table('MDO1-Rad.txt');
head(mdRad)
names(mdRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(mdRad)

# Combine columns together
MD <- cbind(mdLat, mdLon, mdRad)
head(MD)

# Remove duplicate columns in MD
MD <- MD[, !duplicated(colnames(MD))]
head(MD)

# Remove unwanted column in MD
MD$Coordinate <- NULL
head(MD)

# Reorder columns in MD
MD <- MD[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(MD)

# Merge BJFS, FLIN, HNPT and MDO1
BJFLHN_MD <- rbind(BJFL_HN, MD)

#for (k in 1:292) { 
#    if (BJFLHN_MD$Site[k] == "BJFS"){
#        plot(BJFLHN_MD$Time[k], BJFLHN_MD$Estimated_Rad[k], xlab='Time', ylab='Estimated (cm)', ylim = c(-17,18))
#    }
#    k = k + 1 
#}


###

# Data processing of MIZU
miLat = read.table('MIZU-Lat.txt');
head(miLat)
names(miLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(miLat)

miLon = read.table('MIZU-Lon.txt');
head(miLon)
names(miLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(miLon)

miRad = read.table('MIZU-Rad.txt');
head(miRad)
names(miRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(miRad)

# Combine columns together
MI <- cbind(miLat, miLon, miRad)
head(MI)

# Remove duplicate columns in MI
MI <- MI[, !duplicated(colnames(MI))]
head(MI)

# Remove unwanted column in MI
MI$Coordinate <- NULL
head(MI)

# Reorder columns in MI
MI <- MI[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(MI)

# Merge BJFS, FLIN, HNPT, MDO1 and MIZU
BJFLHNMD_MI <- rbind(BJFLHN_MD, MI)


###

# Data processing of NVSK
nvLat = read.table('NVSK-Lat.txt');
head(nvLat)
names(nvLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(nvLat)

nvLon = read.table('NVSK-Lon.txt');
head(nvLon)
names(nvLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(nvLon)

nvRad = read.table('NVSK-Rad.txt');
head(nvRad)
names(nvRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(nvRad)

# Combine columns together
NV <- cbind(nvLat, nvLon, nvRad)
head(NV)

# Remove duplicate columns in NV
NV <- NV[, !duplicated(colnames(NV))]
head(NV)

# Remove unwanted column in NV
NV$Coordinate <- NULL
head(NV)

# Reorder columns in NV
NV <- NV[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(NV)

# Merge BJFS, FLIN, HNPT, MDO1, MIZU and NVSK
BJFLHNMDMI_NV <- rbind(BJFLHNMD_MI, NV)


###

# Data processing of ONSA
onLat = read.table('ONSA-Lat.txt');
head(onLat)
names(onLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(onLat)

onLon = read.table('ONSA-Lon.txt');
head(onLon)
names(onLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(onLon)

onRad = read.table('ONSA-Rad.txt');
head(onRad)
names(onRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(onRad)

# Combine columns together
ON <- cbind(onLat, onLon, onRad)
head(ON)

# Remove duplicate columns in ON
ON <- ON[, !duplicated(colnames(ON))]
head(ON)

# Remove unwanted column in ON
ON$Coordinate <- NULL
head(ON)

# Reorder columns in ON
ON <- ON[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(ON)

# Merge BJFS, FLIN, HNPT, MDO1, MIZU, NVSK and ONSA
BJFLHNMDMINV_ON <- rbind(BJFLHNMDMI_NV, ON)


###

# Data processing of SNI1
snLat = read.table('SNI1-Lat.txt');
head(snLat)
names(snLat) <- c('Time', 'Estimated_Lat', 'Uncertainty_Lat', 'Site', 'Coordinate', 'Date')
head(snLat)

snLon = read.table('SNI1-Lon.txt');
head(snLon)
names(snLon) <- c('Time', 'Estimated_Lon', 'Uncertainty_Lon', 'Site', 'Coordinate', 'Date')
head(snLon)

snRad = read.table('SNI1-Rad.txt');
head(snRad)
names(snRad) <- c('Time', 'Estimated_Rad', 'Uncertainty_Rad', 'Site', 'Coordinate', 'Date')
head(snRad)

# Combine columns together
SN <- cbind(snLat, snLon, snRad)
head(SN)

# Remove duplicate columns in SN
SN <- SN[, !duplicated(colnames(SN))]
head(SN)

# Remove unwanted column in SN
SN$Coordinate <- NULL
head(SN)

# Reorder columns in SN
SN <- SN[, c(1, 2, 3, 6, 7, 8, 9, 5, 4)]
head(SN)

# Merge BJFS, FLIN, HNPT, MDO1, MIZU, NVSK, ONSA and SNI1
BJFLHNMDMINVON_SN <- rbind(BJFLHNMDMINV_ON, SN)

total <- BJFLHNMDMINVON_SN

write.table(total, "D:/Coursera_R/RawData/totalData.txt", sep="\t")

write.csv(total, "D:/Coursera_R/RawData/totalData.csv")

library(xlsx)
write.xlsx(total, "D:/Coursera_R/RawData/totalData.xlsx")

# Subset data according to the Beijing station
BJ <- total[which(total$Site == "BJFS"), ]
