#setting working directory
getwd()
setwd("C:/Users/mdaba/Documents/R/Repeated R/NI Crime Data/")
getwd()
#Task 1 Amalgamating all the crime data from every csv file into single datset
csv_files <- list.files(full.names = TRUE,recursive = TRUE)
csv_files

AllNicrimeData <- Reduce(rbind, lapply(csv_files, read.csv))
#showing number of rows in newly saved AllnicrimeData
nrow(AllNicrimeData)
#writing the newly saved data into csv file

write.csv(AllNicrimeData,file = "C:/Users/mdaba/Documents/R/Repeated R/AllNicrimeData.csv", row.names = TRUE)
str(AllNicrimeData)

head(AllNicrimeData)


# Task-2 Modifying structure of data and removing attributes
AllNicrimeData <- AllNicrimeData[ -c(1,3,4,8,9,11,12) ]

str(AllNicrimeData)
head(AllNicrimeData)

#Task_3 Factorizing crime type attribute
#checking the class type of the dataset

class(AllNicrimeData$Crime.type)

#changing the class type of the attribute criem type to factor
levels(AllNicrimeData$Crime.type) <- as.factor(AllNicrimeData$Crime.type)
str(AllNicrimeData)
head(AllNicrimeData)

#Task-4 Modifying location attribute for removing string "On or near" by using gsub
AllNicrimeData$Location <- lapply(AllNicrimeData$Location,gsub, pattern ="On or near", replacement="",fixed= TRUE)

head(AllNicrimeData)

#thereby modified blank spaces by NA identifiers
AllNicrimeData[AllNicrimeData == " "] <- NA
head(AllNicrimeData)

str(AllNicrimeData)

#Task 4
#removing na identifier for location attribute before chossing a random samples
new_NicrimeData <- subset(AllNicrimeData, !is.na(AllNicrimeData$Location))
new_NicrimeData

head(new_NicrimeData)

#Choosing 1000 random sample new_nicrimedata set
random_sample <- data.frame(new_NicrimeData[sample(nrow(new_NicrimeData), 1000), ])
View(random_sample)

library(dplyr)
#coverting uppercase for attributes location in random sample
random_sample$AllNicrimeData.Location <- toupper(random_sample$Location)

#Reading postcodes csv file
CleanNIPostcodes <- read.csv("C:/Users/mdaba/Documents/R/CleanNIPostcodesdata.csv", stringsAsFactors = FALSE)

#creating new dataset containing postcode and primary thorfare information from nipostcodes dataset
neww_pcode <- CleanNIPostcodes[, c(6, 13)]
head(neww_pcode, 5)


#deleting duplicate columns in primary thorfare column
neww_pcode <- neww_pcode[!duplicated(neww_pcode$'Primary_thorfare'),]

#setting column names for new dataset
colnames(neww_pcode) <- c("Primary_thorfare", "Postcode")
str(neww_pcode)

#adding new column to random sample dataset amd placing values as NA
random_sample$Postcode <-NA
head(random_sample, 5)

#Task 6 Appending postcodes to random sample dataset

#adding postcodes column  data values with matching the location with primary thorfare in neww_pcode
random_sample$Postcode <- neww_pcode$Postcode[match(random_sample$Location,neww_pcode$'Primary_thorfare')]

#Now the structure of random sample data set
str(random_sample)

#Task 7
#random sample data is saved into updated random sample
updtd_random_sample <- data.frame(random_sample)

#columns are shared in updated random sample
colnames(updtd_random_sample) <- c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Postcode")
head(updtd_random_sample, 3)

#updated random sample are saved into chart data set
chartData <- updtd_random_sample

#sorting chart data based on crime type and postcode
chartData <- chartData[order(chartData$Postcode == "BT1", chartData$Crime.type),]
chartData

#Creating new chart dataset containing postcode=BT1
new_chart <- filter(chartData, grepl('BT1', Postcode))
new_chart