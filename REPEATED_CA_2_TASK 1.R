#Task 1 - To show the toatal number of rows, structure and first 10 rows of the data frame
getwd()
setwd("C:/Users/mdaba/Documents/R")

#We should read the NIpostcode data
NIpostcodesdata <- read.csv("C:/Users/mdaba/Documents/R/Repeated R/NIPostcodes.csv", header = FALSE)

#showing number of rows
nrow(NIpostcodesdata)

#Structure of the dataframe
str(NIpostcodesdata)

#Showing first 10 rows of the dataframe
head(NIpostcodesdata, n=10)

#Task -2 -We should add a efficient title for each attribute of the data
colnames(NIpostcodesdata) <- c("Organisation_name", "Subbuilding_name", "Building_name", "Number", "Primary_thorfare", "Alt_thorfare", "Secondary_thorfare", "Locality", "Townland", "Town", "County", "Postcode", "X-coord", "Y-coord", "Primary_key(identifier)")  

head(NIpostcodesdata, n=10)

#Structure of the dataframe
str(NIpostcodesdata)

#Task -3 We should remove or replace misssing entries with suitable identifier
#As dataset has many blank values its better to replace it with 'NA'.

NIpostcodesdata[NIpostcodesdata==""] <- NA
head(NIpostcodesdata,n=10)
str(NIpostcodesdata)

#Task-4 
#Displaying the sum of 'NA' using sapply, by creating dataset dff_sum
dff_sum <- data.frame(sapply(NIpostcodesdata, function(y) sum(length(which(is.na(y))))))
#for the result
dff_sum
#for displaying the mean of'NA's in each column, we created dff_mean by using sapply
dff_mean <- data.frame(sapply(NIpostcodesdata, function(y) mean(is.na(y))))
#for the result
dff_mean

#Task-5 modifying county attribute to a categorizing factor

#checking class type of dataset
class(NIpostcodesdata$County)

#changing class type county to a factor
levels(NIpostcodesdata$County) <- as.factor(NIpostcodesdata$County)
class(NIpostcodesdata$County)

#showing structure of dataset
str(NIpostcodesdata)
head(NIpostcodesdata)

#Task-6 Moving primary key identifier to start of a dataset
NIpostcodesdata <- NIpostcodesdata[,c(15, 1:14)]
str(NIpostcodesdata)
head(NIpostcodesdata)

#Task-7 Creating new dataset limavady and store only locality town
install.packages("dplyr")
library(dplyr)

Limavady_data <- data.frame(NIpostcodesdata$Locality, NIpostcodesdata$Townland, NIpostcodesdata$Town) %>% filter(NIpostcodesdata$Town == "LIMAVADY")
Limavady_data
#storing the information into limavady csv file
write.csv(Limavady_data, file = "C:/Users/mdaba/Documents/R/Limavady.csv", row.names = FALSE )
str(Limavady_data)


#Saving modified nipostcode dataset into a cleannipostcode dataset
write.csv(NIpostcodesdata, "C:/Users/mdaba/Documents/R/CleanNIPostcodesdata.csv")
str(NIpostcodesdata)
