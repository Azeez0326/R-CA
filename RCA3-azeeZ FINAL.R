
df <- read.csv("C:/Users/mdaba/Documents/R CA/Health.csv")
str(df)

df1 <- subset(df, select = c(TotalDischargesMale, TotalDischargesFemale, Year))
head(df1)
View(df1)
str(df1)


# Changing the type from factor to numeric

df1$TotalDischargesMale <- as.numeric(gsub(",","",df1$TotalDischargesMale))
df1$TotalDischargesFemale <- as.numeric(gsub(",","",df1$TotalDischargesFemale))
df1$Year <- as.numeric(df1$Year)
str(df1)


# Omitting all the null values

df1[df1 == "0"] <- NA
df1 <- na.omit(df1)
View(df1)
str(df1)


# creating subsets for extracting only Male Discharges
library(dplyr)
Male <- subset(df1, select=c(TotalDischargesMale,Year))
Male

# creating subsets for extracting only Female Discharges
library(dplyr)
Female <- subset(df1, select=c(TotalDischargesFemale,Year))
Female

# histogram graph for finding the distribution of data for MaleDischarges
library("lattice")
histogram(~TotalDischargesMale  |  Year, data = Male)

# histogram graph for finding the distribution of data for Female Discharges
library("lattice")
histogram(~TotalDischargesFemale  |  Year, data = Female)

# Checking normality value for Male Discharges
library(ggpubr)
ggqqplot(Male$TotalDischargesMale, ylab="Total discharges Male by years", ggtheme = theme_minimal())

# Checking normality value for FeMale Discharges
library(ggpubr)
ggqqplot(Female$TotalDischargesFemale, ylab="Total discharges Female by years", ggtheme = theme_minimal())


# Shapiro Test - normality test for Male discharges
normality_test <- shapiro.test(Male$TotalDischargesMale[0:236])
normality_test$p.value

# Shapiro Test - normality test for Female discharges
normality_test <- shapiro.test(Female$TotalDischargesFemale[0:236])
normality_test$p.value

# Selecting only subsets of Male
library(dplyr)
MaleDischarge <- subset(Male, select=c(TotalDischargesMale)) 
MaleDischarge

# Selecting only subsets of Female
FemaleDischarge <- subset(Female, select=c(TotalDischargesFemale))
FemaleDischarge

class(MaleDischarge)
MaleDischarge <-na.omit(MaleDischarge)
FemaleDischarge <-na.omit(FemaleDischarge)

#to convert the class type to numeric for wilcoxon test
MaleDischarge <- as.numeric(MaleDischarge$TotalDischargesMale)
FemaleDischarge <- as.numeric(FemaleDischarge$TotalDischargesFemale)

# Performing wilcoxon test- non - parameteric and not normally distributed data
wilcox.test <-wilcox.test(MaleDischarge,FemaleDischarge)
wilcox.test

#from the above test the P values is more that 0.05 so we accept the null hypothesis
#p-value is 0.75 from wilcoxon test


# Power Analysis test for sample size dtermination

# finding mean value for Male Discharges

mean(MaleDischarge,each=234)

# finding mean value for Female Discharges
mean(FemaleDischarge, each=234)

MaleDischargecount <-(MaleDischarge [0:234])
FemaleDischargecount <- (FemaleDischarge [0:234])


View(MaleDischarge)
View(FemaleDischarge)

dfnew <- rbind(MaleDischargecount,FemaleDischargecount)
dfnew

# to find SD for Delta value

sd(dfnew)

# SD = 28097 AND D value is 0.06 
# delta value(D value) was found by substracting the mean value and 
# dividing it by the SD

# Power Test for determining the sample size

library(pwr)
power_info <- power.t.test(delta = 0.06, n=NULL, sig.level = .05, power =.90,
                           type="two.sample", alternative = "two.sided")

power_info

plot(power_info)

# from the above test the sample size n was found to be 5838.4
# from the above result we will calculate
Power.V = power.t.test(n= 5838.4, delta = 0.06, sd =1,type = c("two.sample"))
Power.V

View(MaleDischarge)

# Cohens test 

cohen.ES(test = c("t"), size = c("small"))

res1 <- MaleDischarge[0:85]

res2 <- FemaleDischarge[0:85]

# Correlation test 

correlation_test <- cor.test(res1,res2,method = "spearman")

correlation_test

# from correlation test the rho value was found as 0.68
# there is very strong correlation from the above test was found


