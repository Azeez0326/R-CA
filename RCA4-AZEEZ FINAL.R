
# To read the Healthcare dataset
df <- read.csv("C:/Users/mdaba/Documents/R CA/Health.csv")

# To rename and extract the columns in a separate dataframe
df1 <- subset(df, select = c(TotalDischargesMale, TotalDischargesFemale, Year))
View(df1)

# To eliminate strings, blank spaces and other data cleaning 
df1$TotalDischargesMale <- as.numeric(gsub(",","",df1$TotalDischargesMale))
df1$TotalDischargesFemale <- as.numeric(gsub(",","",df1$TotalDischargesFemale))
df1$Year <- as.numeric(df1$Year)

View(df1)

# Omitting all the null values

df1[df1 == "0"] <- NA
df1 <- na.omit(df1)
View(df1)

# creating subsets for extracting only Male Discharges
library(dplyr)
MaleDischarges <- subset(df1, select=c(TotalDischargesMale,Year))
str(MaleDischarges)

# creating subsets for extracting only Female Discharges
library(dplyr)
FemaleDischarges <- subset(df1, select=c(TotalDischargesFemale,Year))
str(FemaleDischarges)

View(MaleDischarges)
View(FemaleDischarges)

# importing libraries for perfroming time series and forecasting
library('ggplot2')
library('forecast')
library('tseries')


# showing the Total Male Discharges over the years  using ggplot
ts_MaleDischarges <- ts(MaleDischarges$TotalDischargesMale, start=c(2015, 1), end=c(2017), frequency=12)
ts_MaleDischarges
ggseasonplot(ts_MaleDischarges)

# showing the Total Male Discharges over the years  using ggplot
ts_FemaleDischarges <- ts(FemaleDischarges$TotalDischargesFemale, start=c(2015, 1), end=c(2017), frequency=12)
ts_FemaleDischarges
ggseasonplot(ts_FemaleDischarges)

# to plot the stationary series for Male Discharges
library('tseries')
ts_MaleDischarges <- ts(MaleDischarges$TotalDischargesMale, start=c(2015, 1), end=c(2017), frequency=12)
plot(ts_MaleDischarges)

# to plot the stationary series for Female Discharges
library('tseries')
ts_FemaleDischarges <- ts(FemaleDischarges$TotalDischargesFemale, start=c(2015, 1), end=c(2017), frequency=12)
plot(ts_FemaleDischarges)


# ACF series for MaleDischarges
library(forecast)
library(tseries)
acf_Mldis <- Acf(ts_MaleDischarges) # autocorrelation
acf_Mldis

# ACF series for FemaleDischarges
library(forecast)
library(tseries)
acf_Fldis <- Acf(ts_FemaleDischarges) # autocorrelation
acf_Fldis

# Arima model for MaleDischarges
arima_model_M <- Arima(ts_MaleDischarges,  order = c(0,2,4))
arima_model_M

forecast(arima_model_M)

# Arima model for FemaleDischarges
arima_model_F <- Arima(ts_FemaleDischarges,  order = c(0,2,4))
arima_model_F

forecast(arima_model_F)

# qq plot FOR fiiting line for MaleDischarges
# the primary points falls on the line 
qqnorm(arima_model_M$residuals)
qqline(arima_model_M$residuals)

# qq plot FOR fiiting line for FemaleDischarges
# the primary points falls on the line 
qqnorm(arima_model_F$residuals)
qqline(arima_model_F$residuals)


# Box plot for finding the value fits or not for the model of MaleDischarges

Box.test(arima_model_M$residuals,type = "Ljung-Box")
forecast(arima_model_M,15)

plot(forecast(arima_model_M, 15), xlab = "Year", ylab = "Total Discharges - Male")

# Box plot for finding the value fits or not for the model of FemaleDischarges

Box.test(arima_model_F$residuals,type = "Ljung-Box")
forecast(arima_model_F,15)

plot(forecast(arima_model_F, 15), xlab = "Year", ylab = "Total Discharges - Female")
