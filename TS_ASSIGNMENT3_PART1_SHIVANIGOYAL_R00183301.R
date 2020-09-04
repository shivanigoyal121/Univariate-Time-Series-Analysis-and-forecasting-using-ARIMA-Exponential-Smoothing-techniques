#-------------------------------TIME SERIES--------------------------------------------

#installing and loading all libraries
library(readr)
library(tidyr)
library(ggplot2)
library(pastecs)
library(dplyr)
library(VIM)
library(corrplot)
library(imputeTS)
library(lubridate)
library(forecast)
library(tseries)
install.packages("VIM")
library(readr)


#reading and saving the file as data frame
Assignment_Data <- read_csv("E:/shivani_timeseries/Assignment Data.csv", col_names = FALSE)
Assignment_Data<- as.data.frame(Assignment_Data)
head(Assignment_Data)

#Changing to numerical data
Assignment_Data <- Assignment_Data %>% mutate_if(is.character,as.numeric)
head(Assignment_Data)

#loading the text file with column names
columnNames <- read_csv("E:/shivani_timeseries/Assignment_columnnames.txt", col_names = FALSE)
columnNames

#Assigning column names to Assignment data
colnames(Assignment_Data) <- columnNames$X1
names(Assignment_Data)

#Visualizing the missing data
aggr(Assignment_Data[,-c(1)], combined = TRUE, numbers = TRUE)

#Sub-selecting the variables to be forecasted from the Assignment_Data 
ozoneData <- subset(Assignment_Data,select = c("WSR_PK","T_PK","T_AV","T85","RH85","HT85","T70","KI","TT","SLP","SLP_"))
str(ozoneData)

#statistical description of the data
stat.desc(ozoneData)

#EDA throgh Visualisation Technique
ggplot(gather(ozoneData), aes(value))+geom_histogram(bins = 10)+ facet_wrap(~key,scales='free_x') 

#Checking Outliers 
boxplot(ozoneData, main = "Boxplot of all variables")
boxplot(ozoneData[,-c(6,10)], main = "Boxplot of same scale variables")

#Visualising the missing data
aggr(ozoneData, numbers = TRUE, prop = c(TRUE, FALSE))

# Converting all the variables of ozoneData to time series data such that it starts from the starting month of the year 1998.
tsWSR_PK <- ts(ozoneData$WSR_PK,start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsT_PK <- ts(ozoneData$T_PK,start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsT_AV <- ts(ozoneData$T_AV,start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsT85 <- ts(ozoneData$T85,start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsRH85 <- ts(ozoneData$RH85, start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsHT85 <- ts(ozoneData$HT85, start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsT70 <- ts(ozoneData$T70, start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsKI <- ts(ozoneData$KI, start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsTT <- ts(ozoneData$TT, start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsSLP <- ts(ozoneData$SLP, start = decimal_date(as.Date("1998-01-01")), frequency = 365)
tsSLP_ <- ts(ozoneData$SLP_,start = decimal_date(as.Date("1998-01-01")), frequency = 365)

####1. For Variable 1:Forecasting on Peak Wind Speed(WSR_PK)

attributes(tsWSR_PK)

#To visualise the time series
plot(tsWSR_PK, main = "Peak Wind Speed observed for years (WSR_PK)", ylim = c(0,10), col = "red")

#Checking the statistics of missing data
statsNA(tsWSR_PK)
plotNA.distribution(tsWSR_PK)

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsWSR_PK.imp <- na_interpolation(tsWSR_PK, method = "linear")
plotNA.imputations(tsWSR_PK, tsWSR_PK.imp)

#Decomposition Of Time Series
decomp <- decompose(tsWSR_PK.imp)
plot(decomp, col = "red")

#visualising the seasonality
plot(decomp$seasonal, type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsWSR_PK.imp)
kpss.test(tsWSR_PK.imp, null="Trend")

acf(tsWSR_PK.imp,lag.max = length(tsWSR_PK.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsWSR_PK.sta <- diff(tsWSR_PK.imp)
attributes(tsWSR_PK.sta)

acf(tsWSR_PK.sta, lag.max = length(tsWSR_PK.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

plot(tsWSR_PK.sta, col = "red")

#train- test split
train <- window(tsWSR_PK.imp, end = c(2004, 3))
test <- window(tsWSR_PK.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train <- forecast(train)
summary(fit_auto_train)
##NOTE: AIC = 16371.83 , BIC = 16371.84, RMSE = 0.8912478

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta <- diff(train)
attributes(train.sta)

test.sta <- diff(test)
attributes(test.sta)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model <- auto.arima(train.sta, seasonal = FALSE)
summary(model)
##NOTE: AIC = 6159.11 , BIC = 6187.58, RMSE = 0.9834439
tsdisplay(residuals(model), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2 <- auto.arima(train.sta, seasonal= TRUE)
summary(model2)
##NOTE: AIC = 6159.11 , BIC = 6187.58, RMSE = 0.9834439
tsdisplay(residuals(model2), lag.max = 20, main = 'Seasonal Model Residuals')

#2.3 Manual Arima
set.seed(301)
model1 <- arima(train.sta, order = c(1,0,18))
summary(model1)
##NOTE: AIC = 6173.8 , RMSE = 0.9795425
tsdisplay(residuals(model1), lag.max = 20, main = 'Seasonal Model Residuals')

#Visualising the forecast by using each method
par(mfrow = c(2,2))
fit_auto_train %>% forecast(h=341) %>% autoplot() 
model1 %>% forecast(h=341) %>% autoplot() 
model2 %>% forecast(h=341) %>% autoplot() 
model %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train$residuals))

hist(model$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model$residuals))

hist(model1$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model1$residuals))

hist(model2$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2$residuals))

#NOTE: It is normally distrbuted

#ACF & PACF PLOTS
windows(10,10)
par(mfrow = c(4,2))
acf(fit_auto_train$residuals, main = 'Correlogram')
pacf(fit_auto_train$residuals, main = 'Partial Corelogram')

acf(model$residuals, main = 'Correlogram')
pacf(model$residuals, main = 'Partial Corelogram')

acf(model1$residuals, main = 'Correlogram')
pacf(model1$residuals, main = 'Partial Corelogram')

acf(model2$residuals, main = 'Correlogram')
pacf(model2$residuals, main = 'Partial Corelogram')


#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train$residuals, lag =20, type = 'Ljung-Box')
Box.test(model$residuals, lag =20, type = 'Ljung-Box')
Box.test(model1$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train), test) ["Test set", "RMSE"]
#o/p - 1.273901

accuracy(forecast(model1), test.sta) ["Test set", "RMSE"]
#O/P - 1.253145

accuracy(forecast(model2), test.sta) ["Test set", "RMSE"]
#o/p -  1.253728

accuracy(forecast(model), test.sta) ["Test set", "RMSE"]
#O/P - 1.253728

################################# VARIABLE 2#############################################
####2. For Variable 2:Forecasting on tsT_PK

attributes(tsT_PK)

plot(tsT_PK, main = "Peak tsT_PK", col = "red")
statsNA(tsT_PK)
plotNA.distribution(tsT_PK)

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsT_PK.imp <- na_interpolation(tsT_PK, method = "linear")
plotNA.imputations(tsT_PK, tsT_PK.imp)

#Decomposition Of Time Series
decomp1 <- decompose(tsT_PK.imp)
plot(decomp1, col = "red")
#NOTE: We observes the seasonality

#Visualising the seasonal component
par(mfrow=c(1,2))
plot(decomp1$seasonal, type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")
plot(decomp1$seasonal[1:400], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")


# Stationarity check of time series 
adf.test(tsT_PK.imp)
##It results stationary, as p-value is less than 0.05
kpss.test(tsT_PK.imp, null="Trend")
## It results non- stationary as p-value is less than 0.05. More p-value means tationary series

par(mfrow=c(1,1))
acf(tsT_PK.imp,lag.max = length(tsT_PK.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')
#Here, most of the lags are outside the significance bound, which means data is not stationary

#difference of 2 is sufficient
acf(diff(tsT_PK.imp, diff = 2))

tsT_PK.sta <- diff(tsT_PK.imp, diff = 2)
attributes(tsT_PK.sta)
acf(tsT_PK.sta, lag.max = length(tsT_PK.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

plot(tsT_PK.sta, col = "red")

#train-test split of the series
train1 <- window(tsT_PK.imp, end = c(2004, 3))
test1 <- window(tsT_PK.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_VAR2 <- forecast(train1)
summary(fit_auto_train_VAR2)
##NOTE: AIC = 21549.73 , BIC = 21566.81 , RMSE = 2.902054 

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta1 <- diff(train1, diff =2)
attributes(train.sta1)
test.sta1 <- diff(test1, diff =2)
attributes(test.sta1)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_VAR2 <- auto.arima(train.sta1, seasonal = FALSE)
summary(model_VAR2)
##NOTE: AIC = 11026.31 , BIC = 11060.46 , RMSE =2.983101
tsdisplay(residuals(model_VAR2), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_VAR2 <- auto.arima(train.sta1, seasonal= TRUE)
summary(model2_VAR2)
##NOTE: AIC = 11800.97  , BIC = 11835.12 , RMSE =3.564718
tsdisplay(residuals(model2_VAR2), lag.max = 20, main = 'Seasonal Model Residuals')

#2.3 Manual Arima
set.seed(301)
model1_VAR2 <- arima(train.sta1, order = c(1,0,5))
summary(model1_VAR2)
##NOTE: AIC = 11026.94, RMSE =2.980804
tsdisplay(residuals(model1_VAR2), lag.max = 20, main = 'Seasonal Model Residuals')

#Visualising forecast by each model
par(mfrow = c(2,2))
fit_auto_train_VAR2 %>% forecast(h=341) %>% autoplot() + autolayer(test1) 
model1_VAR2 %>% forecast(h=341) %>% autoplot() 
model2_VAR2 %>% forecast(h=341) %>% autoplot() 
model_VAR2 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_VAR2$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_VAR2$residuals))

hist(model_VAR2$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_VAR2$residuals))

hist(model1_VAR2$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model1_VAR2$residuals))

hist(model2$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_VAR2$residuals))

#NOTE: It is normally distrbuted

#ACF PLOTS
par(mfrow = c(2,2))
acf(fit_auto_train_VAR2$residuals, main = 'Correlogram')
acf(model_VAR2$residuals, main = 'Correlogram')
acf(model1_VAR2$residuals, main = 'Correlogram')
acf(model2_VAR2$residuals, main = 'Correlogram')

#PACF PLOTS
par(mfrow = c(2,2))
pacf(fit_auto_train_VAR2$residuals, main = 'Partial Corelogram')
pacf(model_VAR2$residuals, main = 'Partial Corelogram')
pacf(model1_VAR2$residuals, main = 'Partial Corelogram')
pacf(model2_VAR2$residuals, main = 'Partial Corelogram')

#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_VAR2$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_VAR2$residuals, lag =20, type = 'Ljung-Box')
Box.test(model1_VAR2$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_VAR2$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_VAR2), test1) ["Test set", "RMSE"]
#o/p - 6.771375

accuracy(forecast(model1_VAR2), test.sta1) ["Test set", "RMSE"]
#O/P - 4.442126

accuracy(forecast(model2_VAR2), test.sta1) ["Test set", "RMSE"]
#o/p - 4.453695

accuracy(forecast(model_VAR2), test.sta1) ["Test set", "RMSE"]
#O/P - 4.44153

################################# VARIABLE 3############################################
####3. For Variable 3:Forecasting on Average T (T_AV)

attributes(tsT_AV)

#To visualise the time series
plot(tsT_AV, main = "Average T observed for years (T_AV)", ylim = c(0,35), col = "red")

#Checking the missing data
plotNA.distribution(tsT_AV)

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsT_AV.imp <- na_interpolation(tsT_AV, method = "linear")
plotNA.imputations(tsT_AV, tsT_AV.imp)

#Decomposition Of Time Series
decomp2 <- decompose(tsT_AV.imp)
plot(decomp2, col = "red")

#Visualising the seasonal data
par(mfrow= c(1,2))
plot(decomp2$seasonal, type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")
plot(decomp2$seasonal[1:400], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsT_AV.imp)
#p-value is less than 0.05, hence it is stationary
kpss.test(tsT_AV.imp, null="Trend")
#p-value is less than 0.05, hence it is not trend- stationary

acf(tsT_AV.imp,lag.max = length(tsT_AV.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsT_AV.sta <- diff(tsT_AV.imp, diff =1)
attributes(tsT_AV.sta)

par(mfrow = c(1,1))
acf(tsT_AV.sta, lag.max = length(tsT_AV.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

plot(tsT_AV.sta, col = "red", main = "Stationary Series")

#TRAIN- TEST SET SPLIT
train2 <- window(tsT_AV.imp, end = c(2004, 3))
test2 <- window(tsT_AV.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var3 <- forecast(train2)
summary(fit_auto_train_Var3)
##NOTE: AIC = 20992.54, BIC = 21009.62 , RMSE = 2.55584

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var3 <- diff(train2, diff = 1)
attributes(train.sta_Var3)

test.sta_Var3 <- diff(test2, diff = 1)
attributes(test.sta_Var3)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var3 <- auto.arima(train.sta_Var3, seasonal = FALSE)
summary(model_Var3)
##NOTE: AIC = 10347.3, BIC = 10375.76 , RMSE = 2.557022
tsdisplay(residuals(model_Var3), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var3 <- auto.arima(train.sta_Var3, seasonal= TRUE)
summary(model2_Var3)
##NOTE: AIC = 10349.78, BIC =10389.63 , RMSE = 2.556135 
tsdisplay(residuals(model2_Var3), lag.max = 20, main = 'Seasonal Model Residuals')

#2.3 Manual Arima
set.seed(301)
model1_Var3 <- arima(train.sta_Var3, order = c(2,0,14))
summary(model1_Var3)
##NOTE: AIC = 10332.66, RMSE = 2.533326
tsdisplay(residuals(model1_Var3), lag.max = 20, main = 'Seasonal Model Residuals')

#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var3 %>% forecast(h=341) %>% autoplot() 
model1_Var3 %>% forecast(h=341) %>% autoplot() 
model2_Var3 %>% forecast(h=341) %>% autoplot() 
model_Var3 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_Var3$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var3$residuals))

hist(model_Var3$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var3$residuals))

hist(model1_Var3$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model1_Var3$residuals))

hist(model2_Var3$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var3$residuals))

#NOTE: It is normally distrbuted

#ACF PLOTS & PACF
windows(10,10)
par(mfrow = c(4,2))
acf(fit_auto_train_Var3$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var3$residuals, main = 'Partial Corelogram')

acf(model_Var3$residuals, main = 'Correlogram')
pacf(model_Var3$residuals, main = 'Partial Corelogram')

acf(model1_Var3$residuals, main = 'Correlogram')
pacf(model1_Var3$residuals, main = 'Partial Corelogram')

acf(model2_Var3$residuals, main = 'Correlogram')
pacf(model2_Var3$residuals, main = 'Partial Corelogram')


#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var3$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var3$residuals, lag =20, type = 'Ljung-Box')
Box.test(model1_Var3$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var3$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var3), test2) ["Test set", "RMSE"]
#o/p - 6.973589

accuracy(forecast(model1_Var3), test.sta_Var3) ["Test set", "RMSE"]
#O/P - 2.481816

accuracy(forecast(model2_Var3), test.sta_Var3) ["Test set", "RMSE"]
#o/p - 2.479714

accuracy(forecast(model_Var3), test.sta_Var3) ["Test set", "RMSE"]
#O/P - 2.479376

################################# VARIABLE 4 ###########################################
####4. For Variable 4 :Forecasting on T at 850 hpa level (or about 1500 m height) (T85)

attributes(tsT85)

#Visualising the time series plots
plot(tsT85, main = "T at 850 hpa level (or about 1500 m height)(tsT85)", col = "red")

#Checking the missing data
statsNA(tsT85)
plotNA.distribution(tsT85)
## Data is missing at random

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsT85.imp <- na_locf(tsT85)
plotNA.imputations(tsT85, tsT85.imp)

#Decomposition Of Time Series
decomp3 <- decompose(tsT85.imp)
plot(decomp3, col = "red")

#Plotting the decomposed series
par(mfrow=c(1,2))
plot(decomp3$seasonal, type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")
plot(decomp3$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsT85.imp)
#less pvalue, it is stationary 
kpss.test(tsT85.imp, null="Trend")
#less pvalue, it is not stationary

acf(tsT85.imp,lag.max = length(tsT85.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsT85.sta <- diff(tsT85.imp)
attributes(tsT85.sta)

acf(tsT85.sta, lag.max = length(tsT85.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#to visualise the transformed stationary data
par(mfrow = c(1,1))
plot(tsT85.sta, col = "red")


#TRAIN- TEST SET SPLIT
train3 <- window(tsT85.imp, end = c(2004, 3))
test3 <- window(tsT85.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var4 <- forecast(train3)
summary(fit_auto_train_Var4)
##NOTE: AIC = 20079.90 , BIC = 20096.98 , RMSE = 2.075702

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var4 <- diff(train3)
attributes(train.sta_Var4)

test.sta_Var4 <- diff(test3)
attributes(test.sta_Var4)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var4 <- auto.arima(train.sta_Var4, seasonal = FALSE)
summary(model_Var4)
##NOTE: AIC = 9667.99 , BIC = 9696.46 , RMSE = 2.19006
tsdisplay(residuals(model_Var4), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var4 <- auto.arima(train.sta_Var4, seasonal= TRUE)
summary(model2_Var4)
##NOTE: AIC =9667.55, BIC = 9690.32, RMSE = 2.190842
tsdisplay(residuals(model2_Var4), lag.max = 20, main = 'Seasonal Model Residuals')

#2.3 Manual Arima
set.seed(301)
model1_Var4 <- arima(train.sta_Var4, order = c(2,0,15))
summary(model1_Var4)
##NOTE: AIC = 9680.88 , RMSE = 2.182491
tsdisplay(residuals(model1_Var4), lag.max = 20, main = 'Seasonal Model Residuals')

#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var4 %>% forecast(h=341) %>% autoplot() 
model1_Var4 %>% forecast(h=341) %>% autoplot() 
model2_Var4 %>% forecast(h=341) %>% autoplot() 
model_Var4 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_Var4$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var4$residuals))

hist(model_Var4$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var4$residuals))

hist(model1_Var4$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model1_Var4$residuals))

hist(model2_Var4$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var4$residuals))

#NOTE: It is normally distrbuted

#ACF PLOTS & PACF PLOTS
windows(10,10)
par(mfrow = c(4,2))
acf(fit_auto_train_Var4$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var4$residuals, main = 'Partial Corelogram')

acf(model_Var4$residuals, main = 'Correlogram')
pacf(model_Var4$residuals, main = 'Partial Corelogram')

acf(model1_Var4$residuals, main = 'Correlogram')
pacf(model1_Var4$residuals, main = 'Partial Corelogram')

acf(model2_Var4$residuals, main = 'Correlogram')
pacf(model2_Var4$residuals, main = 'Partial Corelogram')


#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var4$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var4$residuals, lag =20, type = 'Ljung-Box')
Box.test(model1_Var4$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var4$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var4), test3) ["Test set", "RMSE"]
#o/p - 4.297961

accuracy(forecast(model1_Var4), test.sta_Var4) ["Test set", "RMSE"]
#O/P - 2.202662

accuracy(forecast(model2_Var4), test.sta_Var4) ["Test set", "RMSE"]
#o/p - 2.203002

accuracy(forecast(model_Var4), test.sta_Var4) ["Test set", "RMSE"]
#O/P - 2.202313

################################# VARIABLE 5############################################
####5. For Variable5 :Forecasting on Relative Humidity at 850 hpa (RS85)

attributes(tsRH85)

#TO VISUALISE THE TIME SERIES
par(mfrow = c(1,1))
plot(tsRH85, main = " Relative Humidity at 850 hpa (tsRH85)", col = "red")

#To check the missing data
statsNA(tsRS85)
plotNA.distribution(tsRH85)
#data is missing at random 

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsRH85.imp <- na_locf(tsRH85)
plotNA.imputations(tsRH85, tsRH85.imp)

#Decomposition Of Time Series
decomp4 <- decompose(tsRH85.imp)
plot(decomp4, col = "red")

#Plotting the decomposed series
par(mfrow = c(1,1))
plot(decomp4$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 
adf.test(tsRH85.imp)
#smaller p-value, hence the series is stationary
kpss.test(tsRH85.imp, null="Trend")
#p-value is greater, hence the series is stationary

acf(tsRH85.imp,lag.max = length(tsRH85.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#SERIES IS STATIONARY

#TRAIN- TEST SET SPLIT
train4 <- window(tsRH85.imp, end = c(2004, 3))
test4 <- window(tsRH85.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var5 <- forecast(train4)
summary(fit_auto_train_Var5)
##NOTE: AIC = 10212.74, BIC = 10229.82 , RMSE = 0.2188444

#Model 2 : ARIMA Model

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var5 <- auto.arima(train4, seasonal = FALSE)
summary(model_Var5)
##NOTE: AIC = -230.07 , BIC = -195.91 , RMSE = 0.2289515
tsdisplay(residuals(model_Var5), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var5 <- auto.arima(train4, seasonal= TRUE)
summary(model2_Var5)
##NOTE: AIC = -228.69 , BIC = -177.46 , RMSE = 0.2287086
tsdisplay(residuals(model2_Var5), lag.max = 20, main = 'Seasonal Model Residuals')


#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var5 %>% forecast(h=341) %>% autoplot() 
model2_Var5 %>% forecast(h=341) %>% autoplot() 
model_Var5 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_Var5$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var5$residuals))

hist(model_Var5$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var5$residuals))

hist(model2_Var5$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var5$residuals))

#NOTE: It is normally distrbuted

#ACF PLOTS & PACF PLOTS
windows(10,10)
par(mfrow = c(3,2))
acf(fit_auto_train_Var5$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var5$residuals, main = 'Partial Corelogram')

acf(model_Var5$residuals, main = 'Correlogram')
pacf(model_Var5$residuals, main = 'Partial Corelogram')

acf(model2_Var5$residuals, main = 'Correlogram')
pacf(model2_Var5$residuals, main = 'Partial Corelogram')


#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var5$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var5$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var5$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var5), test4) ["Test set", "RMSE"]
#o/p - 0.2604829

accuracy(forecast(model2_Var5), test4) ["Test set", "RMSE"]
#o/p - 0.248869

accuracy(forecast(model_Var5), test4) ["Test set", "RMSE"]
#O/P - 0.2489681

################################# VARIABLE 6############################################
####6. For Variable5 :Forecasting on Geopotential height at 850 hpa,
####   it is about the same as height at low altitude (HT85)

attributes(tsHT85)

#Visualising the time series
par(mfrow = c(1,1))
plot(tsHT85, main = "Geopotential height at 850 hpa (tsHT85)", col = "red")

#Visualising the missing values
plotNA.distribution(tsHT85)
#data is missing at random 

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsHT85.imp <- na_locf(tsHT85)
plotNA.imputations(tsHT85, tsHT85.imp)

#Decomposition Of Time Series
decomp5 <- decompose(tsHT85.imp)
plot(decomp5, col = "red")

#NOTE: Observes some trend and seasonality in it

#Plotting the decomposed series
plot(decomp5$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsHT85.imp)
#smaller p-value, it is stationary 
kpss.test(tsHT85.imp, null="Trend")
#smaller p-value, hence not stationary

acf(tsHT85.imp,lag.max = length(tsHT85.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsHT85.sta <- diff(tsHT85.imp, diff = 1)
attributes(tsHT85.sta)

acf(tsHT85.sta, lag.max = length(tsHT85.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#Plotting transformed stationary series
plot(tsHT85.sta, col = "red")

#TRAIN- TEST SET SPLIT
train5 <- window(tsHT85.imp, end = c(2004, 3))
test5 <- window(tsHT85.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var6 <- forecast(train5)
summary(fit_auto_train_Var6)
##NOTE: AIC = 30316.22 , BIC = 30333.30, RMSE = 21.41649

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var6 <- diff(train5, diff = 1)
attributes(train.sta_Var6)

test.sta_Var6 <- diff(test5, diff = 1)
attributes(test.sta_Var6)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var6 <- auto.arima(train.sta_Var6, seasonal = FALSE)
summary(model_Var6)
##NOTE: AIC = 19604.82 , BIC = 19638.98, RMSE = 21.11353
tsdisplay(residuals(model_Var6), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var6 <- auto.arima(train.sta_Var6, seasonal= TRUE)
summary(model2_Var6)
##NOTE: AIC = 19610.11 , BIC = 19667.03, RMSE = 21.10002 
tsdisplay(residuals(model2_Var6), lag.max = 20, main = 'Seasonal Model Residuals')


#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var6 %>% forecast(h=341) %>% autoplot() 
model2_Var6 %>% forecast(h=341) %>% autoplot() 
model_Var6 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(3,2))
hist(fit_auto_train_Var6$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var6$residuals))

hist(model_Var6$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var6$residuals))

hist(model2_Var6$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var6$residuals))

#NOTE: It is normally distrbuted

#ACF PLOTS
windows(10,10)
par(mfrow = c(3,2))
acf(fit_auto_train_Var6$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var6$residuals, main = 'Partial Corelogram')

acf(model1_Var6$residuals, main = 'Correlogram')
pacf(model1_Var6$residuals, main = 'Partial Corelogram')

acf(model2_Var6$residuals, main = 'Correlogram')
pacf(model2_Var6$residuals, main = 'Partial Corelogram')

#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var6$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var6$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var6$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var6), test5) ["Test set", "RMSE"]
#o/p - 103.0419

accuracy(forecast(model2_Var6), test.sta_Var6) ["Test set", "RMSE"]
#o/p -23.82281

accuracy(forecast(model_Var6), test.sta_Var6) ["Test set", "RMSE"]
#O/P - 23.86577

################################# VARIABLE 7############################################
####7. For Variable7 :Forecasting on T at 700 hpa level (roughly 3100 m height)(tsT70)

attributes(tsT70)
par(mfrow = c(1,1))
#visualising the time sries
plot(tsT70, main = "T at 700 hpa level (roughly 3100 m height)(tsT70)", col = "red")

#Visualising the series with missing data
plotNA.distribution(tsT70)

# Impute the missing values with "na_locf" function of "imputeTS package" and visualise the imputed values in the time series
tsT70.imp <- na_locf(tsT70)
plotNA.imputations(tsT70, tsT70.imp)

#Decomposition Of Time Series
decomp6 <- decompose(tsT70.imp)
plot(decomp6, col = "red")

#Plotting the decomposed series
plot(decomp6$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsT70.imp)
##p values is small, hence it is stationary
kpss.test(tsT70.imp, null="Trend")
##p - value is smaller than 0.05, hence it is not stationary

#Autocorelation plot 
acf(tsT70.imp,lag.max = length(tsT70.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsT70.sta <- diff(tsT70.imp)
attributes(tsT70.sta)

acf(tsT70.sta, lag.max = length(tsT70.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#Plotting the stationay series
plot(tsT70.sta, col = "red")

#TRAIN- TEST SET SPLIT
train6 <- window(tsT70.imp, end = c(2004, 3))
test6 <- window(tsT70.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var7 <- forecast(train6)
summary(fit_auto_train_Var7)
##NOTE: AIC = 19695.46  , BIC = 19712.54 , RMSE = 1.901511 

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var7 <- diff(train6)
attributes(train.sta_Var7)

test.sta_Var7 <- diff(test6)
attributes(test.sta_Var7)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var7 <- auto.arima(train.sta_Var7, seasonal = FALSE)
summary(model_Var7)
##AIC = 9309.05   , BIC = 9331.82, RMSE = 2.018759
tsdisplay(residuals(model_Var7), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var7 <- auto.arima(train.sta_Var7, seasonal= TRUE)
summary(model2_Var7)
##AIC = 9312.26  , BIC = 9346.42, RMSE = 2.018396
tsdisplay(residuals(model2_Var7), lag.max = 40, main = 'Seasonal Model Residuals')


#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var7 %>% forecast(h=341) %>% autoplot() 
model2_Var7 %>% forecast(h=341) %>% autoplot() 
model_Var7 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
windows(10,10)
par(mfrow= c(3,2))
hist(fit_auto_train_Var7$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var7$residuals))

hist(model_Var7$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var7$residuals))

hist(model2_Var7$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var7$residuals))

#NOTE: It is normally distrbuted

#ACF PLOTS & PACF PLOTS
par(mfrow = c(2,2))
acf(fit_auto_train_Var7$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var7$residuals, main = 'Partial Corelogram')

acf(model_Var7$residuals, main = 'Correlogram')
pacf(model_Var7$residuals, main = 'Partial Corelogram')

acf(model1_Var7$residuals, main = 'Correlogram')
pacf(model1_Var7$residuals, main = 'Partial Corelogram')

acf(model2_Var7$residuals, main = 'Correlogram')
pacf(model2_Var7$residuals, main = 'Partial Corelogram')

#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var7$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var7$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var7$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var7), test6) ["Test set", "RMSE"]
#o/p - 2.870951

accuracy(forecast(model2_Var7), test.sta_Var7) ["Test set", "RMSE"]
#o/p -2.00263

accuracy(forecast(model_Var7), test.sta_Var7) ["Test set", "RMSE"]
#O/P - 2.002481

################################# VARIABLE 8############################################
####8. For Variable8 :Forecasting on Average T observed for years (tsKI)

attributes(tsKI)
#plotting the time series
plot(tsKI, main = "Average T observed for years (tsKI)", col = "red")

#Visualising the missing data
plotNA.distribution(tsKI)

# Impute the missing values with "na_interpolation" function of "imputeTS package" and visualise the imputed values in the time series
tsKI.imp <- na_interpolation(tsKI, method = "linear")
plotNA.imputations(tsKI, tsKI.imp)

#Decomposition Of Time Series
decomp7 <- decompose(tsKI.imp)
plot(decomp7, col = "red")

#Plotting the decomposed series
plot(decomp7$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsKI.imp)
#p-value is smaller, hence series is stationary
kpss.test(tsKI.imp, null="Trend")
#series is not stationary
acf(tsKI.imp,lag.max = length(tsT70.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')
#most of the lags is outside the significance bound

#difference of 1 is sufficient
tsKI.sta <- diff(tsKI.imp)
attributes(tsKI.sta)

acf(tsKI.sta, lag.max = length(tsKI.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#plotting the transformed stationary series
plot(tsKI.sta, col = "red")

#TRAIN- TEST SET SPLIT
train7 <- window(tsKI.imp, end = c(2004, 3))
test7 <- window(tsKI.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var8 <- forecast(train7)
summary(fit_auto_train_Var8)
##NOTE: AIC = 28718.12  , BIC = 28735.20 , RMSE = 14.87674

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var8 <- diff(train7)
attributes(train.sta_Var8)

test.sta_Var8 <- diff(test7)
attributes(test.sta_Var8)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var8 <- auto.arima(train.sta_Var8, seasonal = FALSE)
##NOTE: AIC = 18210.98  , BIC = 18245.14 , RMSE = 15.36312 
summary(model_Var8)

tsdisplay(residuals(model_Var8), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var8 <- auto.arima(train.sta_Var8, seasonal= TRUE)
summary(model2_Var8)
##NOTE: AIC = 18210.98  , BIC = 18245.14 , RMSE = 15.36312
tsdisplay(residuals(model2_Var8), lag.max = 20, main = 'Seasonal Model Residuals')

#2.3 Manual Arima
set.seed(301)
model1_Var8 <- arima(train.sta_Var8, order = c(2,0,13))
summary(model1_Var8)
##NOTE: AIC = 18221.59  , RMSE = 15.32315
tsdisplay(residuals(model1_Var8), lag.max = 20, main = 'Seasonal Model Residuals')

#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var8 %>% forecast(h=341) %>% autoplot() 
model1_Var8 %>% forecast(h=341) %>% autoplot() 
model2_Var8 %>% forecast(h=341) %>% autoplot() 
model_Var8 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_Var8$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var8$residuals))

hist(model_Var8$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var8$residuals))

hist(model1_Var8$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model1_Var8$residuals))

hist(model2_Var8$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var8$residuals))

#NOTE: It is normally distrbuted

#ACF & ACF PLOTS
windows(10,10)
par(mfrow = c(4,2))
acf(fit_auto_train_Var8$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var8$residuals, main = 'Partial Corelogram')

acf(model_Var8$residuals, main = 'Correlogram')
pacf(model_Var8$residuals, main = 'Partial Corelogram')

acf(model1_Var8$residuals, main = 'Correlogram')
pacf(model1_Var8$residuals, main = 'Partial Corelogram')

acf(model2_Var8$residuals, main = 'Correlogram')
pacf(model2_Var8$residuals, main = 'Partial Corelogram')


#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var8$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var8$residuals, lag =20, type = 'Ljung-Box')
Box.test(model1_Var8$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var8$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var8), test7) ["Test set", "RMSE"]
#o/p - 22.98142

accuracy(forecast(model1_Var8), test.sta_Var8) ["Test set", "RMSE"]
#O/P - 16.92509

accuracy(forecast(model2_Var8), test.sta_Var8) ["Test set", "RMSE"]
#o/p - 16.9194

accuracy(forecast(model_Var8), test.sta_Var8) ["Test set", "RMSE"]
#O/P - 16.9194

################################# VARIABLE 9 ############################################
####9. For Variable9 :Forecasting on T-Totals(tsTT)

attributes(tsTT)

#Plotting the time series
par(mfrow = c(1,1))
plot(tsTT, main = " T-Totals(tsTT) ", col = "red")

#Plotting the missing data
plotNA.distribution(tsTT)

# Impute the missing values with "na_locf" function of "imputeTS package" and visualise the imputed values in the time series
tsTT.imp <- na_locf(tsTT)
plotNA.imputations(tsTT, tsTT.imp)

#Decomposition Of Time Series
decomp8 <- decompose(tsTT.imp)
plot(decomp8, col = "red")

#Plotting the decomposed series
plot(decomp8$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 

adf.test(tsTT.imp)
#p-value is smaller so it is stationary
kpss.test(tsTT.imp, null="Trend")
#p-value is greater than 0.05

acf(tsTT.imp,lag.max = length(tsTT.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsTT.sta <- diff(tsTT.imp)
attributes(tsTT.sta)

acf(tsTT.sta, lag.max = length(tsTT.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

plot(tsTT.sta, col = "red")

#TRAIN- TEST SET SPLIT
train8 <- window(tsTT.imp, end = c(2004, 3))
test8 <- window(tsTT.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var9 <- forecast(train8)
summary(fit_auto_train_Var9)
##NOTE: AIC = 26480.80  , BIC = 26497.88 , RMSE = 8.932469 

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var9 <- diff(train8)
attributes(train.sta_Var9)

test.sta_Var9 <- diff(test8)
attributes(test.sta_Var9)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var9 <- auto.arima(train.sta_Var9, seasonal = FALSE)
summary(model_Var9)
##NOTE: AIC = 15970.47  , BIC = 15993.24 , RMSE = 9.223455
tsdisplay(residuals(model_Var9), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var9 <- auto.arima(train.sta_Var9, seasonal= TRUE)
summary(model2_Var9)
##NOTE: AIC = 15975.95  , BIC = 16027.18 , RMSE = 9.213898
tsdisplay(residuals(model2_Var9), lag.max = 20, main = 'Seasonal Model Residuals')


#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var9 %>% forecast(h=341) %>% autoplot() 
model2_Var9 %>% forecast(h=341) %>% autoplot() 
model_Var9 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_Var9$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var9$residuals))

hist(model_Var9$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var9$residuals))

hist(model2_Var9$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var9$residuals))

#NOTE: It is normally distrbuted

#ACF & PACF PLOTS
windows(10,10)
par(mfrow = c(3,2))
acf(fit_auto_train_Var9$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var9$residuals, main = 'Partial Corelogram')

acf(model_Var9$residuals, main = 'Correlogram')
pacf(model_Var9$residuals, main = 'Partial Corelogram')

acf(model2_Var9$residuals, main = 'Correlogram')
pacf(model2_Var9$residuals, main = 'Partial Corelogram')


#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var9$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var9$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var9$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var9), test8) ["Test set", "RMSE"]
#o/p - 11.11376

accuracy(forecast(model2_Var9), test.sta_Var9) ["Test set", "RMSE"]
#o/p - 9.527655

accuracy(forecast(model_Var9), test.sta_Var9) ["Test set", "RMSE"]
#O/P - 9.523064

################################# VARIABLE 10 ############################################
####10. For Variable9 :Forecasting on Sea level pressure  (tsSLP)

#Checking the attributes which gives the idea of structure as well
attributes(tsSLP)

#Visualising the time series
plot(tsSLP, main = "Sea level pressure  (tsSLP)", col = "red")

#Visualising the distribution of missing data
plotNA.distribution(tsSLP)

# Impute the missing values with "na_locf" function of "imputeTS package" and visualise the imputed values in the time series
tsSLP.imp <- na_locf(tsSLP)
plotNA.imputations(tsSLP, tsSLP.imp)

#Decomposition Of Time Series
decomp9 <- decompose(tsSLP.imp)
plot(decomp9, col = "red")

#Plotting the decomposed series
plot(decomp9$seasonal, type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")
##Observes trend and seasonality

# Stationarity check of time series 

adf.test(tsSLP.imp)
#p-vaue is smaller, hence it is stationary 
kpss.test(tsSLP.imp, null="Trend")
#p-vaue is smaller, hence it is not trend stationary

acf(tsSLP.imp,lag.max = length(tsSLP.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#difference of 1 is sufficient
tsSLP.sta <- diff(tsSLP.imp)
attributes(tsSLP.sta)

acf(tsSLP.sta, lag.max = length(tsSLP.sta),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#Visualising the transformed stationary series
plot(tsSLP.sta, col = "red")

#TRAIN- TEST SET SPLIT
train9 <- window(tsSLP.imp, end = c(2004, 3))
test9 <- window(tsSLP.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var10 <- forecast(train9)
summary(fit_auto_train_Var10)
##NOTE: AIC = 32047.14  , BIC = 32064.22  , RMSE = 31.77912  

#Model 2 : ARIMA Model

#Changing to stationary set
train.sta_Var10 <- diff(train9)
test.sta_Var10 <- diff(test9)

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var10 <- auto.arima(train.sta_Var10, seasonal = FALSE)
summary(model_Var10)
##NOTE: AIC = 21325.67  , BIC = 21359.83  , RMSE = 31.26308 
tsdisplay(residuals(model_Var10), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var10 <- auto.arima(train.sta_Var10, seasonal= TRUE)
summary(model2_Var10)
##NOTE: AIC = 21325.67  , BIC = 21359.83 , RMSE = 31.26308
tsdisplay(residuals(model2_Var10), lag.max = 20, main = 'Seasonal Model Residuals')

#2.3 Manual Arima
set.seed(301)
model1_Var10 <- arima(train.sta_Var10, order = c(3,0,13))
summary(model1_Var10)
##NOTE: AIC = 21325.57   , RMSE = 31.0898
tsdisplay(residuals(model1_Var10), lag.max = 20, main = 'Seasonal Model Residuals')

#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var10 %>% forecast(h=341) %>% autoplot() 
model1_Var10 %>% forecast(h=341) %>% autoplot() 
model2_Var10 %>% forecast(h=341) %>% autoplot() 
model_Var10 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
par(mfrow= c(2,2))
hist(fit_auto_train_Var10$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var10$residuals))

hist(model_Var10$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var10$residuals))

hist(model1_Var10$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model1_Var10$residuals))

hist(model2_Var10$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var10$residuals))

#NOTE: It is normally distrbuted

#ACF & PACF PLOTS
windows(10,10)
par(mfrow = c(4,2))
acf(fit_auto_train_Var10$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var10$residuals, main = 'Partial Corelogram')

acf(model_Var10$residuals, main = 'Correlogram')
pacf(model_Var10$residuals, main = 'Partial Corelogram')

acf(model1_Var10$residuals, main = 'Correlogram')
pacf(model1_Var10$residuals, main = 'Partial Corelogram')

acf(model2_Var10$residuals, main = 'Correlogram')
pacf(model2_Var10$residuals, main = 'Partial Corelogram')

#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var10$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var10$residuals, lag =20, type = 'Ljung-Box')
Box.test(model1_Var10$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var10$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var10), test9) ["Test set", "RMSE"]
#o/p - 157.3138

accuracy(forecast(model1_Var10), test.sta_Var10) ["Test set", "RMSE"]
#O/P - 35.2608

accuracy(forecast(model2_Var10), test.sta_Var10) ["Test set", "RMSE"]
#o/p - 35.00271

accuracy(forecast(model_Var10), test.sta_Var10) ["Test set", "RMSE"]
#O/P - 35.00271

################################# VARIABLE 11 ############################################
####11. For Variable9 :Forecasting on SLP change from previous day(tsSLP_)

#Checking the attributes which gives the idea of structure as well
attributes(tsSLP_)

#Visualising the time series
plot(tsSLP_, main = "SLP change from previous day ", col = "red")

#BVisualising the distribution of missing data
plotNA.distribution(tsSLP_)

# Impute the missing values with "na_locf" function of "imputeTS package" and visualise the imputed values in the time series
tsSLP_.imp <- na_locf(tsSLP_)
plotNA.imputations(tsSLP_, tsSLP_.imp)

#Decomposition Of Time Series
decomp10 <- decompose(tsSLP_.imp)
plot(decomp10, col = "red")

#Plotting the decomposed series
plot(decomp10$seasonal[1:365], type ='b', xlab = 'Years', ylab = 'Seasonality Index', col = 'blue', las = 2, main = "SEASONAL DATA")

# Stationarity check of time series 
adf.test(tsSLP_.imp)
#p-value is smaller, hence stationary
kpss.test(tsSLP_.imp, null="Trend")
#p-value is greater, hence stationary 

acf(tsSLP_.imp,lag.max = length(tsSLP_.imp),xlab = "lag #", ylab = 'ACF',main='Statiionary Check ')

#TRAIN- TEST SET SPLIT
train10 <- window(tsSLP_.imp, end = c(2004, 3))
test10 <- window(tsSLP_.imp, start = c(2004, 4))

#Model 1 : Auto Exponential Smoothening Model
set.seed(301)
fit_auto_train_Var11 <- forecast(train10)
summary(fit_auto_train_Var11)
##NOTE: AIC = 31986.86 , BIC = 32003.94, RMSE = 31.34532

#Model 2 : ARIMA Model

#2.1 Auto Arima model without sesonality:
set.seed(301)
model_Var11 <- auto.arima(train10, seasonal = FALSE)
summary(model_Var11)
##NOTE: AIC = 21414.55, BIC = 21443.01, RMSE = 31.85287 
tsdisplay(residuals(model_Var11), lag.max = 45, main = '(1,1,1) Model Residuals')

#2.2 Auto Arima model with sesonality:
set.seed(301)
model2_Var11 <- auto.arima(train10, seasonal= TRUE)
summary(model2_Var11)
##NOTE: AIC = 21416.34, BIC = 21456.19, RMSE = 31.83673
tsdisplay(residuals(model2_Var11), lag.max = 20, main = 'Seasonal Model Residuals')


#Plotting forecast for each model
par(mfrow = c(2,2))
fit_auto_train_Var11 %>% forecast(h=341) %>% autoplot() 
model2_Var11 %>% forecast(h=341) %>% autoplot() 
model_Var11 %>% forecast(h=341) %>% autoplot() 

#Residual Plot - to confirm no problem with this model
windows(10,10)
par(mfrow= c(3,2))
hist(fit_auto_train_Var11$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(fit_auto_train_Var11$residuals))

hist(model_Var11$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model_Var11$residuals))

hist(model2_Var11$residuals,col = 'red',xlab = 'Error',main = 'Histogram Of Residuals',freq= FALSE)
lines(density(model2_Var11$residuals))

#NOTE: It is normally distrbuted

#ACF & PACF PLOTS
windows(10,10)
par(mfrow = c(3,2))
acf(fit_auto_train_Var11$residuals, main = 'Correlogram')
pacf(fit_auto_train_Var11$residuals, main = 'Partial Corelogram')

acf(model_Var11$residuals, main = 'Correlogram')
pacf(model_Var11$residuals, main = 'Partial Corelogram')

acf(model2_Var11$residuals, main = 'Correlogram')
pacf(model2_Var11$residuals, main = 'Partial Corelogram')

#Ljunx-Box Test to check the autocorelations of the residuals
Box.test(fit_auto_train_Var11$residuals, lag =20, type = 'Ljung-Box')
Box.test(model_Var11$residuals, lag =20, type = 'Ljung-Box')
Box.test(model2_Var11$residuals, lag =20, type = 'Ljung-Box')

#Validating The Model by testing the model performance with Holdout set

accuracy(forecast(fit_auto_train_Var11), test10) ["Test set", "RMSE"]
#o/p - 39.2348

accuracy(forecast(model_Var11), test10) ["Test set", "RMSE"]
#O/P - 34.85059

accuracy(forecast(model2_Var11), test10) ["Test set", "RMSE"]
#O/P - 34.81429
