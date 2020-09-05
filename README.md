# Univariate-Time-Series-Analysis-and-forecasting-using-ARIMA-Exponential-Smoothing-techniques

The main aim of this project is to carry exploratory analysis on univariate time series data and further build models to forecast using ARIMA and exponential smoothing such modelling techniques. The dataset used is available in this repository with name "Assignment Data.csv". Also, the dataset used is open source Ozone Level Detection Data Set 
available on: http://archive.ics.uci.edu/ml/datasets/Ozone+Level+Detection.

Here, we are focusing on below mentioned key variables only for analyzing and predicting future states.

1. WSR_PK: continuous. peek wind speed -- resultant (meaning average of wind vector) 
2. T_PK: continuous. Peak T 
3. T_AV: continuous. Average T 
4. T85: continuous. T at 850 hpa level (or about 1500 m height) 
5. RH85: continuous. Relative Humidity at 850 hpa 
6. HT85: continuous. Geopotential height at 850 hpa, it is about the same as height at low altitude 
7. T70: continuous. T at 700 hpa level (roughly 3100 m height)
8. KI: continuous. K-Index 
9. TT: continuous. T-Totals 
10. SLP: continuous. Sea level pressure 
11. SLP_: continuous. SLP change from previous day  

For each series visualised and decomposed the time series and checked for the stationarity. Test the autocorrelation and partial autocorrelation of the residuals (using the plots and Ljung-Box Q statistic*) upto an appropriate lag. Also, checked for normality of the residuals and states the best model by comparing the performance of models build using appropriate exponential smoothing models with a collection of ARIMA models.
