#instal package
#install.packages("tidyverse")

#load library
library(tidyverse)
library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

#Load data
library(readxl)
dataset = read_excel('C:/Users/Sri Handini/Documents/kuliah/semester 4/TS/TUBES/bawang2.xlsx')
#dataset = read.csv('C:/Users/Sri Handini/Documents/kuliah/semester 4/TS/TUBES/bawang2.csv')

#melihat data
head(dataset)
view(dataset)
str(dataset)

# buat variable berisi kolom kolom yang dibutuhkan
dataset_pr = dataset %>% 
  mutate(Tanggal = ymd(Tanggal))

str(dataset_pr)
head(dataset_pr)

#Plot timeseries
p = ggplot(dataset_pr, aes(x = Tanggal, y = Harga)) + 
  geom_line() +
  labs(title = "Harga Bawang Merah di Pasar Induk Kramat Jati",
       y ="Harga",
       x = "Tanggal")
p

#acf Python
acf(dataset_pr$Harga)
#pacf Python
pacf(dataset_pr$Harga)

#Diferencing 1
dataset_1 = diff(dataset_pr$Harga, differences = 1)
plot.ts(dataset_1)
acf(dataset_1)
pacf(dataset_1)

#Diferencing 2
dataset_2 = diff(dataset_pr$Harga, differences = 2)
plot.ts(dataset_2)
acf(dataset_2)
pacf(dataset_2)

#Diferencing 3
dataset_3 = diff(dataset_pr$Harga, differences = 3)
plot.ts(dataset_3)
acf(dataset_3)
pacf(dataset_3)

#acf & pacf setelah diff 1
acf(dataset_3, lag.max = 20)
acf(dataset_3, lag.max = 20, plot = FALSE)
pacf(dataset_3, lag.max = 20)
pacf(dataset_3, lag.max = 20, plot = FALSE)

#automation
library(forecast)
auto.arima(dataset_pr$Harga, trace = TRUE)
ima = arima(dataset_pr$Harga, c(4,1,2))
ima

library('forecast')
#auto.arima(dataset_pr$Harga, trace = TRUE)
Fit1=arima(dataset_pr$Harga, order=c(0,1,0))
Fit2=arima(dataset_pr$Harga, order=c(1,1,0))
Fit3=arima(dataset_pr$Harga, order=c(2,1,0))
Fit4=arima(dataset_pr$Harga, order=c(3,1,0))
Fit5=arima(dataset_pr$Harga, order=c(4,1,0))
Fit6=arima(dataset_pr$Harga, order=c(0,1,2))
Fit7=arima(dataset_pr$Harga, order=c(1,1,2))
Fit8=arima(dataset_pr$Harga, order=c(2,1,2))
Fit9=arima(dataset_pr$Harga, order=c(3,1,2))
Fit10=arima(dataset_pr$Harga, order=c(4,1,2))
Fit1$aic
Fit2$aic
Fit3$aic
Fit4$aic
Fit5$aic
Fit6$aic
Fit7$aic
Fit8$aic
Fit9$aic
Fit10$aic
#prediksi
predict(Fit10, n.ahead = 14)

#forecasting
library(forecast)
dataset_forecast = forecast(Fit10, h = 14)
dataset_forecast

#residual
plot.ts(dataset_forecast$residuals)
hist(dataset_forecast$residuals)

#diag
tsdiag(Fit10)
Box.test(dataset_forecast$residuals)
accuracy(Fit10)

#MSE
install.packages("Metrics")
library(Metrics)
mse(dataset, dataset_forecast$residuals)

sisaan <- dataset_forecast$residuals
# Eksplorasi
par(mfrow=c(2,2))
qqnorm(sisaan)
qqline(sisaan, col = "blue", lwd = 2)
plot(c(1:length(sisaan)),sisaan)
acf(sisaan)
pacf(sisaan)