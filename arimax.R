#ambil data
dataexpor<- read.csv(file.choose(),sep=";")
dataexpor
IHPBI<-read.csv(file.choose(),sep=";")
IHPBI
library (fpp2)
library (tseries)
library (urca)

#plot masing2 data

#plot ACF dan PACF
acf(dataexpor[,2])
pacf(dataexpor[,2])

#estimasi model
datay=dataexpor[,"Ekspor_tanjung_priok"]
dugaan1 = arima(datay, order = c(0,1,0));dugaan1
dugaan2 = arima(datay, order = c(1,1,0));dugaan2
dugaan3 = arima(datay, order = c(2,1,0));dugaan3
dugaan4 = arima(datay, order = c(3,1,0));dugaan4
dugaan5 = arima(datay, order = c(5,1,0));dugaan5

dugaan6 = arima(datay, order = c(0,1,1));dugaan6
dugaan7 = arima(datay, order = c(1,1,1));dugaan7
dugaan8 = arima(datay, order = c(2,1,1));dugaan8
dugaan9 = arima(datay, order = c(3,1,1));dugaan9
dugaan10 = arima(datay, order = c(5,1,1));dugaan10

#uji signifikansi model
library(lmtest)
coeftest(dugaan1)
coeftest(dugaan2)
coeftest(dugaan3)
coeftest(dugaan4)
coeftest(dugaan5)
coeftest(dugaan6)
coeftest(dugaan7)
coeftest(dugaan8)
coeftest(dugaan9)
coeftest(dugaan10)

#karena nilai p value kurang dari alfa maka koefisien nya signifikan


#diagnosa model
library(forecast)

res2=dugaan2$residuals
res3=dugaan3$residuals
res4=dugaan4$residuals
res5=dugaan5$residuals
res6=dugaan6$residuals
res7=dugaan7$residuals
res10=dugaan10$residuals

Box.test(res2)
Box.test(res3)
Box.test(res4)
Box.test(res5)
Box.test(res6)
Box.test(res7)
Box.test(res10)

#model yang layak adalah ketika p value besar dari alfa 
#dan ternyata model yang layak dan memenuhi asumsi residualnya adalah model 2 dan 4

#cek keakuratan
accuracy(dugaan2)
accuracy(dugaan3)
accuracy(dugaan4)
accuracy(dugaan5)
accuracy(dugaan6)
accuracy(dugaan7)
accuracy(dugaan10)

#ternyata model 10 telah mempunyai mape yang paling kecil

#estimasi model arimax dengan model (5,1,1)
#datay=dataexpor[,"Ekspor_tanjung_priok"] maka buat datax
datax=IHPBI[,"IHPBI"]
estimasiarimax= Arima(datay, order=c(5,1,1), xreg=datax)
estimasiarimax
coeftest(estimasiarimax)

#diagnosa model arimax
res_arimax=estimasiarimax$residuals
Box.test(res_arimax)
#karena p value besar dari alfa maka model layak digunakan untuk forecast

#kita dapat cek accuracy dari model arima dengan penambahan variabel exogen
accuracy (estimasiarimax)


#forecast 12 periode kedepan
xreg1 = rep(mean(datax),24)
forecast(estimasiarimax, xreg=xreg1, h=24)
