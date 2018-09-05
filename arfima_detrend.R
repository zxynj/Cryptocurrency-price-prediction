price <- read.table("D:/Spring2018/STAT6347/Data Set/datasetfortimeanalysis.txt",header = TRUE)
validation_price<-data.matrix(price)[2581:2640]
training_price<-data.matrix(price)[1:2580]
price_timeserie<-ts(training_price, frequency=1)

#percentage return change
diff_log_price_timeserie<-diff(log(price_timeserie))

#detrend
library("forecast")
fit_trend <- tslm(diff_log_price_timeserie ~ trend)
diff_log_price_trend<-coef(fit_trend)["(Intercept)"]+seq(1, 2639)*coef(fit_trend)["trend"] 

diff_log_price<-data.matrix(diff_log_price_timeserie)
detrend_diff_log_price = diff_log_price - diff_log_price_trend[1:2579]

detrend_diff_log_price_timeserie<-ts(detrend_diff_log_price, frequency=1)
plot(as.ts(detrend_diff_log_price_timeserie))

#study ts
acf(detrend_diff_log_price_timeserie, lag.max=100, main="acf")
pacf(detrend_diff_log_price_timeserie, lag.max=100, main="pacf")

#test stationary
library("tseries")
adf.test(detrend_diff_log_price_timeserie, alternative="stationary")

#find d from aggvarFit, diffvarFit and auto-selected
library("fArma")
aggvarFit(detrend_diff_log_price_timeserie, doplot=TRUE)
#d=0.5642436-0.5=0.0642436
diffvarFit(detrend_diff_log_price_timeserie, doplot=TRUE)
#d=0.673712-0.5=0.173712
arfima<-arfima(detrend_diff_log_price_timeserie)
summary(arfima)
#d=0.137

#define rmse function
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#run arma p:1-14  q:1-14
library("arfima")
library("openxlsx")
for (i in 10:14){
  aic_matrix<- matrix(0,14,1)
  rmse_matrix<- matrix(0,14,1)
  print(paste(i))
  for (j in 1:14){
    print(paste(j))
    arfima<-arfima(detrend_diff_log_price_timeserie, order = c(i, 0, j))
    print(AIC(arfima))
    options(warn=1)
    aic_matrix[j]<-AIC(arfima)
    
    pred_arfima<-predict(arfima,60)
    pred_arfima_matrix<-data.matrix(pred_arfima[[2]][["Forecast"]])
    pred_arfima_adding_trend<-pred_arfima_matrix[,1]+diff_log_price_trend[2580:2639]
  
    pred_price = matrix(0,61,1)
    pred_price[,1][1]<-295.1347521
    for (t in 2:61){
      pred_price[,1][t]<-exp(log(pred_price[,1][t-1])+pred_arfima_adding_trend[t-1])
    }
    error <- validation_price - pred_price[2:61]
    rmse_value<-rmse(error)
    rmse_matrix[j]<-rmse_value
    print(rmse_value)
  }
  nam <- paste("p",i, sep="_")
  write.xlsx(aic_matrix, "C:/Users/Xiao/Documents/arfima/aic.xlsx", sheetName=nam, append=TRUE)
  write.xlsx(rmse_matrix, "C:/Users/Xiao/Documents/arfima/rmse.xlsx", sheetName=nam, append=TRUE)
}

#check

arfima<-arfima(detrend_diff_log_price_timeserie, order=c(11,0,4),fixed = list(frac=0.173712))


#arfima<-arfima(detrend_diff_log_price_timeserie, order = c(6, 0, 6))
print(AIC(arfima))

pred_arfima<-predict(arfima,60)
pred_arfima_matrix<-data.matrix(pred_arfima[[2]])
#pred_arfima_matrix<-data.matrix(pred_arfima[[2]][["Forecast"]])
pred_arfima_adding_trend<-pred_arfima_matrix[,1]+diff_log_price_trend[2580:2639]

pred_price = matrix(0,61,1)
pred_price[,1][1]<-295.1347521
for (t in 2:61){
  pred_price[,1][t]<-exp(log(pred_price[,1][t-1])+pred_arfima_adding_trend[t-1])
}
error <- validation_price - pred_price[2:61]
rmse_value<-rmse(error)
print(rmse_value)

bytiself
-16476.94
44.02769

use 0.137
-16475.86
29.17552

use 0.275709
-16473.49
29.54318

use 0.0642436
-16472.2
29.91759

use 0.173712
29.26509
-16471.84

               ,fixed = list(frac=0.137)

x<-data.matrix(residuals(arfima)[[1]])
acf(x, lag.max=100, main="acf")
pacf(x, lag.max=100, main="pacf")