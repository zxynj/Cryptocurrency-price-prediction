price <- read.table("D:/Spring2018/STAT6347/Data Set/datasetfortimeanalysis.txt",header = TRUE)
validation_price<-data.matrix(price)[2581:2640]
training_price<-data.matrix(price)[1:2580]
price_timeserie<-ts(training_price, frequency=1)

#percentage return change
diff_log_price_timeserie<-diff(log(price_timeserie))

#study ts
acf(diff_log_price_timeserie, lag.max=100, main="acf")
pacf(diff_log_price_timeserie, lag.max=100, main="pacf")

#test stationary
library("tseries")
adf.test(diff_log_price_timeserie, alternative="stationary")

#use auto arima
library("forecast")
auto_arima<-auto.arima(diff_log_price_timeserie,start.p = 10,trace=TRUE,seasonal=FALSE,ic="aic")

#define rmse function
rmse <- function(error)
{
  sqrt(mean(error^2))
}

#run arma p:1-14  q:1-14
library(xlsx)

for (i in 1:14){
  aic_matrix<- matrix(0,14,1)
  rmse_matrix<- matrix(0,14,1)
  print(paste(i))
  for (j in 1:14){
    print(paste(j))
    arima<-arima(diff_log_price_timeserie,order=c(i,0,j))
    print(AIC(arima))
    aic_matrix[j]<-AIC(arima)
    options(warn=1)
    
    pred_arima<-predict(arima,n.ahead=60)
    pred_arima_df<-data.frame(pred_arima)
    pred_arima_matrix<-data.matrix(pred_arima_df)
    pred_arfima<-pred_arima_matrix[,1]
    
    pred_price = matrix(0,61,1)
    pred_price[,1][1]<-295.1347521
    for (t in 2:61){
      pred_price[,1][t]<-exp(log(pred_price[,1][t-1])+pred_arfima[t-1])
    }
    error <- validation_price - pred_price[2:61]
    rmse_value<-rmse(error)
    rmse_matrix[j]<-rmse_value
    print(rmse_value)
  }
  nam <- paste("p",i, sep="_")
  write.xlsx(aic_matrix, "D:/Spring2018/STAT6347/project/aic.xlsx", sheetName=nam, append=TRUE)
  write.xlsx(rmse_matrix, "D:/Spring2018/STAT6347/project/rmse.xlsx", sheetName=nam, append=TRUE)
}

#check for opti converge warning
arima<-arima(diff_log_price_timeserie,order=c(9,0,9))
