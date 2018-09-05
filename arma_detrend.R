price <- read.table("D:/Spring2018/STAT6347/Data Set/datasetfortimeanalysis.txt",header = TRUE)
validation_price<-data.matrix(price)[2581:2640]
training_price<-data.matrix(price)[1:2580]
price_timeserie<-ts(training_price, frequency=1)

#percentage return change
diff_log_price_timeserie<-diff(log(price_timeserie))

#detrend
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

#use auto arima
library("forecast")
auto_arima<-auto.arima(detrend_diff_log_price_timeserie,start.p = 10,trace=TRUE,seasonal=FALSE,ic="aic")

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
    arima<-arima(detrend_diff_log_price_timeserie,order=c(i,0,j))
    print(AIC(arima))
    options(warn=1)
    aic_matrix[j]<-AIC(arima)
    
    pred_arima<-predict(arima,n.ahead=60)
    pred_arima_df<-data.frame(pred_arima)
    pred_arima_matrix<-data.matrix(pred_arima_df)
    pred_arfima_adding_trend<-pred_arima_matrix[,1]+diff_log_price_trend[2580:2639]
    
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
  write.xlsx(aic_matrix, "D:/Spring2018/STAT6347/project/aic.xlsx", sheetName=nam, append=TRUE)
  write.xlsx(rmse_matrix, "D:/Spring2018/STAT6347/project/rmse.xlsx", sheetName=nam, append=TRUE)
}

#arma 5 14 prediction
arima<-arima(detrend_diff_log_price_timeserie,order=c(5,0,14))
tsdiag(arma_5_14, gof.lag= 100)

library(lawstat)
shapiro.test(arma_5_14$res)
qqnorm(arma_5_14$residuals)
qqline(arma_5_14$residuals)

par(mfcol=c(2,1)) 
  acf( arma_5_14$residuals^2 )
  pacf(arma_5_14$residuals^2 )

library("fGarch")
garch<-garchFit(~arma(5,14)+ garch(8,8), data = detrend_diff_log_price_timeserie, trace=FALSE, cond.dist="sstd")
summary(garch)

garch_res<-garch@residuals/garch11@sigma.t
garch_res2<-(garch@residuals/garch11@sigma.t)^2

par(mfcol=c(2,2)) 
acf(garch_res,  na.action = na.exclude,  main="ACF for standardized residuals" )
pacf( garch_res, na.action = na.exclude, main="PACF for standardized residuals" ) 

acf( garch_res2 , na.action = na.exclude,  main="ACF for Squared standardized residuals")
pacf( garch_res2, na.action = na.exclude,  main="PACF for Squared standardized residuals" )

par(mfcol=c(1,1))
plot( garch, which= 13)

predict(garch11, n.ahead = 120)



pred_x<-data.matrix(price)[61:2640]
pred_x_ts<-ts(pred_x, frequency=1)
difflog_x_ts<-diff(log(pred_x_ts))

difflog_x_matrix<-data.matrix(difflog_x_ts)
detrend_x_matrix = difflog_x_matrix - diff_log_price_trend[61:2639]

detrend_x_ts<-ts(detrend_x_matrix, frequency=1)

pred_arima<-predict(arima,newdata=detrend_x_ts,n.ahead=60)

pred_arima_y<-data.frame(pred_arima[1])
y<-data.matrix(pred_arima_y)
pred_arima_se<-data.frame(pred_arima[2])
se<-data.matrix(pred_arima_se)
trend_in_pred<-data.frame(diff_log_price_trend[2580:2639])
trend<-data.matrix(trend_in_pred)
