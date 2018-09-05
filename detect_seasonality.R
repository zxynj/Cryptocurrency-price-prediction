price <- read.table("D:/Spring2018/STAT6347/Data Set/datasetfortimeanalysis.txt",header = TRUE)
training_price<-data.matrix(price)[1:2580]
price_timeserie<-ts(training_price, frequency=1)

#percentage return change
diff_log_price_timeserie<-diff(log(price_timeserie))

# find out the 10 highest "power" frequencies
library("TSA")
p = periodogram(diff_log_price_timeserie)

dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top10 = head(order, 10)

top10

time = 1/top10$f
time

#validate
price_timeserie<-ts(training_price, frequency=5.2)

fit <- tbats(price_timeserie)
seasonal <- !is.null(fit$seasonal)
seasonal