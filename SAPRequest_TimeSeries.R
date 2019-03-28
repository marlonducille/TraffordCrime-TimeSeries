
#http://www.trafforddatalab.io/crime_scanner/?code=E05000825&name=Clifford&within=Trafford

library(tseries)
library(forecast)

#df <- read.csv('TraffordCrimeData.csv')
df <- read.csv('SAPRequest_Statistics.csv')


request_ts <- ts(df$Total, start=c(2016, 11), frequency = 12, end=c(2019, 2))
plot(request_ts)

decomposeAP <- decompose(request_ts,"additive")
autoplot(decomposeAP)

diff <- diff(request_ts)
plot(diff)

acf(diff)
pacf(diff)

model <- auto.arima(request_ts, ic = 'aic', trace = TRUE);model

forecast <- forecast(model, level = c(95), h = 12)
autoplot(forecast)

# making predictions
pred <- predict(model, n.ahead = 12*12)
result <- pred$pred; result
plot(crime_ts, xlim=c(2016,2019))
lines(pred$pred, col='blue')
lines(pred$pred + 1.96*pred$se, col='red')
lines(pred$pred - 1.96*pred$se, col='red')
