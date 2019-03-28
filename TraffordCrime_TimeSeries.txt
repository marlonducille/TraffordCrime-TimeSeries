
#http://www.trafforddatalab.io/crime_scanner/?code=E05000825&name=Clifford&within=Trafford

library(tseries)
library(forecast)

df <- read.csv('TraffordCrimeData.csv')
df <- df[df$category=='All crime',]
df <- df[order(df$date),]

crime_ts <- ts(df$count, start=c(2016, 1), frequency = 12, end=c(2018, 12))
plot(crime_ts)

decomposeAP <- decompose(crime_ts,"additive")
autoplot(decomposeAP)

diff <- diff(crime_ts)
plot(diff)

acf(diff)
pacf(diff)

model <- auto.arima(crime_ts, ic = 'aic', trace = TRUE);model

forecast <- forecast(model, level = c(95), h = 12)
autoplot(forecast)
