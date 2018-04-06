library('ggplot2')
library('forecast')
library('tseries')

#load the file
daily_data = read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)

#examine the data 

daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")

#cleaning the data
count_ts <- ts(daily_data[,c('cnt')])
daily_data$clean_cnt <- tsclean(count_ts)
ggplot() + geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

#applyting moving average for smoothning and then plotting
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

#applying stl fucntion for decomposition of trend seasonality and cycles

count_ma <- ts(na.omit(daily_data$cnt_ma), frequency = 30)
decomp <- stl(count_ma, s.window = "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)


#making the data stationary

adf.test(count_ma, alternative = "stationary")

#acf and choosing model order (p,d,q) values

acf(count_ma, main = "")
pacf(count_ma, main = "")

#diffrecning by 1 as acf tails off that means it has a trend

count_d1 <- diff(deseasonal_cnt, diffrences = 1)
plot(count_d1)
adf.test(count_d1, alternative ="stationary")

#now testing the acf and pacf

acf(count_d1, main='ACF for Differenced Series')
pacf(count_d1, main='PACF for Differenced Series')

#fitting the arima model (1,1,1)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

#forecasting

fcast <- forecast(fit, h = 30)
plot(fcast, main = "auto arima")




