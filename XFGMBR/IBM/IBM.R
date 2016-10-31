# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("forecast", "tseries", "fGarch", "FinTS", "wavethresh", "waveslim","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Preparation
y = read.csv("IBM.csv", header = TRUE)
IBMorder = autodetermine(y$spread3y)  # get estimated model ARIMA(2,1,2)+GARCH(2,2)
IBMts = IBM.txt[, c(2, 5)]
IBMts$spread3y = 10000 * IBMts$spread3y
IBMts$date = as.Date(as.character(IBMts$date), "%Y%m%d")
dIBM = diff(IBMts$spread3y)
dIBM = dIBM * 1000
IBMfit = garchFit(substitute(~arma(2, 2) + garch(2, 2)), data = dIBM, trace = FALSE)
IBMfitseries = IBMfit@fitted
IBMfitseries = IBMfitseries/1000
IBMpred = predict(IBMfit, 1)
IBMfitvalue = c()
IBMfitvalue[1] = IBMts$spread3y[1]
for (i in 2:2767) {
    IBMfitvalue[i] = IBMts$spread3y[(i - 1)] + IBMfitseries[(i - 1)]
}
IBMts$fitted = IBMfitvalue
IBMforecast = matrix(0, nrow = 1, ncol = 2)
IBMforecast[1, 1] = "2014-09-22"
IBMforecast[1, 2] = IBMpred$meanForecast/1000 + IBMts$spread3y[2767]
IBMforecast = data.frame(IBMforecast)
colnames(IBMforecast) = c("Date", "Predict")
IBMforecast$Predict = as.numeric(paste(IBMforecast$Predict))
IBMforecast$Predict = log(IBMforecast$Predict)
IBMforecast$Date = as.Date(IBMforecast$Date)
IBMts$spread3y = log(IBMts$spread3y)
IBMts$fitted = log(IBMts$fitted)

# Plot
IBMgg = ggplot(IBMts, aes(x = date, y = spread3y))
IBMgraph = IBMgg + geom_line(aes(y = spread3y, color = "Data")) + geom_line(aes(y = fitted, color = "Fitted")) + 
    geom_point(aes(x = IBMforecast$Date, y = IBMforecast$Predict, color = "Forecast"))
IBMgraph + ggtitle("IBM") + xlab("Time") + ylab("Log price of CDS") + scale_color_manual(name = "series", values = c(Data = "black", 
    Fitted = "green", Forecast = "red"))

## IBM logwave
logIBMts = IBM.txt[, c(2, 5)]
logIBMts$spread3y = log(10000 * logIBMts$spread3y)
logIBMts$date = as.Date(as.character(logIBMts$date), "%Y%m%d")
dlogIBM = diff(logIBMts$spread3y)
dlogIBM = dlogIBM * 1000
for (i in 2767:4096) {
    # By nature of wavelet, we need observations to be a power of 2.
    dlogIBM[i] = dlogIBM[(i - 2766)]
}
# wavelet transformation, thresholding and recovery
logIBMwd = wd(dlogIBM)
logIBMwd2 = threshold(logIBMwd, type = "soft", policy = "universal")
logIBMwd3 = wr(logIBMwd2)

# Preparation
dlogIBM2 = logIBMwd3[1:2766]
logIBMorder = autodetermine(dlogIBM2)  #get estimated model ARIMA(5,0,4)+GARCH(2,2)
logIBMfit = garchFit(substitute(~arma(5, 4) + garch(2, 2)), data = dlogIBM2, trace = FALSE)
logIBMfitseries = logIBMfit@fitted
logIBMfitseries = logIBMfitseries/1000
dlogIBM = dlogIBM/1000
dlogIBM = dlogIBM[1:2766]
logIBMts = logIBMts[-1, ]
logIBMts$spread3y = dlogIBM
logIBMts$fitted = logIBMfitseries

# Plot
logIBMgg = ggplot(logIBMts, aes(x = date, y = spread3y))
logIBMgraph = logIBMgg + geom_line(aes(y = spread3y, color = "Data")) + geom_line(aes(y = fitted, color = "Fitted"))
logIBMgraph + ggtitle("IBM") + xlab("Time") + ylab("Log return of CDS") + scale_color_manual(name = "series", values = c(Data = "black", 
    Fitted = "green"))

