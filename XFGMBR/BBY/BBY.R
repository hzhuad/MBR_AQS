# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages and function
libraries = c("forecast", "tseries", "fGarch", "FinTS","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

source("autodetermine.R")

# Prepration

BBY = read.csv("BBY.csv")  # import data
BBYorder = autodetermine(x$spread3y)  # get estimated model ARIMA(1,1,1)
BBYts = BBY[, c(2, 5)]
BBYts$spread3y = 10000 * BBYts$spread3y
BBYts$date = as.Date(as.character(BBYts$date), "%Y%m%d")
BBYfit = Arima(BBY$spread3y, order = c(1, 1, 1), method = "ML")
BBYfitvalue = fitted(BBYfit)  # get fitted value
BBYts$fitted = BBYfitvalue
BBYforecast = matrix(0, nrow = 1, ncol = 2)
BBYforecast[1, 1] = "2014-09-22"
BBYforecast[1, 2] = 117.2822
BBYforecast = data.frame(BBYforecast)
colnames(BBYforecast) = c("Date", "Predict")
BBYforecast$Predict = as.numeric(paste(BBYforecast$Predict))
BBYforecast$Predict = log(BBYforecast$Predict)
BBYforecast$Date = as.Date(BBYforecast$Date)

BBYts$spread3y = log(BBYts$spread3y)
BBYts$fitted = log(BBYts$fitted)

# plot
BBYgg = ggplot(BBYts, aes(x = date, y = spread3y))
BestBuygraph = BBYgg + geom_line(aes(y = spread3y, color = "Data")) + geom_line(aes(y = fitted, color = "Fitted")) + 
    geom_point(aes(x = BBYforecast$Date, y = BBYforecast$Predict, color = "Forecast"))
BestBuygraph + ggtitle("BestBuy Co.") + xlab("Time") + ylab("Log price of CDS") + scale_color_manual(name = "series", 
    values = c(Data = "black", Fitted = "green", Forecast = "red"))


