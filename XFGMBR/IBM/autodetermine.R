autodetermine = function(data) { # Input a time series, Output estimated ARIMA+GARCH model and its prediction
    require(forecast)
    require(tseries)
    require(FinTS)
    require(fGarch)
    require(rugarch)
    # get time series
    tsone = ts(data * 1000)
    leng = length(tsone)
    
    # drop initial zero terms
    ts = tsone
    le = length(ts)
    while (ts[1] == 0) {
        for (i in 1:(le - 1)) {
            ts[i] = ts[(i + 1)]
        }
        ts = ts[1:(le - 1)]
        le = le - 1
    }
    
    # use auto.arima to find ARIMA order and get the differenced series if needed
    autoarma = auto.arima(ts, seasonal = FALSE)
    p1 = as.numeric(autoarma$arma[1])
    q1 = as.numeric(autoarma$arma[2])
    d = as.numeric(autoarma$arma[6])
    series = ts
    difftime = d
    if (difftime > 0) {
        while (difftime != 0) {
            series = series * 1000
            series = diff(series)
            difftime = difftime - 1
        }
    }
    # Test for ARCH Effect
    arch = ArchTest(autoarma$residuals, lag = 12)
    if (arch$p.value > 0.05) {
        optimalfit = arima(series, order = c(p1, 0, q1), method = "ML")
        garch1 = 0
        garch2 = 0
        pred = predict(optimalfit, 1)
        if (d == 1) {
            prevalue = pred$pred/1000 + tsone[leng]
            presd = pred$se/1000
        } else if (d == 2) {
            prevalue = pred$pred/1e+06 + 2 * tsone[leng] - tsone[(leng - 1)]
            presd = pred$se/1e+06
        } else {
            prevalue = pred$pred
            presd = pred$se
        }
        
    } else {
        # fit Garch. If error, that may mean the model is wrong(set AIC=INF),usually (0,1)and(0,2)is not common
        garch10 = try(garchFit(substitute(~arma(p1, q1) + garch(1, 0), list(p1 = p1, q1 = q1)), series, trace = FALSE), 
            silent = TRUE)
        # garch01=try(garchFit(substitute(~arma(p1,q1)+garch(0,1),list(p1=p1,q1=q1)),series,trace=FALSE),silent=TRUE)
        garch11 = try(garchFit(substitute(~arma(p1, q1) + garch(1, 1), list(p1 = p1, q1 = q1)), series, trace = FALSE), 
            silent = TRUE)
        garch20 = try(garchFit(substitute(~arma(p1, q1) + garch(2, 0), list(p1 = p1, q1 = q1)), data = series, trace = FALSE), 
            silent = TRUE)
        # garch02=try(garchFit(substitute(~arma(p1,q1)+garch(0,2),list(p1=p1,q1=q1)),data=series,trace=FALSE),silent=TRUE)
        garch21 = try(garchFit(substitute(~arma(p1, q1) + garch(2, 1), list(p1 = p1, q1 = q1)), data = series, trace = FALSE), 
            silent = TRUE)
        garch22 = try(garchFit(substitute(~arma(p1, q1) + garch(2, 2), list(p1 = p1, q1 = q1)), data = series, trace = FALSE), 
            silent = TRUE)
        
        # ind01=0
        ind10 = 0
        ind11 = 0
        ind20 = 0
        # ind02=0
        ind21 = 0
        ind22 = 0
        if (class(garch10) == "try-error") {
            ind10 = 1
        }
        # if(class(garch01)=='try-error'){ ind01=1 }
        if (class(garch11) == "try-error") {
            ind11 = 1
        }
        if (class(garch20) == "try-error") {
            ind20 = 1
        }
        # if(class(garch02)=='try-error'){ ind02=1 }
        if (class(garch21) == "try-error") {
            ind21 = 1
        }
        if (class(garch22) == "try-error") {
            ind22 = 1
        }
        
        
        if (ind10 == 1) {
            AIC10 = Inf
        } else {
            
            AIC10 = garch10@fit$ics[1]
        }
        
        # if(ind01==1){ AIC01=Inf } else{ AIC01=garch01@fit$ics[1] }
        
        if (ind11 == 1) {
            AIC11 = Inf
        } else {
            
            AIC11 = garch11@fit$ics[1]
        }
        
        if (ind20 == 1) {
            AIC20 = Inf
        } else {
            AIC20 = garch20@fit$ics[1]
        }
        
        # if(ind02==1){ AIC02=Inf } else{
        
        # AIC02=garch02@fit$ics[1] }
        
        if (ind21 == 1) {
            AIC21 = Inf
        } else {
            AIC21 = garch21@fit$ics[1]
        }
        
        if (ind22 == 1) {
            AIC22 = Inf
        } else {
            AIC22 = garch22@fit$ics[1]
        }
        
        nogarch = arima(series, order = c(p1, 0, q1), method = "ML")
        optimalfit = nogarch
        garch1 = 0
        garch2 = 0
        
        # find the optimal fit, if all are error, then it might mean no garch effect.
        value = AIC10
        if (value < Inf) {
            optimalfit = garch10
            value = AIC10
            garch1 = 1
            garch2 = 0
        }
        
        # if(value>AIC01){ optimalfit=garch01 garch1=0 garch2=1 }
        
        
        if (value > AIC11) {
            value = AIC11
            optimalfit = garch11
            garch2 = 1
            garch1 = 1
        }
        
        if (value > AIC20) {
            value = AIC20
            optimalfit = garch20
            garch2 = 0
            garch1 = 2
        }
        
        # if(value>AIC02){ optimalfit=garch02 garch1=0 garch2=2 }
        
        if (value > AIC21) {
            value = AIC21
            optimalfit = garch21
            garch1 = 2
            garch2 = 1
        }
        if (value > AIC22) {
            value = AIC22
            optimalfit = garch22
            garch1 = 2
            garch2 = 2
        }
        
        # make prediction and prepare result output
        if (value == Inf) {
            optimalfit = nogarch
            pred = predict(optimalfit, 1)
            if (d == 1) {
                prevalue = pred$pred/1000 + tsone[leng]
                presd = pred$se/1000
            } else if (d == 2) {
                prevalue = pred$pred/1e+06 + 2 * tsone[leng] - tsone[(leng - 1)]
                presd = pred$se/1e+06
            } else {
                prevalue = pred$pred
                presd = pred$se
            }
            print("No garch fit")
            print(tsone[1])
            print(ts[1])
        } else {
            pred = predict(optimalfit, 1)
            
            if (d == 0) {
                prevalue = pred$meanForecast
                presd = pred$standardDeviation
            } else if (d == 1) {
                prevalue = pred$meanForecast/1000 + tsone[leng]
                presd = pred$standardDeviation/1000
            } else {
                
                prevalue = pred$meanForecast/1e+06 + 2 * tsone[leng] - tsone[(leng - 1)]
                presd = pred$standardDeviation/1e+06
            }
        }
    }
    prevalue = prevalue * 10
    presd = presd * 10
    return(list(ar = p1, ma = q1, d = d, gar1 = garch1, gar2 = garch2, predict = prevalue, predictsd = presd))
}
