library(rugarch)
library(rmgarch)
library(tseries)
library(forecast)
library(moments)

inr<-read.csv(file.choose())
inr.data=data.frame(inr)
View(inr.data)


#Descriptive Study
summary(inr.data$INR)
var(inr.data$INR)
sd(inr.data$INR)
skewness(inr.data$INR)
kurtosis(inr.data$INR)
range(inr.data$INR)

inr.1<-ts(inr.data$INR, frequency = 365.5)
plot(inr.1)



#GARCH,ARCH

#Testing Presence of ARCH Effects

library(FinTS)

Atest<-ArchTest(inr.1,lags =1, demean = TRUE)
Atest

#Suitable ARMA Model
fitarfima<-autoarfima(data=inr.1, ar.max = 5, ma.max = 5, criterion = "AIC", method="partial")
fitarfima
fitarfima$fit

#Model Specification
gcft<-ugarchspec(variance.model = list(garchOrder = c(1, 1) ),mean.model = list(armaOrder=c(4,3)))
gcft1<-ugarchspec(variance.model = list(garchOrder = c(1, 2) ),mean.model = list(armaOrder=c(4,3)))
gcft2<-ugarchspec(variance.model = list(garchOrder = c(0, 1) ),mean.model = list(armaOrder=c(4,3)))
gcft3<-ugarchspec(variance.model = list(garchOrder = c(0, 2) ),mean.model = list(armaOrder=c(4,3)))



gcft.fit<-ugarchfit(spec=gcft, data=inr.1)
gcft.fit1<-ugarchfit(spec=gcft1, data=inr.1)
gcft.fit2<-ugarchfit(spec=gcft2, data=inr.1)
gcft.fit3<-ugarchfit(spec=gcft3, data=inr.1)


#Model Selection
infocriteria(gcft.fit)
infocriteria(gcft.fit1)
infocriteria(gcft.fit2)
infocriteria(gcft.fit3)



plot.ts(sigma(gcft.fit), ylab="sigma(t)", col="red")




#Standardized Residuals
gcft.resid<-data.frame(residuals(gcft.fit,standardize=TRUE))
gcft.resid2<-data.frame(residuals(gcft.fit,standardize=TRUE)^2)

#Standardized Residuals Tests
Box.test(gcft.resid$residuals.gcft.fit, type="Ljung")
Box.test(gcft.resid2$residuals.gcft.fit, type="Ljung")
ArchTest(gcft.resid$residuals.gcft.fit)
jarque.bera.test(gcft.resid$residuals.gcft.fit)
shapiro.test(gcft.resid$residuals.gcft.fit)


#Forecast
gfct<-ugarchforecast(gcft.fit, n.ahead = 15)
gfct
plot(gfct)
