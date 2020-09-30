
library(astsa)
library("fUnitRoots")
library(readr)

GD.dat <- read_csv("C:/Users/Jordan/Downloads/GD.dat.txt")

dim(GD.dat)

urkpssTest(GD.dat[,1], type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(GD.dat[,1], differences=30)
plot(tsstationary)

acf(diff(diff(GD.dat[,1],6)))
pacf(diff(diff(GD.dat[,1],6)))

par(mfrow=c(3,1))

ts.plot(GD.dat[,1], col =4)
ts.plot(diff(GD.dat[,1]), col=4)
ts.plot(diff(diff(GD.dat[,1],6)), col=4)


require(tseries)
adf.test(diff(diff(GD.dat[,1],30)))


m1=sarima(GD.dat[,1], p=1,d=1,q=1, P=1,D=1,Q=0,30)
m2=sarima(GD.dat[,1], p=3,d=1,q=1, P=1,D=1,Q=0,6)
m3=sarima(GD.dat[,1], p=5,d=1,q=1, P=1,D=1,Q=0,30)
m4=sarima(GD.dat[,1], p=1,d=1,q=1, P=1,D=1,Q=0,30)


c(m1$AIC,m1$AICc,m1$BIC)
c(m2$AIC,m2$AICc,m2$BIC)
c(m3$AIC,m3$AICc,m3$BIC)
c(m4$AIC,m4$AICc,m4$BIC)

sarima.for(GD.dat[,1], 20, 1,1,1, 1,1,0,30)

par(mfrow=c(2,1))
ts.plot(GD.dat[,1], col =4)
sarima.for(GD.dat[,1], 50, 1,1,1, 1,1,0,30)

adf.test(GD.dat[,2])

urkpssTest(GD.dat[,2], type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(GD.dat[,2], difference = 30)
plot(tsstationary)

par(mfrow=c(3,1))

ts.plot(GD.dat[,2], col=4)
ts.plot(diff(GD.dat[,2]), col=4)
ts.plot(diff(diff(GD.dat[,2],30)), col=4)

acf(diff(diff(GD.dat[,2],10))) 
pacf(diff(diff(GD.dat[,2],30))) 

M1 =sarima(GD.dat[,2], p=0,d=1,q=1, P=1,D=1,Q=0,12)
M2 =sarima(GD.dat[,2], p=1,d=1,q=1, P=0,D=1,Q=1,12)
M3 =sarima(GD.dat[,2], p=0,d=1,q=3, P=0,D=1,Q=1,12)
M4 =sarima(GD.dat[,2], p=0,d=1,q=3, P=0,D=1,Q=1,12)

c(M1$AIC,M1$AICc,M1$BIC)
c(M2$AIC,M2$AICc,M2$BIC)
c(M3$AIC,M3$AICc,M3$BIC)
c(M4$AIC,M4$AICc,M4$BIC)

sarima.for(GD.dat[,2], 20, 0,1,3, 0,1,1,12)

par(mfrow=c(2,1))
ts.plot(GD.dat[,1], col =4)
sarima.for(GD.dat[,1], 50, 1,1,1, 1,1,0,30)

