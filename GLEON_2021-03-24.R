################################################################################
# Time-series introduction for ecologists
# 2021-03-31
# Ulrike Obertegger, ulrike.obertegger@fmach.it
################################################################################
# What is a Time Series ?
# Any metric that is measured over time at regular or irregular intervals 
# impute missing data (library MICE); threshold of 5% of the total for large datasets

# most statistical test rely on 
# independence, 
# heteroscedasticity if the variability of the random disturbance is different across elements of the vector.
# normality

# How to create a Time Series in R ?
data("AirPassengers")
str(AirPassengers)
library(tidyverse)
glimpse(AirPassengers)
par(mfrow=c(4,1))
# If you get a "figure margins too large" error, 
# try enlarging the plotting window, or type:
# par(mar=c(1,1,1,1))
plot(ts(AirPassengers, frequency = 4,  start = c(1959, 2)))          # frequency 4 => Quarterly Data)
plot(ts(AirPassengers, frequency = 12, start = 1990))                # freq 12 => Monthly data. 
plot(ts(AirPassengers, start=c(1877), end=c(2021), frequency=1))     # Yearly Data
library(lubridate)                                                   # weekly data
plot(ts(AirPassengers, 
   freq=365.25/7, 
   start=decimal_date(ymd("2006-12-27"))))

# What is Autocorrelation and Partial-Autocorrelation?
# Autocorrelation is the correlation of a Time Series with lags of itself. 
# It shows if the previous states (lagged observations) of the time series has an 
# influence on the current state. 
par(mfrow=c(1,1))
acf(AirPassengers)
ACF<-acf(AirPassengers)
ACF$acf

# threshold for statistical significance
N=144
1.96/sqrt(N)

pacf(AirPassengers)
# Partial Autocorrelation is the correlation of the time series with a lag of itself, 
# with the linear dependence of all the lags between them removed.
# controlling for other lags

# Each data point (Yt) at time t in a Time Series can be expressed as either a sum or a product of 3 components, 
# namely, Seasonality (St), Trend (Tt) and Error (et) (a.k.a White Noise).

# For Additive Time Series,

# Yt = St + Tt + errort

# For Multiplicative Time Series,

# Yt = St � Tt � errort

# A multiplicative time series can be converted to additive by taking a log of the time series.

# additiveTS <- log (multiplicativeTS)  # often converts multiplicative to additive time serie
par(mfrow=c(1,2))
plot(AirPassengers)
additiveTS <- log(AirPassengers)
plot(additiveTS)

# What is a Stationary Time Series?
# A time series is said to be stationary when 
# The mean value of time-series is constant over time, which implies, the trend 
# component is nullified.
# The variance does not increase over time.
# Seasonality effect is minimal.

# Time series that show no autocorrelation are called white noise
                      
# It is used commonly to determine if the time series is stationary or not. 
# stationary time series will have the autocorrelation fall to zero fairly quickly 
# but for a non-stationary series it drops gradually.

# Thus, time series with trends, or with seasonality, are not stationary — 
# the trend and seasonality will affect the value of the time series at different times.

# Concept of stationarity and the technique of differencing time series.

library(forecast)
# Transformations such as logarithms can help to stabilise the variance of a time series. 
# Differencing can help stabilise the mean of a time series by removing changes in the level 
# of a time series, and therefore eliminating (or reducing) trend and seasonality.

library(fpp2)
plot(goog200)
acf(goog200)

# compute the differences between consecutive observations
plot(diff(goog200))
acf(diff(goog200))

Box.test(goog200, lag=10, type="Ljung-Box")
Box.test(diff(goog200), lag=10, type="Ljung-Box")

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: the null hypothesis is that the data are stationary, 
# and we look for evidence that the null hypothesis is false.
# ur.kpss() function from the urca package.

library(urca)
plot(goog)
goog %>% ur.kpss() %>% summary()
# The test statistic is much bigger than the 1% critical value, 
# indicating that the null hypothesis is rejected. 

goog %>% diff() %>% ur.kpss() %>% summary()
plot(diff(goog))

# While exponential smoothing models are based on a description of the trend and seasonality in the data, 
# ARIMA models aim to describe the autocorrelations in the data.

# ARIMA
# autoregressive integrated moving average
# AR
# In an autoregression model, we forecast the variable of interest using a 
# linear combination of p-past values of the variable. AR(p)

# moving average model uses past forecast errors in a regression-like model.
# MA(q) model, a moving average model of order q.

# ARIMA(p,d,q)
autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")

auto.arima(uschange[,"Consumption"], seasonal=FALSE)

# end of the introduction
# have a look at some data
################################################################################
setwd("your path for the data")
dat<-read.csv("dat1.csv")
head(dat)
par(mfrow=c(1,1))
plot(dat$x, type="o")

library(tseries)
kpss.test(dat$x)
acf(dat$x)
pacf(dat$x)

data<-ts(dat$x, frequency=12, start=c(2002,2))
plot(data)

(t<-time(data))
# (t<-seq(1,length(dat$x), by=1))

mod.0<-lm(dat$x~t)
summary(mod.0)
abline(mod.0)

acf(residuals(mod.0))
pacf(residuals(mod.0))
plot(residuals(mod.0))
abline(h=0)

library(nlme)
dati<-dat$x
# corARMA
mod0<-gls(dati~t)
mod1<-gls(dati~t, correlation=corAR1(form=~1))
anova(mod0, mod1)
resid<-residuals(mod1, type="normalized")
acf(resid)
summary(mod1)

library(rcompanion)
nagelkerke(mod1)

abline(mod1)
predict(mod1)
(11.031234-6.563517)/90     # delta
(11.031234-6.563517)/90*12 # monthly data

#### ARIMA
diff2<-diff(dati)
kpss.test(diff2)

(fit2<-auto.arima(dati, ic="aic"))
(fit2<-Arima(dati, order=c(2,1,2), include.drift=TRUE))
resi2<-fit2$residuals
acf(resi2)
acf(resi2^2)

length(dati)
-0.0399 + c(-1.96, 1.96)*0.0149 # CI for the estimate of the drift (=trend)

str(fit2)
fit2$fitted

(10.230723-6.726570)/90  # delta
(10.230723-6.726570)/90*12

# compare ARIMA with gls
# [1] 0.4672204 vs 0.5956956

########################### the classical approach #############################
# water level of the Nile
data("Nile")
Nile
plot(Nile)
acf(Nile)
pacf(Nile)

# the classic approach
library(Kendall)
MannKendall(Nile)

library(openair)
# there is a trend, then we can use non-parametric Sen Theil test to assess the trend

Time<-seq(as.POSIXct("1871-01-01 00:00:00", tz="UTC"),
          as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
          by="year")

input<-as.numeric(Nile)
data <-data.frame(date=Time, 
                 water.level=input)
head(data)
glimpse(data)
plot(data$date, data$water.level, type="o")

st<-TheilSen(data, pollutant = "water.level")
st$data

# inspect ACF
# residuals = predicted - observed
slope     <-st$data$res2$slope[1]
intercept <-st$data$res2$intercept[1]

obs    <-data$water.level
pred.st<-intercept+slope*seq(1, 100, 1)

resi<-obs - pred.st
acf(resi)

# we neglect the misfit and inspect the RMSE
sqrt(sum((pred.st-obs)^2/100))  # 100 observations

library(Metrics)
rmse(obs, pred.st)

## Nile data with one breakpoint: the annual flows drop in 1898
## because the first Ashwan dam was built
## compute breakpoints
library(strucchange)
# this model says that there is just an intercept
# and we are looking for a change in the intercept
bp.nile <- breakpoints(Nile ~ 1)

## fit and visualize segmented and unsegmented model

plot(Nile)
fm0 <- lm(Nile ~ 1)
fm1 <- lm(Nile ~ breakfactor(bp.nile, breaks = 1))
lines(y=fitted(fm0), x=seq(1871,1970,1), col = 3)
lines(fitted(fm1),   x=seq(1871,1970,1), col = 4, lwd=4)
lines(bp.nile, breaks = 1)
summary(fm1)

acf(residuals(fm1))

# compare two competing models by the RMSE
### RMSE
part1<-rep(1097.75, 28)
part2<-rep((1097.75-247.78), 100-28)
pred.cp<-c(part1, part2)

rmse(obs, pred.cp)
# versus
rmse(obs, pred.st)
### not considering a change point can let you find a trend that is not there

##### change in the slope
# Computation of breakpoints in regression relationships. Given a number of breaks the function
# computes the optimal breakpoints.
# the BIC would choose 0 breakpoints although the RE and supF test
# clearly reject the hypothesis of structural stability. Bai &
# Perron (2003) report that the BIC has problems in dynamic regressions.
# due to the shape of the RE process of the F statistics choose two
# breakpoints and fit corresponding models

################################################################################
rm(list=ls(all=TRUE))
data("nottem")
nottem<-(nottem-32)/1.8  # from Fahrenheit to celsius
# 20 years of monthly temperature measurements at Nottingham Castle (ts object) 
# (Anderson, O. D. 1976). 
par(mfrow=c(1,1))
plot(nottem)
temp<-nottem
class(temp)

length(temp)

# the classic
library(trend)
# seasonal Kendall test for trend 
# R. Hirsch, J. Slack, R. Smith (1982), 
# Techniques of Trend Analysis for Monthly Water Quality Data, Water Resources Research 18, 107–121.
# there may be instances where some month exhibit a strong upward trend and others exhibit a strong downward trend,
# and thus the SMK test indicates no trend. The SMK test is specifically designed to provide single summary statistic 
# for the entire record and will indicate trend when there are trends in opposing directions in different months
(res <- smk.test(temp))
## print method
## summary method
summary(res)
sum(res$Sg)
###################################################
library(bfast)
# Break Detection in the Seasonal and Trend Component of a Univariate Time Series
fit<-bfast(temp,h=0.15,season="harmonic",max.iter=10)
plot(fit)
fit
str(fit)
plot(fit$output[[1]]$Tt)  # Trend
plot(fit$output[[1]]$St)  # seasonal

############## # * # modeling time-series with cosine trends # * # #############
# plotting of raw data
plot(temp, ylab="mean monthly air temperature")

# periodogram
# spec.pgram(temp, spans=c(5,5))

## linear regression with trend and harmonic seasonality (of order 2)
library("dynlm")
dm0 <- dynlm(temp ~ trend(temp) + harmon(temp, order = 2))
summary(dm0)
# seasonal component is a cyclical pattern of annually
# recurring deviations from the long-term mean
# trend is significant, annual sine wave highly significant (cos1/sin1),
# biannual sine wave also significant (cos2/sin2).

## unfortunately strucchange cannot use the nice formula above
## directly -> so extract the trend regressor and the matrix with
## the harmonic regressors
dtemp        <- data.frame(temp = temp)
dtemp$trend  <- model.matrix(dm0)[, 2]
dtemp$harmon <- model.matrix(dm0)[, 3:6]
colnames(dtemp$harmon) <- c("cos1", "cos2", "sin1", "sin2")
m0 <- lm(temp ~ trend + harmon, data = dtemp)
summary(m0)
## --> same as above

## supF test
library("strucchange")
fs <- Fstats(temp ~ trend + harmon, data = dtemp, from = 0.1)
plot(fs)
sctest(fs)

(bp <- breakpoints(temp ~ trend + harmon, data = dtemp, breaks=1, h=0.1))

confint(bp)
## 

## inspect fitted coefficients in both segments
coef(bp)

## M-fluctuation tests
scus <- gefp(temp ~ trend + harmon, data = dtemp)
plot(scus, functional = supLM(0.1))

plot(scus, aggregate = FALSE, ylim = c(-2.2, 2.2))

coef(m0)

## visualization
plot(temp, col = "black", lwd = 2, ylab="monthly air temperature")
X<-time(temp)
fit<-fitted(m0)
lines(x=as.vector(X), y=as.vector(fit), col="red", type="l")

## trend
lines(ts(coef(m0)[1] + coef(m0)[2] * dtemp$trend,
         start = 1920, freq = 12), col = 4, lwd = 2, lty = 5)
summary(m0)
# slope is 0.03/?
coef(m0)[1] + coef(m0)[2] * dtemp$trend # how we get the values
(9.156176-9.776694) # increase in 240 month
(9.156176-9.776694)/240*12 
# slope is 0.03/year
0.0310259*20 # the total increase

## seasonality patterns
a<-coef(m0)[3:6]

harmoncos1<-coef(m0)[3]
harmoncos2<-coef(m0)[4]
harmonsin1<-coef(m0)[5]
harmonsin2<-coef(m0)[6]
(a<-c(harmoncos1,harmoncos2,harmonsin1,harmonsin2))

seas <- model.matrix(m0)[1:12,3:6] %*% a

# head(model.matrix(m0))[,1:6]
#(Intercept)     trend harmoncos1 harmoncos2    harmonsin1    harmonsin2
#1         1 0.1666667        1.0        1.0 -9.308876e-13 -1.861775e-12
#2         1 0.3333333        0.5       -0.5  8.660254e-01  8.660254e-01
#3         1 0.5000000       -0.5       -0.5  8.660254e-01 -8.660254e-01
#4         1 0.6666667       -1.0        1.0  6.006076e-13 -1.201215e-12
#5         1 0.8333333       -0.5       -0.5 -8.660254e-01  8.660254e-01
#6         1 1.0000000        0.5       -0.5 -8.660254e-01 -8.660254e-01

#plot(head(model.matrix(m0))[,2],head(model.matrix(m0))[,3], type="l")

colnames(seas) <-c("1973-2012")
rownames(seas) <- month.abb[1:12]
barplot(t(seas), beside = TRUE, legend = TRUE)
seas

## annual amplitude and phase
sqrt(coef(m0)[3]^2 + coef(m0)[5]^2)
atan(-coef(m0)[5]/coef(m0)[3])

## bi-annual amplitude and phase
sqrt(coef(m0)[4]^2 + coef(m0)[6]^2)
atan(-coef(m0)[6]/coef(m0)[4])

## with respect to the long-term mean

## continuous seasonality patterns
tr <- 0:365/365    
# tr goes from 0 to 1 in 365 steps
h1 <- cbind("cos" = cos(2 * pi * 1 * tr), "sin" = sin(2 * pi * 1 * tr))
h2 <- cbind("cos" = cos(2 * pi * 2 * tr), "sin" = sin(2 * pi * 2 * tr))
s1a <- h1 %*% coef(m0)[c(3,5)]   # harmon 1 period 1
# coef(m0)[c(3,5)] 
# harmoncos1 harmonsin1 
# -9.471048  -2.706701 
s2a <- h2 %*% coef(m0)[c(4,6)]   # harmon 2 period 1

#### visualization
par(mfrow = c(1, 2))

plot(tr, s1a + s2a, type = "l", lty = 1,
     xlab = "Time (within year)", ylab = "Seasonality",
     main = "Combined")
legend("bottomleft", colnames(seas), lty = 1, bty = "n")

plot(tr, s1a, type = "l", lty = 1, col = "blue",
     xlab = "Time (within year)", ylab = "Seasonality", main = "Separate")
lines(tr, s2a, col = "red", lty = 2)
legend("bottomleft", c(colnames(seas), "annual", "bi-annual"), lty = c(1,1, 2),
       col = c(1, 4, 2), bty = "n")

#### check residuals from breakpoint model for autocorrelation
par(mfrow = c(1,2))
acf(residuals(m0))
pacf(residuals(m0))

# Ljung–Box test for examining the 0-hypothesis of independence in a time series
Box.test(residuals(m0), type = "Ljung", lag = 12)

## normality seems to be roughly ok
hist(residuals(m0))
qqnorm(residuals(m0))
qqline(residuals(m0))

###### what we could do
library(nlme)
m0 <- gls(temp ~ trend + harmon,
          data = dtemp)
m1 <- gls(temp ~ trend + harmon, correlation =corAR1(form=~1),
          data = dtemp)
summary(m1)
anova(m0,m1)

par(mfrow=c(1,1))
plot(temp, col = "black", lwd = 2, ylab="monthly air temperature")
X<-time(temp)
fit<-fitted(m1)
lines(x=as.vector(X), y=as.vector(fit), col="red", type="l")

## Flaim et al. 2016 Freshwater Biology example of struchchange and cosine trends 
################################################################################
## set up data frame with all auxiliary variables
plus<-c(rep(0,120), rep(10, 120))
tempi<-temp+plus
plot(tempi)
library(trend)
(res <- smk.test(tempi))

## summary method
summary(res)

fit<-bfast(tempi,h=0.15,season="harmonic",max.iter=10)
plot(fit)
fit

########### for safety reasons let`s do the analysis step by step
dtempi      <- data.frame(tempi = tempi)
dtempi$time <- as.numeric(time(tempi)) + 1/12

dtempi$trend <- dtempi$time - 1975
## -> trend uses 1975 as origin

dtempi$harmon1 <- cbind("cos"=cos(2*pi*1*dtempi$time),"sin"=sin(2*pi*1*dtempi$time))
dtempi$harmon2 <- cbind("cos"=cos(2*pi*2*dtempi$time),"sin"=sin(2*pi*2*dtempi$time))
## -> separate harmonic matrices for annual and bi-annual patterns
head(dtempi)
## regression model
m0 <- lm(tempi ~ trend + harmon1 + harmon2, data = dtempi)
summary(m0)

## supF test
library("strucchange")
fs <- Fstats(tempi ~ trend + harmon1 + harmon2, data = dtempi, from = 0.1)
plot(fs)

sctest(fs)

(bp <- breakpoints(tempi ~ trend + harmon1 + harmon2, data = dtempi, breaks=1, h=0.1))

confint(bp)

## M-fluctuation tests
dtempo        <- data.frame(tempi = tempi)
dtempo$trend  <- model.matrix(dm0)[, 2]
dtempo$harmon <- model.matrix(dm0)[, 3:6]
scus <- gefp(tempi ~ trend + harmon, data = dtempo)
plot(scus, functional = supLM(0.1))

plot(scus, aggregate = FALSE, ylim = c(-2.2, 2.2))

## inspect fitted coefficients in both segments
coef(bp)

seg <- breakfactor(bp)

# two different possibilities how to get the results
seglm<-lm(tempi ~ 0+ seg/trend+ seg/harmon1 + seg/harmon2, data = dtempi)  # values of the intercept as they are
seglm<-lm(tempi ~ seg/trend+ seg/harmon1 + seg/harmon2, data = dtempi)     # baseline 
summary(seglm)

## visualization
par(mfrow=c(1,1))
plot(tempi, col = "black", lwd = 2, ylab="tempi")
lines(fitted(bp))
lines(confint(bp))

## trends
seg <- breakfactor(bp)
lines(ts(coef(bp)[seg, 1] + coef(bp)[seg, 2] * dtempi$trend,
         start = 1920, freq = 12), col = "blue", lwd = 2, lty = 5)

as.matrix(ts(coef(bp)[seg, 1] + coef(bp)[seg, 2] * dtempi$trend,
             start = 1920, freq = 12))

## seasonality patterns (at observed time points)
seas <-model.matrix(m0)[1:12, 3:6] %*% t(coef(bp)[, 3:6])
rownames(seas) <- apply(matrix(month.abb, 1, 12), 2, paste, collapse = "/")
barplot(t(seas), beside = T, legend = T, col=c("red","blue"))

## -> amplitude increases over time

## continuous seasonality patterns
tr <- 0:365/365    
# tr geht von 0 bis 1 in 365 Schritten
h1 <- cbind("cos" = cos(2 * pi * 1 * tr), "sin" = sin(2 * pi * 1 * tr))
h2 <- cbind("cos" = cos(2 * pi * 2 * tr), "sin" = sin(2 * pi * 2 * tr))
s1a <- h1 %*% coef(bp)[1, 3:4]   # harmon 1 period 1
# > coef(bp)[1, 3:4]
# harmon1cos harmon1sin 
# -621.3082  -598.2405 
s2a <- h2 %*% coef(bp)[1, 5:6]   # harmon 2 period 1
s1b <- h1 %*% coef(bp)[2, 3:4]   # harmon 1 period 2
s2b <- h2 %*% coef(bp)[2, 5:6]   # harmon 2 period 2

par(mfrow = c(1, 2))

plot(tr, s1b + s2b, type = "l", lty = 1,
     xlab = "Time (within year)", ylab = "Seasonality",
     main = "Combined")
lines(tr, s1a + s2a, lty = 3)
legend("topleft", colnames(seas), lty = c(3,1), bty = "n")

plot(tr, s1b, type = "l", lty = 1, col = 2,
     xlab = "Time (within year)", ylab = "Seasonality", main = "Separate")
lines(tr, s1a, col = 2, lty = 2)
lines(tr, s2b, col = 4, lty = 1)
lines(tr, s2a, col = 4, lty = 2)
legend("topleft", c(colnames(seas), "annual", "bi-annual"), lty = c(2,1,1,2),
       col = c(1, 1, 2, 4), bty = "n")

cbind(c(s1a + s2a),c(s1b + s2b))  # extract the values

## annual amplitude and phase
sqrt(coef(bp)[,3]^2 + coef(bp)[,4]^2)
atan(-coef(bp)[,4]/coef(bp)[,3])

## bi-annual amplitude and phase
sqrt(coef(bp)[,5]^2 + coef(bp)[,6]^2)
atan(-coef(bp)[,6]/coef(bp)[,5])

## 
## check residuals from breakpoint model for autocorrelation
par(mfrow = c(1, 2))
acf(residuals(bp))
pacf(residuals(bp))

## --> virtually no autocorrelation left, Box-Ljung test non-significant
# Ljung–Box test for examining the 0-hypothesis of independence in a time series
Box.test(residuals(bp), type = "Ljung", lag = 6)

## normality seems to be roughly ok
hist(residuals(bp))
qqnorm(residuals(bp))
qqline(residuals(bp))

################################################################################
## new data
plus<-c(rep(0,120), seq(1:120))
tempii<-temp+plus
plot(tempii)

library(trend)
(res <- smk.test(tempii))
## summary method
summary(res)

fit<-bfast(tempii,h=0.15,season="harmonic",max.iter=10)
plot(fit)
fit

dtempii      <- data.frame(tempii = tempii)
dtempii$time <- as.numeric(time(tempii)) + 1/12

dtempii$trend <- dtempii$time - 1975
## -> trend uses 1975 as origin

dtempii$harmon1 <- cbind("cos"=cos(2*pi*1*dtempii$time),"sin"=sin(2*pi*1*dtempii$time))
dtempii$harmon2 <- cbind("cos"=cos(2*pi*2*dtempii$time),"sin"=sin(2*pi*2*dtempii$time))
## -> separate harmonic matrices for annual and bi-annual patterns

## regression model
m0 <- lm(tempii ~ trend + harmon1 + harmon2, data = dtempii)
summary(m0)

## supF test
library("strucchange")
fs <- Fstats(tempii ~ trend + harmon1 + harmon2, data = dtempii, from = 0.1)
plot(fs)

sctest(fs)

(bp <- breakpoints(tempii ~ trend + harmon1 + harmon2, data = dtempii, breaks=1, h=0.1))

confint(bp)
## breakpoint
## inspect fitted coefficients in both segments
coef(bp)

seg <- breakfactor(bp)
seglm<-lm(tempii ~ 0+ seg/trend+ seg/harmon1 + seg/harmon2, data = dtempii)
summary(seglm)

## trends
seg <- breakfactor(bp)
plot(dtempii$tempii, col = "black", lwd = 2, ylab="tempii")

lines(ts(coef(bp)[seg, 1] + coef(bp)[seg, 2] * dtempii$trend,
         start = 1920, freq = 12), col = "blue", lwd = 2, lty = 5)

as.matrix(ts(coef(bp)[seg, 1] + coef(bp)[seg, 2] * dtempii$trend,
             start = 1920, freq = 12))

## --> linear trend (with breakpoint)

## seasonality patterns (at observed time points)
seas <-model.matrix(m0)[1:12, 3:6] %*% t(coef(bp)[, 3:6])
rownames(seas) <- apply(matrix(month.abb, 1, 12), 2, paste, collapse = "/")
barplot(t(seas), beside = T, legend = T, col=c("red","blue"))
barplot(t(seas), beside = T, legend = T)

## -> amplitude increases over time

## continuous seasonality patterns
tr <- 0:365/365    
# tr geht von 0 bis 1 in 365 Schritten
h1 <- cbind("cos" = cos(2 * pi * 1 * tr), "sin" = sin(2 * pi * 1 * tr))
h2 <- cbind("cos" = cos(2 * pi * 2 * tr), "sin" = sin(2 * pi * 2 * tr))
s1a <- h1 %*% coef(bp)[1, 3:4]   # harmon 1 period 1
# > coef(bp)[1, 3:4]
# harmon1cos harmon1sin 
# -621.3082  -598.2405 
s2a <- h2 %*% coef(bp)[1, 5:6]   # harmon 2 period 1
s1b <- h1 %*% coef(bp)[2, 3:4]   # harmon 1 period 2
s2b <- h2 %*% coef(bp)[2, 5:6]   # harmon 2 period 2

par(mfrow = c(1, 2))

plot(tr, s1b + s2b, type = "l", lty = 1,
     xlab = "Time (within year)", ylab = "Seasonality",
     main = "Combined")
lines(tr, s1a + s2a, lty = 3)
legend("topleft", colnames(seas), lty = c(3,1), bty = "n")

plot(tr, s1b, type = "l", lty = 1, col = 2,
     xlab = "Time (within year)", ylab = "Seasonality", main = "Separate")
lines(tr, s1a, col = 2, lty = 2)
lines(tr, s2b, col = 4, lty = 1)
lines(tr, s2a, col = 4, lty = 2)
legend("topleft", c(colnames(seas), "annual", "bi-annual"), lty = c(2,1,1,2),
       col = c(1, 1, 2, 4), bty = "n")

cbind(c(s1a + s2a),c(s1b + s2b))  # extract values

## annual amplitude and phase
sqrt(coef(bp)[,3]^2 + coef(bp)[,4]^2)
atan(-coef(bp)[,4]/coef(bp)[,3])

## bi-annual amplitude and phase
sqrt(coef(bp)[,5]^2 + coef(bp)[,6]^2)
atan(-coef(bp)[,6]/coef(bp)[,5])

## --> annual amplitude increases over time

## check residuals from breakpoint model for autocorrelation
par(mfrow = c(1, 2))
acf(residuals(bp))
pacf(residuals(bp))

## --> virtually no autocorrelation left, Box-Ljung test non-significant
# Ljung–Box test for examining the 0-hypothesis of independence in a time series
Box.test(residuals(bp), type = "Ljung", lag = 6)

## even normality seems to be roughly ok
hist(residuals(bp))
qqnorm(residuals(bp))
qqline(residuals(bp))

################ * # generalised additive modelling GAM # * ####################
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/#fn1
# Spline Regression is one of the non-parametric regression technique. 
# In this technique the dataset is divided into bins at intervals or points which we called as knots. 
# Also this bin has its separate fit. 
# https://medium.com/analytics-vidhya/spline-regression-in-r-960ca82aa62c#:~:text=Spline%20Regression%20is%20one%20of,bin%20has%20its%20separate%20fit.
library(mgcv)
library(ggplot2)
library(tidyverse)

# Behind the scenes, the model is trying to balance two competing goals. 
# On the one hand we want to maximize the fit to the data. In linear regression, 
# this goal amounts to minimizing the sum of squared errors. On the other hand, 
# we want to minimize wiggliness (overfitting). In penalized smoothing splines, 
# this is done by first specifying a penalty matrix that defines wiggliness for
# that spline basis. 

# pseudocode mod <- gam(y ~ s(x1) + s(x2), data = foo)

################################################################################
library(mgcv)
setwd("your path for the data")
cet<-read.csv("cet.csv")
head(cet)
## get rid of the annual too - store for plotting
rn <- cet$X
Years <- rn[1]:rn[length(rn)]
annCET <- data.frame(Temperature = cet$Annual,
                     Year = Years)
head(annCET)
ceti <- data.frame(year=Years, cet[, 2:13])
head(ceti)
dim(ceti)

(cet.new<-ceti %>%
    pivot_longer(!year, names_to="month", values_to="temp") %>%
    mutate(Month=rep(1:12,362))  %>% 
    mutate(Date=as.Date(paste(year, Month, 15, sep="-"))) %>%
    mutate(Time= as.numeric(Date) / 1000 ))

head(cet.new)
ggplot(cet.new, aes(x=Date, y=temp)) +
  geom_line()

ggplot(annCET, aes(x=Year, y=Temperature)) +
  geom_line()

# bs = cc, cyclical cubic spline
# k = 12, because of 12 month
m <- gam(temp ~ s(Month, bs = "cc", k = 12) + s(Time),
         data = cet.new)

summary(m)

layout(matrix(1:2, ncol = 2))
plot(m, scale = 0)
layout(1)

layout(matrix(1:2, ncol = 2))
acf(resid(m),  lag.max = 36, main = "ACF")
pacf(resid(m), lag.max = 36, main = "pACF")
layout(1)

# some low-order AR model is needed
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

## Null-Model
m <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time),
          data = cet.new, method="REML")
summary(m$gam)
acf(residuals(m$lme))

## AR(1)
# corARMA(form = ~ 1|year, p = 1) -> | year is a grouping factor, the 
# correlation structure is assumed to apply only to observations within the same group

m1 <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time, k = 20),
           data = cet.new, correlation = corARMA(form = ~ 1|year, p = 1),
           control = ctrl, method="REML")
## AR(2)
m2 <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time, k = 20),
           data = cet.new, correlation = corARMA(form = ~ 1|year, p = 2),
           control = ctrl, method="REML")
## AR(3)
m3 <- gamm(temp ~ s(Month, bs = "cc", k = 12) + s(Time, k = 20),
           data = cet.new, correlation = corARMA(form = ~ 1|year, p = 3),
           control = ctrl, method="REML")

anova(m$lme, m1$lme, m2$lme, m3$lme)

layout(matrix(1:2, ncol = 2))
plot(m2$gam, scale = 0)
layout(1)

layout(matrix(1:2, ncol = 2))
res <- resid(m2$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)

# best source for gam:
# https://fromthebottomoftheheap.net/2015/11/21/climate-change-and-spline-interactions/
# fit a model that allows the seasonal part of the model to change in time along with the trend.
# pseudo R code
# mod <- gam(y = te(x1, x2), data = foo)

############### hierarchical clustering of time-series #########################
library(dtwclust)
setwd("your path for the data")
emission<-read.csv("emission.csv")
# emissions of greenhouse gasses per capita Eurostat (2018)
head(emission)

# dynamic time warping
# normalising of data and performing clustering using DTW distance on a range of 2-6 clusters.
library(vegan)
emissions.norm <- decostand(emission[,-1], method="standardize", MARGIN=1)
?tsclust
(clust.pam <- tsclust(emissions.norm, type="hierarchical", k=2L:6L, distance="dtw"))

# evaluation of clustering efficiency because clustering is a unsupervised procedure
# cluster validity indices (CVIs)
out<-sapply(clust.pam, cvi, type = "internal")
colnames(out)<-c("2 clusters", "3 clusters", "4 clusters", "5 clusters", "6 clusters")
out

clust.pam <- tsclust(emissions.norm, type="hierarchical", k=2L, distance="dtw")
plot(clust.pam, type = "sc")

plot(clust.pam)
# The dashed line represents the representative time series.

t(cbind(emission[,1], cluster = clust.pam@cluster))

# see Obertegger et al. 2017 Water Resources Research
################################################################################
################################################################################
