setwd("C:\\Tejo\\Datascience\\350\\Classwork\\Lecture8\\Assignment")


read.CADairy = function(file = "C:\\Tejo\\Datascience\\350\\Classwork\\DSClasswork\\Lecture8\\Assignment\\CADairyProduction.CSV"){
    ## Read the csv file
    dairy.produce <- read.csv(file, header = TRUE, 
                              stringsAsFactors = FALSE)
    
  }
  
ca.dairy = read.CADairy()

##Icecream.Prod 
require(forecast)
require(repr)

str(ca.dairy)



## Create a ts class object from the vector
## by adding time series attributes
#ts(dairy.data[,"Icecream.Prod"], start = c(1995, 1), frequency = 12)
vec.ts = with(ca.dairy, log(ts(Icecream.Prod, start = c(1995, 1), freq = 12)))
attributes(vec.ts) # Note the time series attributes

options(repr.pmales.extlot.width=8, repr.plot.height=4)
plot(vec.ts) # Note the x-axis is the time attribute



options(repr.pmales.extlot.width=8, repr.plot.height=6)
plot.acf <- function(df, col = 'remainder', is.df =TRUE){
  if(is.df) temp <- df[, col]
  else temp <- df
  par(mfrow = c(2,1))
  acf(temp, main = paste('ACF of', col))
  pacf(temp, main = paste('PACF of', col))
  par(mfrow = c(1,1))
}

plot.acf(vec.ts, col = 'time series - Is stationary', is.df = F)

##PLot ACF and PACF.

#pacf(vec.ts,is.df=F)
#acf(vec.ts,is.df=F)
##- Is this time series stationary?
Box.test(vec.ts, lag =20, type="Ljung-Box")

##P.value is <0.05, hence the time series dataset is stationary.

#- Is there a significant seasonal component?

plot(vec.ts, main = 'seasonal time series')
monthplot(vec.ts)

####3
## Decomposition of the time series into components
ts.decomp <- function(df, col = 'dairy.ts', span = 0.25, Mult = TRUE, is.df = TRUE){
  # if(Mult) temp = log(df[, col])  else temp = ts(df[, col]
  if(is.df) temp = log(df[, col])  
  else temp = df
  spans = span * length(temp)  
  fit <- stl(temp, s.window = "periodic", t.window = spans)
  plot(fit, main = paste('Decompositon of',col,'with lowess span = ', as.character(span)))
  fit$time.series
}

temp = ts.decomp(vec.ts, is.df = FALSE, Mult = FALSE)
plot.acf(temp[,3],is.df=FALSE)

plot.acf(temp[,3],is.df=FALSE)


iceCream.arima = ts.model(temp[, 3], col = 'ARIMA model for icecream production', order = c(2,0,2))##
plot.acf(iceCream.arima$resid[-1],is.df=FALSE)


final.aic <- Inf
x = temp[,3]
final.order <- c(0,0,0)
for (i in 0:2) for (j in 0:2) {
  current.aic <- AIC(arima(x, order=c(i, 0, j)))
  if (current.aic < final.aic) {
    final.aic <- current.aic
    final.order <- c(i, 0, j)
    final.arma <- arima(x, order=final.order, include.mean = FALSE)
  }
}

final.aic
final.arma
final.order

#4,0,4
#plot.acf(final.arma$residuals, col = 'time series - Is stationary', is.df = F)
acf(resid(final.arma))

#######################

## Function for ARIMA model estimation
ts.model = function(ts, col = 'remainder', order = c(0,0,1)){
  mod = arima(ts, order = order, include.mean = FALSE)
 print(mod)
  mod
}


#mod.est = ts.model(temp[,3], col = 'AR(1) process', order = c(1,0,0))
#plot.acf(mod.est$resid[-1], col = 'AR(1) estimate', is.df = F)


## Compute ARIMA model of electric residual
#Icecream.arima = ts.model(temp[,3], col = 'ARIMA model for CA Dairy Icecream Prod', order = c(2,1,2))
#plot.acf(Icecream.arima$resid[-1], col = 'AR(1) estimate', is.df = F)


fit.icecream = auto.arima(vec.ts, max.p=3, max.q=3,
                       max.P=2, max.Q=2, max.order=5, max.d=2, max.D=1,
                       start.p=0, start.q=0, start.P=0, start.Q=0)
summary(fit.icecream)


icecream.forecast = forecast(fit.icecream, h=12)
summary(icecream.forecast)
plot(icecream.forecast)

#######################
