rm(list = ls())
require(dplyr)
require(simpleboot)
require(repr)
require(knitr)

setwd("C:\\Tejo\\Datascience\\350\\MethodsofDataAnalysis_350\\DataScience350-master\\Lecture5\\Assignment")

#RawDataURL <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data"
#auto.price <- read.csv(RawDataURL, header = FALSE, stringsAsFactors = FALSE)

read.auto = function(file = 'Automobile price data _Raw_.csv'){
        ## Read the csv file
        auto.price <- read.csv(file, header = TRUE, 
                               stringsAsFactors = FALSE)
        
        ## Coerce some character columns to numeric
        numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
        auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
        
        ## Remove cases or rows with missing values. In this case we keep the 
        ## rows which do not have nas. 
        auto.price[complete.cases(auto.price), ]
}
auto.price = read.auto()

#Compare and test Normality the distributions of price and log price
#- Use both a graphical method and a formal test.

read.auto <- auto.price[auto.price$drive.wheels != "4wd",c("fuel.type", 
                                                           "aspiration","drive.wheels" ,"price" ,
                                                           "num.of.doors", "body.style" )]

read.auto <- read.auto[read.auto$num.of.doors != "?", ]
read.auto$log.price <- log(read.auto$price)
read.auto$scaled.log.price <- scale(read.auto$log.price, center = TRUE, scale = TRUE)
read.auto$scaled.price <- scale(read.auto$price, center = TRUE, scale = TRUE)

plot.hist <- function(a, maxs, mins, cols = 'difference of means', nbins = 80, p = 0.05) {
        breaks = seq(maxs, mins, length.out = (nbins + 1))
        hist(a, breaks = breaks, main = paste('Histogram of', cols), xlab = cols)
        abline(v = mean(a), lwd = 4, col = 'red')
        abline(v = 0, lwd = 4, col = 'blue')
        abline(v = quantile(a, probs = p/2), lty = 3, col = 'red', lwd = 3)  
        abline(v = quantile(a, probs = (1 - p/2)), lty = 3, col = 'red', lwd = 3)
}

plot.diff <- function(a, cols = 'difference of means', nbins = 80, p = 0.05){
        maxs = max(a)
        mins = min(a)
        plot.hist(a, maxs, mins, cols = cols[1])
}


options(repr.plot.width=6, repr.plot.height=4)

read.auto.aspiration.std = read.auto[read.auto$aspiration == 'std',]
read.auto.aspiration.turbo = read.auto[read.auto$aspiration == 'turbo',]

two.boot.mean = two.boot(read.auto.aspiration.std$scaled.log.price, read.auto.aspiration.turbo$scaled.log.price, mean, R = 100000)

plot.diff(two.boot.mean$t)

read.auto.fuel.type.gas = read.auto[read.auto$fuel.type == 'gas',]
read.auto.fuel.type.diesel = read.auto[read.auto$fuel.type == 'diesel',]

two.boot.mean = two.boot(read.auto.fuel.type.gas$scaled.log.price, read.auto.fuel.type.diesel$scaled.log.price, mean, R = 100000)

plot.diff(two.boot.mean$t)

body.style <- unique(read.auto$body.style)
body.style.pair.index <- NULL
pairwise.columns <- NULL
final.data <- NULL

for ( i in 1:(length(body.style ) - 1))
{
        pair.1 <- read.auto[read.auto$body.style == body.style[i],]
        for ( j in (i+1):length(body.style))
        {
                pair.2 <- read.auto[read.auto$body.style == body.style[j],]
                body.style.pair.index <- two.boot(pair.1$scaled.log.price, 
                                pair.2$scaled.log.price, mean, R = 100000)        
                pairwise.columns <- rbind(pairwise.columns, 
                                         paste(body.style[i], '-', body.style[j]))
                final.data <- cbind(final.data, body.style.pair.index$t)
        }
        
}

final.data <- data.frame(final.data)
names(final.data) <- pairwise.columns


par(mfrow=c(5,2), mar=c(2,2,2,2))

for (i in 1: ncol(final.data))
{
  a <- names(final.data[i])
  plot.diff(final.data[,a],a)
}
par(mfrow = c(1,1))


kable(cbind(pairwise.columns, 
            c("Reject", "Reject","Reject", "Accept", "Reject", "Reject",
              "Reject","Accept", "Accept", "Accept"), 
            c("Accept", "Accept","Accept","Accept","Accept","Accept", 
              "Reject", "Accept","Accept","Accept")), 
      col.names = c("PairWise Combinations", "Bootstrap Result", 
                    "Anova & Tukey HSD Result" ))

