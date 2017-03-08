setwd("C:\\Tejo\\Datascience\\350\\Classwork\\Lecture7\\Assignment")

#RawDataURL <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data"
#auto.price <- read.csv(RawDataURL, header = FALSE, stringsAsFactors = FALSE)

require(MASS)
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

auto.price = auto.price[, names(auto.price) != c("symboling", "normalized.losses")]

#Compare and test Normality the distributions of price and log price
#- Use both a graphical method and a formal test.

read.auto <- auto.price[auto.price$drive.wheels != "4wd",c("fuel.type", 
                                                           "aspiration","drive.wheels" ,"price" ,
                                                           "num.of.doors", "body.style" )]

read.auto <- read.auto[read.auto$num.of.doors != "?", ]
read.auto$log.price <- log(read.auto$price)
read.auto$scaled.log.price <- scale(read.auto$log.price, center = TRUE, scale = TRUE)
#read.auto$scaled.price <- scale(read.auto$price, center = TRUE, scale = TRUE)



lm.auto = lm(scaled.log.price ~ ., data = read.auto)
summary(lm.auto)
plot(lm.auto)

lm.step = stepAIC(lm.auto, direction = 'both')
lm.step$anova # ANOVA of the result 
summary(lm.step) # Summary of the best model
plot(lm.step)


