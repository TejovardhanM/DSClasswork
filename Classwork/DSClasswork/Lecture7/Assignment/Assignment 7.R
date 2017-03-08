rm(list=ls())
require(dplyr)
require(HistData)
#library(dplyr)
#setwd("C:\\Tejo\\DataScience\\UW_Datascience_Course\\350\\DataScience350-master\\Lecture7")
read.auto = function(file = "C:\\Tejo\\DataScience\\UW_Datascience_Course\\350\\DataScience350-master\\Lecture7\\Automobile price data _Raw_.csv"){
  ## Read the csv file
  auto.price <- read.csv(file, header = TRUE, 
                         stringsAsFactors = FALSE)
  ## Coerce some character columns to numeric

  numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm','wheel.base','length','width','height','curb.weight','engine.size','compression.ratio','city.mpg','highway.mpg')
  auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  auto.price$log.price=log(auto.price$price)
  auto.price[, numcols] <- lapply(auto.price[, numcols], scale)
  
  
  ## remove symbolizing and normalized.losses
  auto.price <- auto.price %>% dplyr::select(-symboling,-normalized.losses,-price)
  #auto.price = auto.price[, names(auto.price) != c("symboling", "normalized.losses", "price")]  
  #auto.price <- auto.price %>% dplyr::select(log.price, bore, stroke, horsepower, peak.rpm)
  
  #auto.price <- auto.price %>% dplyr::select(log.price)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  auto.price[complete.cases(auto.price), ]
  
}
auto.price = read.auto()
head(auto.price)

#require(MASS)
library(MASS)
lm.auto.log.price= lm(log.price ~ . , data = auto.price)
summary(lm.auto.log.price)
plot(lm.auto.log.price)

lm.auto.log.price.step =stepAIC(lm.auto.log.price, direction = "both")
summary(lm.auto.log.price.step)
lm.auto.log.price.step$anova # ANOVA of the result 


###############################################################################################################################


ModelMatrix= model.matrix(log.price ~ . -1, data = auto.price)
M=as.matrix(ModelMatrix)
str(auto.price)
str(ModelMatrix)
MTM = t(M) %*% M
dim(MTM)

mSVD = svd(MTM)

mSVD$d

d.trim = rep(0,60)
d.trim[1:57]=1/mSVD$d[1:57]
mD=diag(d.trim)
mD

mInv = mSVD$v %*% mD %*% t(mSVD$u)
mInv

MTMTM = mInv %*% t(M)
dim(MTMTM)

b= MTMTM %*% auto.price$log.price

auto.price$score = M%*% b + mean(auto.price$log.price)
auto.price$resids = auto.price$score-auto.price$log.price

plot.svd.reg <- function(df, k = 4){
  require(ggplot2)
  require(gridExtra)
  
  p1 <- ggplot(df) + 
    geom_point(aes(score, resids), size = 2) + 
    stat_smooth(aes(score, resids)) +
    ggtitle('Residuals vs. fitted values')
  
  p2 <- ggplot(df, aes(resids)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density(color = 'red', fill = 'red', alpha = 0.2) +
    ggtitle('Histogram of residuals')
  
  qqnorm(df$resids)
  
  grid.arrange(p1, p2, ncol = 2)
  
  df$std.resids = sqrt((df$resids - mean(df$resids))^2)  
  
  p3 = ggplot(df) + 
    geom_point(aes(score, std.resids), size = 2) + 
    stat_smooth(aes(score, std.resids)) +
    ggtitle('Standardized residuals vs. fitted values')
  print(p3) 
  
  n = nrow(df)
  Ybar = mean(df$log.price)
  SST <- sum((df$log.price - Ybar)^2)
  SSR <- sum(df$resids * df$resids)
  SSE = SST - SSR
  cat(paste('SSE =', as.character(SSE), '\n'))
  cat(paste('SSR =', as.character(SSR), '\n'))
  cat(paste('SST =', as.character(SSE + SSR), '\n'))
  cat(paste('RMSE =', as.character(SSE/(n - 2)), '\n'))
  
  adjR2  <- 1.0 - (SSR/SST) * ((n - 1)/(n - k - 1))
  cat(paste('Adjusted R^2 =', as.character(adjR2)), '\n')
}

plot.svd.reg(auto.price)

##################################################

install.packages("glmnet")
require(glmnet)
b = as.matrix(auto.price$log.price)
mod.ridge = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.5)
plot(mod.ridge, xvar = 'lambda', label = TRUE)
plot(mod.ridge, xvar = 'dev', label = TRUE)


mod.ridge.lasso = glmnet(M, b, family = 'gaussian', nlambda = 20, alpha = 0.5)
plot(mod.ridge.lasso, xvar = 'lambda', label = TRUE)
plot(mod.ridge.lasso, xvar = 'dev', label = TRUE)

auto.price$score = predict(mod.ridge.lasso, newx = M)[, 15]
auto.price$resids = auto.price$score - auto.price$log.price

plot.svd.reg(auto.price)


