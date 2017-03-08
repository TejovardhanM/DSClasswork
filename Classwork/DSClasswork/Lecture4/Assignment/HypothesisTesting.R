## Compare and test Normality the distributions of price and log price 
## – Use both a graphical method and a formal test.
rm(list = ls())
require(dplyr)
require(ggplot2)

setwd("C:\\Tejo\\DataScience\\UW_Datascience_Course\\350\\DataScience350-master\\Lecture4\\Assignment")
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

pop_auto.price = rnorm(nrow(read.auto), mean=mean(read.auto$scaled.price), sd = sd(read.auto$scaled.price))
pop_auto.norm = rnorm(nrow(read.auto), mean=0, sd = 1)
pop_auto.log.price = rnorm(nrow(read.auto), mean=mean(read.auto$scaled.log.price), sd = sd(read.auto$scaled.log.price))

par(mfrow = c(1, 2))

plot(sort(pop_auto.price), sort(pop_auto.norm), main = 'Plot of Price vs. normalized', 
     xlab = 'Quantiles of norm1', ylab = 'Qunatiles of norm2')
abline(a = 0.0, b = 1.0, lty = 2, col = 'red')

plot(sort(pop_auto.log.price), sort(pop_auto.norm), main = 'Plot of log price vs. normalized', 
     xlab = 'Quantiles of norm1', ylab = 'Qunatiles of norm2')
abline(a = 0.0, b = 1.0, lty = 2, col = 'red')

par(mfrow = c(1, 1))


#t.test(read.auto$scaled.log.price, pop_auto.norm)


ks.test(read.auto$scaled.log.price, pop_auto.norm)
ks.test(read.auto$scaled.price, pop_auto.norm)
ks.test(read.auto$scaled.price, read.auto$scaled.log.price)


# Have to standardize the x-values
x_seq = seq(-3,3,len=nrow(read.auto))
y_cdf1 = sapply(x_seq, function(x){
        sum(read.auto$scaled.log.price<x)/length(read.auto$scaled.log.price)
})
y_cdf2 = sapply(x_seq, function(x){
        sum(pop_auto.norm<x)/length(pop_auto.norm)
})

## Find the max deviation
k_s_stat = max(abs(y_cdf1 - y_cdf2))
k_s_stat
# where does it occur?
k_index = which.max(abs(y_cdf1-y_cdf2))
k_s_x = x_seq[k_index]
plot(x_seq,y_cdf1, col='blue', pch=16, main ='CDFs of standardized samples', 
     xlab = 'Value', ylab = 'Cumulative density')
points(x_seq,y_cdf2,col='red', pch=16) 
lines(c(k_s_x,k_s_x), c(y_cdf1[k_index],y_cdf2[k_index]),
      col='black', lwd=8)


##################################

#lapply( read.auto, function (x) table(x))


plot.t <- function(a, b, cols = c('pop_A', 'pop_B'), nbins = 20){
        maxs = max(c(max(a), max(b)))
        mins = min(c(min(a), min(b)))
        breaks = seq(maxs, mins, length.out = (nbins + 1))
        par(mfrow = c(2, 1))
        hist(a, breaks = breaks, main = paste('Histogram of', cols[1]), xlab = cols[1])
        abline(v = mean(a), lwd = 4, col = 'red')
        hist(b, breaks = breaks, main = paste('Histogram of', cols[2]), xlab = cols[2])
        abline(v = mean(b), lwd = 4, col = 'red')
        par(mfrow = c(1, 1))
}

n = 10


auto.fuel.type = read.auto %>% group_by(fuel.type ) %>% 
        sample_n(n, replace = FALSE)

auto.fuel.type.bydiesel <-  auto.fuel.type[auto.fuel.type$fuel.type=="diesel",]
auto.fuel.type.bygas <-  auto.fuel.type[auto.fuel.type$fuel.type=="gas",]


pop_A = auto.fuel.type.bydiesel$scaled.log.price
pop_B = auto.fuel.type.bygas$scaled.log.price

plot.t(pop_A, pop_B, cols = c("Log price by Diesel", "Log price by Gas"))


ks.test(pop_A, pop_B, alternative = "two.sided")
#t.test(pop_A, pop_B, alternative = "two.sided")
#df_gp_1 <- auto.fuel.type %>% group_by(fuel.type) %>% summarise(n.group = n(), mean.index = mean(scaled.log.price))



auto.aspiration.type = read.auto %>% group_by(aspiration) %>% 
        sample_n(n, replace = FALSE)

auto.aspiration.type.std <-  auto.aspiration.type[auto.aspiration.type$aspiration=="std",]
auto.aspiration.type.turbo <-  auto.aspiration.type[auto.aspiration.type$aspiration=="turbo",]


pop_A = auto.aspiration.type.std$scaled.log.price
pop_B = auto.aspiration.type.turbo$scaled.log.price

plot.t(pop_A, pop_B)


ks.test(pop_A, pop_B, alternative = "two.sided")
#windows() ## create window to plot your file
## ... your plotting code here ...
#dev.off()

########################################


auto.drive.wheels = read.auto %>% group_by(drive.wheels) %>% 
        sample_n(n, replace = FALSE)

auto.drive.wheels.rwd <-  auto.drive.wheels[auto.drive.wheels$drive.wheels=="rwd",]
auto.drive.wheels.fwd <-  auto.drive.wheels[auto.drive.wheels$drive.wheels=="fwd",]


pop_A = auto.drive.wheels.rwd$scaled.log.price
pop_B = auto.drive.wheels.fwd$scaled.log.price
#plot.t(pop_A, pop_B)

ks.test(pop_A, pop_B, alternative = "two.sided")
#windows() ## create window to plot your file
plot.t(pop_A, pop_B)
#dev.off()

ks.test(pop_A, pop_B, alternative = "two.sided")
#####################

auto.anova.sample = read.auto %>% group_by(num.of.doors, body.style) %>% 
        sample_n(n, replace = TRUE)

boxplot(auto.anova.sample$scaled.log.price ~ auto.anova.sample$num.of.doors+
                auto.anova.sample$body.style)


df_aov = aov(scaled.log.price ~ num.of.doors + body.style, data = auto.anova.sample)
summary(df_aov)
print(df_aov)


tukey_anova = TukeyHSD(df_aov)  # Tukey's Range test:
tukey_anova


plot(tukey_anova)

#ajuste <- lm(auto.anova.sample$scaled.log.price ~ auto.anova.sample$num.of.doors+
#                     auto.anova.sample$body.style)
#summary(ajuste)
#anova(ajuste)

#install.packages("agricolae")
#library(agricolae)
#HSD.test(ajuste, 'auto.anova.sample$scaled.log.price')
