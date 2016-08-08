##--------------------------------------------
##
## Class: PCE 350 Data Science Methods Class
##
##---- Logistic Regression Simulation ----
##

sim.logt.data <- function(x1, y1, n1, sd1, x2, y2, n2, sd2){
  wx1 = rnorm(n1, x1, sd1)
  wy1 = rnorm(n1, y1, sd1)
  z1 = rep(TRUE, n1)
  wx2 = rnorm(n2, x2, sd2)
  wy2 = rnorm(n2, y2, sd2)
  z2 = rep(FALSE, n2)
  
  df1 = data.frame(x = wx1, y = wy1, z = z1)
  df2 = data.frame(x = wx2, y = wy2, z = z2) 
  rbind(df1, df2)
}

plot.class <- function(df){
  require(ggplot2)
  df$z <- as.factor(df$z)
  ggplot(df, aes(x, y)) + 
    geom_point(aes(color = z, size = 3)) +
    ggtitle('X vs. Y for two classes')
}


logistic.mod <- function(df){
  glm(z ~ 0 + x + y, data = df, family = binomial)
}


logistic.pred <- function(mod, df, prob = 0.5){
  df$score = ifelse(predict(mod, type = 'response') > prob, TRUE, FALSE)
  df
}


logistic.eval <- function(mod, df, prob = 0.5){
  require(ggplot2)
  df <- logistic.pred(mod, df, prob = prob)
  df$conf <- ifelse(df$z == TRUE & df$score == TRUE, 'TP',
                    ifelse(df$z == FALSE & df$score == TRUE, 'FP',
                           ifelse(df$z == FALSE & df$score == FALSE, 'TN', 'FN')))
  
  df$error <- ifelse((df$conf == 'FP' | df$conf == 'FN'), 'error', 'correct')

  cols <- c('z', 'error')
  df[, cols] <- lapply(df[, cols], as.factor)

  p1 <- ggplot(df, aes(x, y)) + 
    geom_point(aes(color = z, size = 3, shape = error)) +
    ggtitle('X vs. Y for two classes')
  print(p1)
  
  TP = length(df[df$conf == 'TP', 'conf'])
  FP = length(df[df$conf == 'FP', 'conf'])
  TN = length(df[df$conf == 'TN', 'conf'])
  FN = length(df[df$conf == 'FN', 'conf'])
  
  print(paste('accuracy =', as.character((TP + TN)/(TP + TN + FP + FN))))      
  print(paste('precision =', as.character(signif(TP/(TP + FP)), digits = 2)))     
  print(paste('recall =', as.character(TP/(TP + FN))))
  
  out <- data.frame(Positive = c(TP, FP), Negative = c(FN, TN))
  row.names(out) <- c('TruePos', 'TrueNeg')
  print(out)
}


run.demo <- function(){
  x1 = c(1, 0.5, 0.1)
  y1 = c(1, 0.5, 0.1)
  x2 = c(-1, -0.5, -0.1)
  y2 = c(-1, -0.5, -0.1)
  for(i in 1:3){
    logt <- sim.logt.data(x1[i], y1[i], 50, 1, x2[i], y2[i], 50, 1)
    logMod <- logistic.mod(logt)
    logistic.eval(logMod, logt)
  }
}


run.demo.prob <- function(){
  logt <- sim.logt.data(0.5, 0.5, 50, 1, -0.5, -0.5, 50, 1)
  probs = c(0.5, 0.25, 0.125)
  for(p in probs){    
    logMod <- logistic.mod(logt)
    logistic.eval(logMod, logt, p)
  }
}


