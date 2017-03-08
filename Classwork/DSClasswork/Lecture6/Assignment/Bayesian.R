require(LearnBayes)
## I think the chance of rain is 0.2 with
## with a probability at the 75% point of 0.28
## Compute my Beta prior
beta.par <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
beta.par ## The parameters of my Beta distribution

par(mfrow = c(3,1))
beta.par + c(2, 18)
triplot(beta.par, c(2, 18))

beta.par + c(4, 16)
triplot(beta.par, c(4, 16))

beta.par + c(1, 19)
triplot(beta.par, c(1, 19))
par(mfrow = c(1,1))

# Simulate from the posterior and 
## compute confidence intervals
options(repr.plot.width=8, repr.plot.height=5)
beta.post.par <- beta.par + c(7, 53)
post.sample <- rbeta(100, beta.post.par[1], beta.post.par[2])
par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 41)
hist(post.sample, breaks = breaks, 
     main = 'Distribution of samples \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
qqnorm(post.sample)
par(mfrow = c(1,1))
quants


predplot(beta.post.par, 7, 53)



n <- 100
s <- 0:n
pred.probs <- pbetap(beta.post.par, n, s)
pred.probs.beta <- pbetap(beta.par, n, s)

plot(s, pred.probs, type="h", 
     main = paste('Probability distribution of successes in', as.character(n), 'trials'),
     xlab = 'Successes')


#discint(cbind(s, pbetap(beta.par, n,s)) , 0.90)
discint(cbind(s, pred.probs.beta) , 0.90)
discint(cbind(s, pred.probs), 0.90)


par(mfrow = c(2,1))


plot(s, pred.probs.beta, type="h", 
     main = paste('Probability distribution of Prior beleifs in', as.character(n), 'trials'),
     xlab = 'Successes')

plot(s, pred.probs, type="h", 
     main = paste('Probability distribution of posterier beleifs in', as.character(n), 'trials'),
     xlab = 'Successes')

par(mfrow = c(1,1))




