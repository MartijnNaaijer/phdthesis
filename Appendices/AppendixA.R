#Code for simulation with samples of size 100 and 95% confidence interval

simci = function(n){
  sample10 = rnorm(n)
  mean10 = mean(sample10)
  se = 1/sqrt(n)
  #this code presupposes that the standard deviation of the population is known.
  ci = c(mean10 -qnorm(0.975)*se, mean10 + qnorm(0.975)*se)
  return(ci)
}
sim100 = t(replicate(100,simci(1000)))
plot(x = sim100[, 1], y = 1:100, 
     xlim = c(-0.5, 0.5), type="n", xlab = "", ylab = "Sample", main = 
       "95% confidence intervals for estimates of the mean of standard normal distribution (sample size = 100)")
segments(y0=1:100, x0=sim100[, 1], y1 = 1:100, x1=sim100[, 2], col = ifelse(sim100[, 1]>0 | sim100[, 2]<0, "red", "black"))
abline(v=0)

#################################################################################

# simulate data and fit with linear model 
x <- 1:30
set.seed(2)
y <- 2 * x + rnorm(30, 0, 5)
plot(x, y)

# make model
fit <- lm(y~x)

# draw the model
abline(fit)
pred.val <- predict(fit)
segments(x, y, x, pred.val, col = 'blue')
summary(fit)

###############################################################################

# simulate data for logistic regression

fail <- rnorm(20,0,5)+20 
success <- rnorm(20,0,5)+30 
hours <- c(fail, success)
result <- c(rep(0,20), rep(1,20))
df <- data.frame(hours, result)
plot(df$hours, df$result, xlab='Hours of study', ylab='Success', main='Passing an exam or not?')

#plot linear model
par(mfrow=c(1, 2))
plot(df$hours, df$result, xlab='Hours of study', ylab='Probability of success', main='Passing an exam or not with linear model')
lin.model <- lm(result~hours, data=df)
abline(lin.model)

library(popbio)
logi.hist.plot(hours, result, boxp=FALSE, type="hist", col="gray", xlab='Hours of study', main='Passing an exam or not with logistic curve')
abline(h=0.5)
par(mfrow=c(1, 1))

##################################################################

# simulate data for a gam model

library(mgcv)

x <- seq(0, 10, 0.1)
y <- 1/25*x^3 + 1/10*x^2 - 4*x + rnorm(length(x),0,2)
plot(x, y, xlab='X', ylab='Y', main='Simulated data with non-linear structure')

#################################################################

#linear model and GAM

par(mfrow=c(1,2))
plot(x, y, pch = '.', main = 'Linear model')
lin.m <- lm(y~x)
abline(lin.m)

gam.m <- gam(y~s(x)) 
plot(gam.m, rug = F, residuals = T, shift = gam.m$coefficients['(Intercept)'], xlab='X', ylab='Y', main='Generalized Additive Model')

par(mfrow=c(1,1))

