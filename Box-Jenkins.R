library(tseries)
library(forecast)
library(lawstat)
library(stats)

data <- read.csv("/Users/libingyi/Documents/MSAN/MSAN604/project/train.csv")
train <- data[1:264,]
valid <- data[265:288, ]
test <- read.csv("/Users/libingyi/Documents/MSAN/MSAN604/project/test.csv")
x <- ts(train[c(2,3,5)], start = c(1987, 1), end = c(2008, 12), frequency=12)
y <- ts(train[4], start = c(1987, 1), end = c(2008, 12), frequency=12)

valid_x <- ts(valid[c(2,3,5)], start = c(2009, 1), end = c(2010, 12), frequency=12)
valid_y <- ts(valid[4], start = c(2009, 1), end = c(2010, 12), frequency=12)


plot(x)
plot(y)
View(x)
View(y)

# remove heteroscedasticity
y.new <- log(y)
plot(y.new)
acf(y.new)
ndiffs(x = y.new, test = "adf", max.d = 2)

# remove trend
AP1 <- diff(y.new)
plot(AP1)
acf(AP1, lag.max = 264)
#adf.test(AP1,k=20)
ndiffs(x = AP1, test = "adf", max.d = 2)

# remove seasonality
AP1.60 <- diff(AP1, lag = 60)
plot(AP1.60)
acf(AP1.60, lag.max = 264)
#adf.test(AP1.60, k = 20)

AP1.60.2 <- diff(AP1.60, lag = 60)
plot(AP1.60.2)
acf(AP1.60.2, lag.max = 264)
pacf(AP1.60.2, lag.max = 120)
#adf.test(AP1.60.2, k=20)

# Choose d=1, D=2, m=60. Check ACF and PACF to choose p, q, P, Q.
acf(AP1.60.2, lag.max = 264)
pacf(AP1.60.2, lag.max = 264)

# Try p = 2, q = 1, P = Q = 1 SARIMA(2, 1, 1)(1, 2, 1)_{60}
m2.ml <- arima(log(y), order = c(2,1,1), seasonal = list(order = c(1,2,1), period = 60), method = "CSS-ML")
m2.ml

# Check auto.arima  SARIMA(2, 1, 1)(1, 0, 0)_{12}
auto.arima(log(y), allowdrift = F)
auto.m <- arima(log(y), order = c(2,1,1), seasonal = list(order = c(1,0,0), period = 12), method = "CSS-ML")
auto.m
auto.arima(log(y), allowdrift = F, stepwise = F, approximation = F)
auto.m2 <- arima(log(y), order = c(4,1,0), seasonal = list(order = c(1,0,0), period = 12), method = "CSS-ML")
auto.m2

#SARIMA(2, 1, 1)(1, 2, 1)_{60}: Prediciton RMSE = 0.006052131
pred <- forecast(m2.ml, h=24)
sqrt(mean((valid_y - exp(pred$mean))^2))

#SARIMA(2, 1, 1)(1, 0, 0)_{12}:  Prediciton RMSE = 0.006332476
pred2 <- forecast(auto.m2, h=24)
sqrt(mean((valid_y - exp(pred2$mean))^2))


##########################################################################################################################################
##########################################################################################################################################





# Model Comparison
sigma2<-c(m2.ml$sigma2, auto.m$sigma2, auto.m2$sigma2)
AIC<-c(m2.ml$aic, auto.m$aic, auto.m2$aic)
d <- data.frame(orders = c("(2,1,1)(1,2,1)[60] ","(2,1,1)(1,0,0)[12]", "(2,1,1)(2,0,0)[12]"),sigma2,AIC)
# Order this by AIC
d[order(d$AIC),]
# Order this by sigma2
d[order(d$sigma2),]
# auto.m2 model is "optimal"

# likelihood ratio tests based on maximized log-likelihood function
D <- -2*(m2.ml$loglik - auto.m2$loglik)
pval <- 1-pchisq(D,0)
print(c("Test Statistic:",round(D,4),"P-value:",round(pval,4)))
# auto.m2 model is "optimal"



# Plot the times series and fitted values for m2.ml model
plot(log(y))
fit <- log(y) - auto.m2$residuals
lines(fit, col = "red")
legend("bottomright", legend = c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex = 0.5)
# Recreate plots on the raw scale
plot(y)
lines(exp(fit), col = "red")
legend("bottomright", legend = c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex = 0.5)

# Residual Diagnostics for auto.m2 model
e <- auto.m2$residuals
r <- e/sqrt(auto.m2$sigma2)
# Zero-Mean
t.test(e)
# Since p-value = 0.6693, which is way bigger than the 0.01 significent level, we fail to reject H0 that the residuals have zero mean. 
# Thus, Zero-Mean assumption is satisfied.
# Homoscedasticity
# Informally: Plot residuals and standardized residuals
par(mfrow=c(2,1))
plot(e, main="Residuals vs t", ylab="")
abline(h=0, col="red")
plot(r, main="Standardized Residuals vs t", ylab="")
abline(h=0, col="red")
# From the plots above, we can see that there is no funny pattern,
# suggesting that the error variance is constant.
# Formally: Levene Test
par(mfrow=c(1,1))
plot(e, main="Residuals vs t", ylab="")
abline(v=c(1992,1997,2002,2007), lwd=3, col="red")
group <- c(rep(1,63),rep(2,57),rep(3,57),rep(4,57),rep(5,54))
levene.test(e,group)
bartlett.test(e,group) 
# Since p-value = 0.5363 > 0.01 for the Levene Test and p-value = 0.53 > 0.01 for the Bartlett Test
# we fail to reject H0 that the resisuals have constant variance. 
# Thus, Homoscedasticity assumption is satisfied.
# Zero-Correlation
# Informally: ACF plot of residuals
tsdiag(auto.m2)
# From the ACF plots above, we can see that there is several significant autocorrelation for residuals when lag > 0, 
# suggesting that the errors are not independent. Also, the p-values for Ljung-Box statistics are not all above the significent level line, suggesting that we might need to reject H0 that the residuals are independent. 
# We will formally test the model satisfaction of the assumption.
# Formally: Ljung-Box Test
Box.test(e,type = "Ljung-Box")
# Since p-value = 0.9897, which is way bigger than the 0.01 significent level, 
# we fail to reject H0 that the residuals have zero correlation, based on 1 autocorrelation coefficients (lag = 1 by default). 
# Thus, Zero-Correlation assumption is satisfied.
# Normality
# Informally: QQ-plot of residuals
par(mfrow=c(1,1))
qqnorm(e, main="QQ-plot of Residuals")
qqline(e, col = "red")
# From the plots above, we can see that the residual quantiles match fairly closely with the theoretical normal quantiles, 
# suggesting that the normality assumption is reasonable.
# Formally: Shapiro-Wilk Test
shapiro.test(e)
# Since p-value = 0.7353, which is bigger than the 0.01 significent level, we fail to reject H0 that the residuals are nornally distributed. 
# Thus, Normality assumption is satisfied.




