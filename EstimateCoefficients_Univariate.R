library(UsingR)
data(diamond)
setwd("c:/Users/ssbhat3/Desktop/Coursera Regression/")
# Data: Response variable (price) and explanatory variable (carats)
y <- diamond$price
x <- diamond$carat
n <- length(x)
plot(x, y, pch=20)

# Regression coefficients by Ordinary Linear Regression
# Estimates
#   beta0:  intercept
#   beta1:  slope
#   e:      residuals
beta1 = cor(x,y)*sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
e <- y - beta0 - beta1*x
plot(e, type="h")

# Confidence Intervals
# A. Necessary terms
#   sigma:  sum of squared residuals, square-root of 
#           also Root Mean Squared Error (RMSE)
#   ssx:    sum of squared differences from mean for explanatory variable
sigma <- sqrt(sum(e^2)/(n-2))
ssx <- sum((x-mean(x))^2)
# B. Std. Errors
#   seBeta0: s.d. beta0
#   seBeta1: s.d. beta1
seBeta0 <- sigma*sqrt((1/n) + mean(x)^2/ssx)
seBeta1 <- sigma/sqrt(ssx)
# C. Hypothesis test
#   t statistic for two-tailed test with df = n-2
#     tBeta0, tBeta1
tBeta0 <- beta0/seBeta0
tBeta1 <- beta1/seBeta1
#   p-value 
pBeta0 = 2*pt(abs(tBeta0), df=n-2, lower.tail = FALSE)
pBeta1 = 2*pt(abs(tBeta1), df=n-2, lower.tail = FALSE)

# Result Table
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0),
                   c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable

# Compare with fitted model 
fit <- lm(y ~ x)
coefSummary <- summary(fit)$coefficients

# Confidence Intervals
coefSummary[1,1]+c(-1, 1)*qt(0.975, df=fit$df)*coefSummary[1,2]
coefSummary[2,1]+c(-1, 1)*qt(0.975, df=fit$df)*coefSummary[2,2]
