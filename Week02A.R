library(UsingR)
data(diamond)
x <- diamond$carat; y <- diamond$price; n <- length(y)

# Regression Coefficients
slope <- cor(x,y)*sd(y)/sd(x)
intercept <- mean(y)-slope*mean(x)
# Residuals
residuals <- y - (intercept + slope*x)
residualsVar <- sum(residuals^2)/(n-2)
SSx <- sum((x-mean(x))^2)
# Standard Erros
slopeSE <- sqrt(residualsVar/SSx)
interceptSE <- sqrt(((1/n)+(mean(x)^2/SSx))*residualsVar)
# Student t statistic
slopeSt <- slope/slopeSE
interceptSt <- intercept/interceptSE
# p-value 
slopePv <- 2*pt(abs(slopeSt), df=n-2, lower.tail=FALSE)
interceptPv <- 2*pt(abs(interceptSt), df=n-2, lower.tail=FALSE)
# Present Results
coefTable <- rbind(c(intercept, interceptSE, interceptSt, interceptPv), 
                   c(slope, slopeSE, slopeSt, slopePv))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

# Built-in
coefTable
fit <- lm(y~x)
summary(fit)$coefficients

# Confidence Int.
coefficientsTable <- summary(fit)$coefficients
coefficientsTable[1,1] + c(-1,1)*qt(0.975, df=fit$df)*coefficientsTable[1,2]
coefficientsTable[2,1] + c(-1,1)*qt(0.975, df=fit$df)*coefficientsTable[2,2]

# Prediction

newx <- data.frame(x=seq(min(x), max(x), length=100))
yConfidence <- data.frame(predict(fit, newdata=newx, interval="confidence"))
yConfidence$x <- newx$x
yConfidence$interval <- "Confidence"

yPrediction <- data.frame(predict(fit, newdata=newx, interval="prediction"))
yPrediction$x <- newx$x
yPrediction$interval <- "Prediction"

dat <- rbind(yConfidence, yPrediction)
head(dat)
names(dat)[1] <- "y"
 