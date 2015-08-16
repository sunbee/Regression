library(ggplot2)
setwd("c:/Users/ssbhat3/Desktop/Coursera Regression/")
source("EstimateCoefficients_Univariate.R")

# Predict
# A. Set up explanatory variable
newx <- data.frame(x=seq(min(x), max(x), length=100))

# B. Predict response using fitted model
#   p1:   Table with confidence estimates for fitted line
#   p2:   Table with condidence estimates for predicted response
p1 <- data.frame(predict(fit, newdata=newx, interval="confidence"))
p2 <- data.frame(predict(fit, newdata=newx, interval="prediction"))

# C. Condition Data for Display
#   dat:  Data stacked together, names appropriately set
p1$interval <- "confidence"
p1$x <- newx$x
p2$interval <- "prediction"
p2$x <- newx$x
dat <- rbind(p1, p2)
names(dat)[1] <- "y"

# D. Plot Results
g <- ggplot(dat, aes(x=x, y=y))
g <- g + geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2)
g <- g + geom_line()
g <- g + geom_point(data=data.frame(x=x, y=y), aes(x=x, y=y), size=4)
g
