#Q1:
library(MASS)
?shuttle
head(shuttle)
use <- shuttle$use
wind <- shuttle$wind
use <- relevel(use, "noauto")
LogMod <- glm(use ~ wind, family=binomial)
betas <- coefficients(LogMod)
pHead <- exp(betas[1])/(1+exp(betas[1]))
pTail <- exp(betas[1]+betas[2])/(1+exp(betas[1]+betas[2]))
oHead <- pHead/(1-pHead)
oTail <- pTail/(1-pTail)
oddsRatio <- oHead/oTail
oddsRatio
exp(betas["windtail"])
1/exp(betas["windtail"])
# 0.969

#Q2:
# (cont'd from Q1)
magn <- shuttle$magn
LogModAdjusted <- glm(use ~ wind + magn, family=binomial)
betasAdjusted <- coefficients(LogModAdjusted)
exp(betasAdjusted["windtail"])
1/exp(betasAdjusted["windtail"]) # 0.969

#Q3:
# (cont'd from Q1)
LogModMinus <- glm(use ~ wind, family=binomial, data=shuttle)
betasMinus <- coefficients(LogModMinus)
# Ans: The coefficients reverse their signs. (Odds have flipped.)

#Q4: 
data(InsectSprays)
SprayMod <- glm(count ~ spray, family=poisson, data=InsectSprays)
betasSpray <- coefficients(SprayMod)
1/exp(betasSpray["sprayB"])

#Q5
# The offset is added when modeling rates. This is appropriate when modeling counts
# and the window of time for mesaurement is not fixed. For example, 6 cases in a day
# should not amount to 6 cases in a week.
# log(MUx)    <- beta0 + beta1*x
# log(MUx/t)  <- beta0 + beta1*x            # Standardize the window for rate
# log(MUx)    <- log(t) + beta0 + beta1*x
# Now:log(t) represents the offset
# Further: log(7*t) = log(10)+ log(t)       # Say we want a smaller window, one-tenth
# log(MUx)    <- log(10t) + beta0 + beta1*x - log(10)   # Keeping counts fixed
# log(MUx/10t) <- (beta0-log(10)) + beta1*x 

# Ans: The coefficient is subtracted by log(10) ERR

#Q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
LinModel <- lm(y ~ x) 
plot(x, y, type="p", pch=19)
abline(LinModel)
knot <- 0;
splineX <- (x > knot) * (x - knot)
designMatrix <-cbind(1, x, splineX)
SplineModel <- lm(y ~ designMatrix-1)
plot(x, y, pch=19, col="blue")
lines(x, predict(SplineModel), lwd=4)
SplineBetas <- coefficients(SplineModel)
SplineBetas[2]+SplineBetas[3]

# Example: Spline Regression
n <- 500; x <- seq(0, 4*pi, length=n);y <- sin(x) + rnorm(n, sd=0.3);
knots <- seq(0, 4*pi, length=10)
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
designMatrix <- cbind(1, x, splineTerms)
yHat <- predict(lm(y ~ designMatrix-1))
plot(x, y, pch=19)
lines(x, yHat, pch=19, col="blue", lwd=4)

