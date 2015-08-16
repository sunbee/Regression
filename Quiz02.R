# Q1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit <- lm(y ~ x)
co <- summary(fit)$coefficients

co[2,4] # 0.0529

# Q2

e <- y - predict(fit)
e <- y - co[1,1] - co[2,1]*x
n = length(x)
v <- sum(e^2)/(n-2)
sqrt(v) # 0.223

# Q3
data(mtcars)
x <- mtcars$wt
y <- mtcars$mpg

Mod <- lm(y ~ x)
Out <- predict(Mod, newdata=data.frame(x=mean(x)), interval="confidence" )

Out[2] # 18.991

# Q4
# The estimated change in mpg per 1,000 lbs in wt

# Q5
New <- predict(Mod, newdata=data.frame(x=3), interval="prediction" )
New # 27.573

# Q6
cc <- summary(Mod)$coefficients
(cc[2,1] + c(-1,1)*qt(0.975, df=Mod$df)*cc[2,2])*2 # -12.973

# Q7 * CORRECTED
# Coeff in cm, gets MULTIPLIED by 100 for use with meters 
# Term: slope * predictor must remain unchanged with change in units

# Q8
# new intercept is old intercept minus slope*constant
# Add two terms that sum to zero to the original regression equation (slope*constant)
# Negative term modifies the intercept, Positive term modifies the predictor 

# Q9 *
# Compare to r-square
# Regression to intercept (no slope) is tantamount to regression to mean
whyHat <- predict(Mod, newdata=data.frame(x=x))
sum((y-whyHat)^2)/sum((y-mean(y))^2) # 0.25




# Q10
# Residuals sum to zero for regression with intercept and slope
