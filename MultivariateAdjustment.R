# Set up data for simulation
#   Size: n=100 observations
#   Explanatory Variables: x, x2, x3
#   Response Variable: y
n=100
x=rnorm(n)
x2=rnorm(n)
x3=rnorm(n)

# Set up the "true" model, adding noise
y = 1 + x + x2 + x3 + rnorm(n, sd=0.1)
par(mfrow=c(2,2), mar=c(0.5, 0.5, 0.5, 0.5))
plot(x, y, pch=20)
plot(x, x2, pch=20)
plot(x2, x3, pch=20)
plot(x3, x, pch=20)
par(mfrow=c(1,1))

# Multivariate Regression: Compute coefficient of x
#   PRINCIPLE: 
#   Multivariate regression estimates are exactly those 
#   having adjusted for the linear effect of all other variables
#   from both the response variable and the explanatory variable 
#   A. Adjust for effect of x2, x3
#     Adjust for x2, x3
#     Adjust both y and x
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
plot(ey, ex)
#   B. Regress through origin
sum(ey*ex)/sum(ex^2)

# Multivariate Regression: Compare
coef(lm(y ~ x + x2 + x3))
