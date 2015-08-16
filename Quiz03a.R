#Q1: Compare effect on mpg of 8 vs 4 cylinders, where cyl is a factor variable
#     and wt is a confounder
data(mtcars)
fit <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
summary(fit)
co <- coefficients(fit)
co["as.factor(cyl)8:wt"]+co["wt"]+co["as.factor(cyl)8"]
# Ans: Holding weight constant, 
# cylinder appears to have more of an impact on mpg
# than if weight is disregarded.

#Q2: Compare effect of mpg on 8 vs 4 cylinders basis 
#     the adjusted and unadjusted model where wt is confounder
data(mtcars)
fitAdjusted <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
fitUnAdjusted <- lm(mpg ~ as.factor(cyl), data=mtcars)

with(data=mtcars, plot(mpg ~ as.factor(cyl)))
with(data=mtcars, abline(fitAdjusted))
with(data=mtcars, abline(fitUnAdjusted))

#Q3: Compare the models with and without interaction 
data(mtcars)
fitAdditive <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
fitMultiplicative <- lm(mpg ~ as.factor(cyl)*wt, data=mtcars)
anova(fitAdditive, fitMultiplicative)

#Q4: Give the hat diagonal for the most influential point
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

Model <- lm(y ~ x) 
plot(x, y, type="p", pch=19)
abline(Model, col="red")
?influence
influence(Model)

# Leverage:
# Leverage is a function of the explanatory variables alone 
#   and measures the potential for a data point 
#   to affect the model parameter estimates
# Influence is a measure of how a data point
#   actually does affect a model.

# Examine the residuals
plot(fitted(Model), residuals(Model), pch=19)
lines(lowess(fitted(Model), residuals(Model)), lty=3, col="red")

# Leverage: Examine hats derived from the design matrix
h <- hat(model.matrix(Model))
plot(h, type="h") # Identify high leverage observation(s), here obs. #5

# Influence: Examine influence
plot(cooks.distance(Model), type="h")

# Residuals
plot(residuals(Model), type="h")  # Obs. #1, #3 show largest residuals
plot(rstudent(Model), type="h")   # Obs. #5 is the largest when normalized
# Normalization of residuals:
#   For any observation:
#     The response variable has inherent variation
#     Variance of response variable is equal to variance of residual
#   Ideal model: For any observation.. 
#     Regression coefficents have zero variation
#     Variance of response variable is equal to the variance of residual
#       var(y) = var(e)
#   Estimated model: For any observation..
#     Estimated coefficient have some variation - intercept, slope
#     Estimated residuals have some variation 
#       r(i) = eHat(i)/sd(eHat(i))
#       var(eHat(i)) = var(e(i))*(1-Hat(i))    
#           Hat: Diagonal of the Hat matrix
#           MSE: Estimate of var(e) 

#   Homoskedasticity: 
#     Variation of the residual is the same for any observation
#     Therefore residuals may be pooled
#     Variation of the pool is the variation of response variable

#dfbetas
dfbetas(Model)


# What is the hat matrix?

# What are the magical properties?