#Q1: Compare effect on mpg of 8 vs 4 cylinders, where cyl is a factor variable
#     and wt is a confounder
# Fit a linear regression model with no. cylinders (as factor) and weight 
# Examine: factor-levels of cylinder - 4,6,8
# Examine: Coefficients
#   1. Intercept represents the effect of 4 cylinders. 
#   2. The 3rd (2nd) coefficient captures the additive effect of 8 (6) cylinders
#   3. The weight is a confounder and drops out on comparison
data(mtcars)
fit <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
summary(fit)
co <- coefficients(fit)
co["as.factor(cyl)8"] # -6.071


#Q2: Compare effect of mpg on 8 vs 4 cylinders basis 
#     the adjusted and unadjusted model where wt is confounder
# Fit a linear regression model 
#   1. ADJUSTED:    With no. cylinders (as factor) and weight. 
#   2. UNADJUSTED:  With  no. cylinders (as factor).  
# Compute the difference between 8 and 4 cylinders. Refer Q1.
#   1. The coefficient 
# Note: A confounding variable changes (reduces) the main effect,
#   when included in a model.

data(mtcars)
fitAdjusted <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
fitUnAdjusted <- lm(mpg ~ as.factor(cyl), data=mtcars)
coAdjusted <- coefficients(fitAdjusted)
coUnAdjusted <- coefficients(fitUnAdjusted)
coUnAdjusted["as.factor(cyl)8"]   # -11.56 when wt is disregarded
coAdjusted["as.factor(cyl)8"]     # -6.071 when wt is included 

# Ans: Holding weight constant, no. of cylinders appears to have less of an impact 
# on mpg than if weight is disregarded.

with(data=mtcars, plot(mpg ~ as.factor(cyl)))
with(data=mtcars, abline(fitAdjusted))    # With wt, lower gradient by cyl.
with(data=mtcars, abline(fitUnAdjusted))  # Without wt, higher gradient by cyl.

#Q3: Compare the models with and without interaction 
# Fit a linear regression model 
#   1. ADDITIVE:    With no interaction term for cylinders (as factor) and weight
#   2. INTERACTIVE: With an interaction term 
# A p-value smaller than 0.05 indicates that the observed diference is unlikely
# to have occured by chance along, suggesting that interaction term is necessary.
# Contrariwise, in the absence of a sufficiently small p-value there is insufficient
# evidence to adduce that interaction is necessary. 

data(mtcars)
fitAdditive <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
fitInteractive <- lm(mpg ~ as.factor(cyl)*wt, data=mtcars)
anova(fitAdditive, fitInteractive)

# Ans: The P-value is larger than 0.05. So, according to our criterion, 
# we would fail to reject, which suggests that the interaction terms 
# may not be necessary.

#Q4: 
HalfTon <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
# Ans: The estimated expected change in MPG per half ton increase in weight 
# for a specific number of cylinders (4, 6, 8). ERR

#Q5: Give the hat diagonal for the most influential point
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
plot(h, type="h") # Identify high leverage observation(s) based on hat diagonal,
                  # here obs. #5

# Influence: Examine influence
plot(cooks.distance(Model), type="h") # Identify influential points based on Cook's distance
                                      # here obs. #5 

h[5] # 0.9946

#Q6: Give the slope DFBETA for the point with highest hat value
#     This is the same data as in Q5.
#     Obs. #5 has the highest hat value.
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)

dfbetas(Model)
dfbetas(Model)[5,2]  # -134

#Q7: 
# It is possible for the coefficient to reverse sign after adjustment. 
# For example, it can be strongly significant and positive before adjustment 
# and strongly significant and negative after adjustment.

# Note:
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