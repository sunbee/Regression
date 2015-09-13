# 1. Preliminary observation of data
library(datasets)
data(mtcars)
str(mtcars)
pairs(mtcars)

# Some interesting obserations:
# 1.  Mileage (mpg) appears correlated to:
#       cyl:  no. of cylinders 
#       disp: displacement
#       hp:   horsepower
#       wt:   weight
#       am:   tramsmission (auto/manual)
# 2.  Further:
#     The following show high degree of correlation among themselves
#       cyl, disp, hp, wt
#     Engines with higher displacement tend to have more cylinders, weigh more and
#     deliver more power.

# 2. Full Model
ModelPrelim <- lm(mpg ~ ., data=mtcars)
summary(ModelPrelim)
# Note: None of the explanatory variables show sufficient explanatory power.
# The model has an R-squared of 0.81.

ModelDropOne <- update(ModelPrelim, .~.-disp) 
summary(ModelDropOne)
anova(ModelPrelim, ModelDropOne)

ModelDropTwo <- update(ModelDropOne, .~.-hp) 
summary(ModelDropTwo)
anova(ModelPrelim, ModelDropTwo)

ModelDropThree <- update(ModelDropTwo, .~.-wt) 
summary(ModelDropThree)
anova(ModelPrelim, ModelDropThree)

# Drop by least significance?

# Automate:
step(ModelPrelim, direction="backward")
ModelStepBackward <- lm(mpg ~ wt + qsec + am, data=mtcars)
summary(ModelStepBackward)

# 3. Regression Diagnostics
par(mfrow=c(2,2))
plot(ModelStepBackward)
par(mfrow=c(1,1))
# COnsider removing points

# 4. Effect of automatic transmission (am)
# When wt and qsec are included as confounders
boxplot(mpg ~ am, data=mtcars)  # Suggests transmission has an effect
ModelSimple <- lm(mpg ~ am, data=mtcars)
summary(ModelSimple)            # Simple Model: Effect of am is 7.245
boxplot(predict(ModelStepBackward) ~ mtcars$am) # Confounders reduce the effect
summary(ModelStepBackward)      # Adjusted Model: Effect of am is 2.936                          

summary(ModelSimple)
summary(ModelStepBackward)
anova(ModelSimple, ModelStepBackward) # Difference is significant
