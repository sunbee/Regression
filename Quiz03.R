data(mtcars)

# Q1
Mil <- lm(mpg ~ as.factor(cyl) + wt, data=mtcars)
summary(Mil)$coefficients

Out <- predict(Mil, newdata=data.frame(cyl=c(8,4), wt=(mean(mtcars$wt))))
Out[2]-Out[1] # 6.07

# Q2
MilCyl <- lm(mpg ~ as.factor(cyl), data=mtcars)
summary(MilCyl)$coefficients
OutUadj <- predict(MilCyl, newdata=data.frame(cyl=c(8,4)))
OutUadj[2]-OutUadj[1]
# Ans: #Cylinders appears to have more influene if wt is disregarded

# Q3
Mo1 <- lm(mpg ~ as.factor(cyl)+wt, data=mtcars)
Mo2 <- lm(mpg ~ as.factor(cyl)*wt, data=mtcars)
summary(Mo1)
summary(Mo2)
anova(Mo1, Mo2)
