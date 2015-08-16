# Q1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

lse <- function(mu) {
  sum(w*(x-mu)^2)    

}

test <- c(0.0025, 0.1471, 0.3, 1.077)
sapply(test, lse)

xx <- seq(0,0.2,len=10)
yy <- sapply(xx, lse)
plot(xx,yy) # Ans: 0.1471

# Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

r <- lm(y ~ x-1)
coef(r) # Ans: 0.8263

# Q3
data(mtcars)
mpg <- mtcars$mpg
weight <- mtcars$wt
class(mpg)
class(weight)
rmw <- lm(mpg ~ weight)
coef(rmw) # Ans: -5.344

# Q4
Cor = 0.5
sdx_over_sdy = 0.5
slope = Cor/sdx_over_sdy
slope # Ans: 1

# Q5
1.5*0.4 # Ans: 0.6

# Q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
(x-mean(x))/sd(x) # Ans: -0.9719

# Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

r <- lm(y ~ x)
coef(r) # Ans: 1.567

# Q8
# Identically 0

# Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x) # An: 0.573

# Q10

# Other
x <- rnorm(99)
y <- rnorm(99)
