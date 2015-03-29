
# Question 1 ------------------------------------------------------

fit <- lm(mpg ~ factor(cyl) + wt, mtcars)
fit


# Question 2 -------------------------------------------------------

fit1 <- lm(mpg ~ factor(cyl), mtcars)
fit
fit1


# Question 3 -------------------------------------------------------
fit1 <- lm(mpg ~ factor(cyl) + wt + factor(cyl)*wt, mtcars)
summary(fit1)
library(lmtest)
lrtest(fit,fit1)

# Question 4 -------------------------------------------------------
fit3 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)

# Question 5 -------------------------------------------------------
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
plot(x,y)
fit5 <- lm(y ~ x)
hatvalues(fit5)

# Question 6 -------------------------------------------------------
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6 <- lm(y ~ x)
dfbetas(fit6)
