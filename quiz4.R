
# Question 1 ---------------------------------------------------------------
fit <- glm(use ~ wind, data=shuttle, family=binomial())

exp(fit$coeff[2])


# Question 2 ------------------------------------------------------

fit <- glm(use ~ wind + magn, data=shuttle, family=binomial())

exp(fit$coeff[2])


# Question 4 ------------------------------------------------------

fit <- glm(count ~ ., data=InsectSprays, family=poisson())
# coef[2] is for B which is B/A, negate for A/B then take exp()
# because the log odds is b/a. we are looking for regular odds.
exp(-fit$coef[2])

## OR ##
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
# then it's Est A/B:
14.5/15.333333

# Question 6 ------------------------------------------------------

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

