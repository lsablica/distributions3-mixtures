## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##",
  message = FALSE,
  warning = FALSE,
  echo = TRUE
)

## -----------------------------------------------------------------------------
library("distributions3")
Y <- Poisson(lambda = 1.5)
print(Y)
mean(Y)
variance(Y)
pdf(Y, 0:5)
cdf(Y, 0:5)
quantile(Y, c(0.1, 0.5, 0.9))
set.seed(0)
random(Y, 5)

## ---- echo=-1, fig.height=8, fig.width=12, out.width="100%"-------------------
par(mfrow = c(2, 2))
plot(Poisson(0.5), main = expression(lambda == 0.5), xlim = c(0, 15))
plot(Poisson(2),   main = expression(lambda == 2),   xlim = c(0, 15))
plot(Poisson(5),   main = expression(lambda == 5),   xlim = c(0, 15))
plot(Poisson(10),  main = expression(lambda == 10),  xlim = c(0, 15))

## -----------------------------------------------------------------------------
data("FIFA2018", package = "distributions3")
head(FIFA2018)

## -----------------------------------------------------------------------------
summary(FIFA2018$goals)

## -----------------------------------------------------------------------------
observed <- prop.table(table(FIFA2018$goals))
observed

## -----------------------------------------------------------------------------
p_const <- Poisson(lambda = mean(FIFA2018$goals))
p_const

## -----------------------------------------------------------------------------
expected <- pdf(p_const, 0:6)
cbind(observed, expected)

## -----------------------------------------------------------------------------
m <- glm(goals ~ difference, data = FIFA2018, family = poisson)
summary(m)

## -----------------------------------------------------------------------------
lambda_zero <- exp(coef(m)[1])
lambda_zero

## -----------------------------------------------------------------------------
predict(m, newdata = data.frame(difference = 0), type = "response")

## -----------------------------------------------------------------------------
p_zero <- Poisson(lambda = lambda_zero)
pdf(p_zero, 0:6)

## -----------------------------------------------------------------------------
p_reg <- Poisson(lambda = fitted(m))
length(p_reg)
head(p_reg)

## -----------------------------------------------------------------------------
tail(FIFA2018, 2)
p_final <- tail(p_reg, 2)
p_final
pdf(p_final, 0:6)

## -----------------------------------------------------------------------------
res <- outer(pdf(p_final[1], 0:6), pdf(p_final[2], 0:6))
round(100 * res, digits = 2)

## -----------------------------------------------------------------------------
sum(res[lower.tri(res)]) ## France wins
sum(diag(res))           ## draw
sum(res[upper.tri(res)]) ## France loses

## -----------------------------------------------------------------------------
expected <- pdf(p_reg, 0:6)
head(expected)
expected <- colMeans(expected)
cbind(observed, expected)

## ---- echo=-1, fig.height=4.5, fig.width=5, out.width="60%"-------------------
par(mar = c(4, 4, 1, 1))
bp <- barplot(sqrt(observed), offset = sqrt(expected) - sqrt(observed),
  xlab = "Goals", ylab = "sqrt(Frequency)")
lines(bp, sqrt(expected), type = "o", pch = 19, lwd = 2, col = 2)
abline(h = 0, lty = 2)

