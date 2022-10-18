## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
biology <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2, 4, 7, 2, 9)
english <- c(8, 5, 4, 10, 4, 5, 7, 2, 6, 1, 2, 7, 0, 6, 4, 12, 5, 2)

qqnorm(biology)
qqline(biology)

## -----------------------------------------------------------------------------
qqnorm(english)
qqline(english)

## ---- echo = FALSE------------------------------------------------------------
got_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

## ---- eval = got_ggplot2------------------------------------------------------
library(ggplot2)

# make a data frame in long format for plotting
test_results <- data.frame(
  score = c(biology, english),
  department = c(
    rep("biology", length(biology)),
    rep("english", length(english))
  )
)

ggplot(test_results, aes(x = department, y = score, color = department)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
delta_0 <- 0

# by assumption
sigma_sq_1 <- 3
sigma_sq_2 <- 2

n_1 <- length(biology)
n_2 <- length(english)

# calculate the z-statistic
z_stat <- (mean(biology) - mean(english) - delta_0) / 
  sqrt(sigma_sq_1 / n_1 + sigma_sq_2 / n_2)

z_stat

## -----------------------------------------------------------------------------
library(distributions3)

Z <- Normal(0, 1)  # make a standard normal r.v.
1 - cdf(Z, 0.376) + cdf(Z, -0.376)

## -----------------------------------------------------------------------------
1 - cdf(Z, abs(z_stat)) + cdf(Z, -abs(z_stat))

## -----------------------------------------------------------------------------
2 * cdf(Z, -0.376)

## -----------------------------------------------------------------------------
1 - cdf(Z, -0.376)

## -----------------------------------------------------------------------------
cdf(Z, -0.376)

