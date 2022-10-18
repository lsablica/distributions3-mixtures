## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  echo = FALSE
)

## ---- echo = FALSE------------------------------------------------------------
got_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

## ---- eval = got_ggplot2------------------------------------------------------
library(ggplot2)
library(distributions3)

z_score <- 1.6

grid <- seq(-4, 4, length.out = 300)
density <- pdf(Normal(), grid)
reject <- ifelse(abs(grid) >= z_score, density, 0)

ggplot(data = NULL) +
  geom_area(aes(grid, reject, alpha = 0.2), fill = "steelblue") +
  geom_line(aes(grid, density), size = 1, color = "grey") +
  geom_vline(xintercept = z_score, size = 1, color = "darkgrey") +
  geom_text(
    aes(x = z_score, y = 0.25, label="\nobserved z-score = 1.6"),
    angle = 90
  ) +
  labs(
    title = "Area of sampling distribution corresponding to p-value",
    subtitle = "Sampling distribution of Z under the null distribution",
    y = "Density",
    x = "Support"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

