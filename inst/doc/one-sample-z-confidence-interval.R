## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## -----------------------------------------------------------------------------
library(distributions3)

# read in the data
x <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)
n <- length(x)

# make a standard normal r.v.
Z <- Normal(0, 1)

# first approach
mean(x) + quantile(Z, 0.12 / 2) * 2 / sqrt(n)
mean(x) + quantile(Z, 1 - 0.12 / 2) * 2 / sqrt(n)

## -----------------------------------------------------------------------------
# second approach
mean(x) - quantile(Z, 1 - 0.12 / 2) * 2 / sqrt(n)
mean(x) + quantile(Z, 1 - 0.12 / 2) * 2 / sqrt(n)

## ---- echo = FALSE------------------------------------------------------------
got_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
got_cowplot <- requireNamespace("cowplot", quietly = TRUE)
got_ggplot2_and_cowplot <- got_ggplot2 * got_cowplot

## ---- echo = FALSE, eval = got_ggplot2_and_cowplot----------------------------
library(ggplot2)
library(cowplot)

grid <- seq(-4, 4, length.out = 300)
density <- pdf(Z, grid)
lower_trunc <- ifelse(grid <= 1.96, density, 0)
upper_trunc <- ifelse(grid >= 1.96, density, 0)

lower_quantile_plot <- ggplot(data = NULL) +
  geom_area(aes(grid, lower_trunc, alpha = 0.2), fill = "steelblue") +
  geom_line(aes(grid, density), size = 1, color = "grey") +
  geom_vline(xintercept = 1.96, size = 1, color = "darkgrey") +
  geom_text(
    aes(x = 3.27, y = 0.3, label = "z[0.975] == 1.96"),
    parse = TRUE,
    size = 4
  ) +
  labs(
    title = "Lower tail quantile of a standard normal",
    subtitle = "Integral of shaded region is 0.975",
    y = "Density",
    x = "Support"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

upper_quantile_plot <- ggplot(data = NULL) +
  geom_area(aes(grid, upper_trunc, alpha = 0.2), fill = "steelblue") +
  geom_line(aes(grid, density), size = 1, color = "grey") +
  geom_vline(xintercept = 1.96, size = 1, color = "darkgrey") +
  geom_text(
    aes(x = 3.27, y = 0.3, label = "z[0.025] == 1.96"),
    parse = TRUE,
    size = 4
  ) +
  labs(
    title = "Upper tail quantile of a standard normal",
    subtitle = "Integral of shaded region is 0.025",
    y = "Density",
    x = "Support"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

cowplot::plot_grid(lower_quantile_plot, upper_quantile_plot)

