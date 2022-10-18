## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE
)

## ----echo = FALSE-------------------------------------------------------------
set.seed(27)

before <- round(rnorm(10, 50, 20))
after <- round(before + 5 + rnorm(10, 0, 10))

knitr::kable(data.frame(person = 1:10, before, after))

## -----------------------------------------------------------------------------
before <- c(88, 73, 35, 21, 28, 56, 50, 73, 93, 55)
after <- c(80, 78, 56, 28, 26, 50, 39, 67, 98, 63) 

diff <- after - before
diff

