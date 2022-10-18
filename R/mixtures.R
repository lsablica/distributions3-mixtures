Mixture <- function(..., weights){ 
  ellipsis::check_dots_used()
  UseMethod("Mixture")
}

Mixture.distribution <- function(..., weights){ 
  K <- list(...)
  if (length(K) == 1) return(K[[1]])
  d1 <- length(K[[1]])
  d2 <- length(K)
  weights <- matrix(weights, d1, d2)
  weights <- as.list(as.data.frame(weights))
  names(weights) <- NULL
  x <- data.frame(distributions = I(K), weights = I(weights))
  class(x) <- c("Mixture", "distribution")
  x
}


K <- list(list(B1,B2),list(B1,B2))
weights <- list(c(0.3,0.7), c(0.2,0.8))

format.Mixture <- function (x, digits = pmax(3L, getOption("digits") - 3L), ...) {
  cl <- class(x)[1L]
  if (length(x) < 1L) {
    return(character(0))
  }
  n <- names(x)
  if (is.null(attr(x, "row.names"))) 
    attr(x, "row.names") <- 1L:length(x)
  class(x) <- "data.frame"
  f <- sprintf("%s distribution (%s)", cl, apply(rbind(apply(sapply(x$distribution, format, digits, ...),1, function(y){paste(y, collapse = ", ")} ), 
                                                       apply(sapply(x$weights, function(y) format(unlist(y), digits, ...)),2, function(z) paste(z, collapse = ", "))), 2L, function(p) paste(names(x), 
                                                                                                                       "=", as.vector(p), collapse = ", ")))
  setNames(f, n)
}

weights <- function(X){ 
  UseMethod("weights")
}

weights.Mixture <- function(X){
  matrix(unlist(X$weights), nrow = length(X$distributions))
}

distributions <- function(X){ 
  UseMethod("distributions")
}

distributions.Mixture <- function(X){
  X$distributions
}


B3 <- Binomial(size = c(8,10))
N3 <- Normal(mu = c(-100,100))
K <- list(B3,N3)
weights <- c(0.7,0.3)


M <- Mixture(B3, N3, weights = weights)
M <- Mixture(B3, N3, weights = matrix(c(0.7,0.3,0.7,0.3)))

#' @export
mean.Mixture <- function(x, ...) {
  ellipsis::check_dots_used()
  setNames(x$mu, names(x))
}

#' @export
variance.Mixture <- function(x, ...) {
  setNames(x$sigma^2, names(x))
}
Reduce("+", mapply(function(A,b) A*b, lapply(M$distributions, pdf, c(4,7,100)),M$weights, SIMPLIFY = FALSE))
pdf(B3, c(4,7,100))*c(0.7,0.3)+pdf(N3, c(4,7,100))*c(0.7,0.3)
