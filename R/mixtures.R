Mixture <- function(..., weights){ 
  ellipsis::check_dots_used()
  UseMethod("Mixture")
}

Mixture.distribution <- function(..., weights){ 
  K <- list(...)
  if (length(K) == 1) return(K[[1]])
  d1 <- length(K[[1]])
  d2 <- length(K)
  weights <- matrix(weights, d2, d1)
  weights <- as.list(as.data.frame(t(weights)))
  names(weights) <- NULL
  x <- data.frame(distributions = I(K), weights = I(weights))
  class(x) <- c("Mixture", "distribution")
  x
}


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
                                                       apply(sapply(x$weights, function(y) format(unlist(y), digits, ...)),1, function(z) paste(z, collapse = ", "))), 2L, function(p) paste(names(x), 
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

pdf(M, c(4,7,100))
pdf(B3, c(4,7,100))*c(0.7,0.3)+pdf(N3, c(4,7,100))*c(0.7,0.3)

pdf(M, c(4,7))
pdf(B3, c(4,7))*c(0.7,0.3)+pdf(N3, c(4,7))*c(0.7,0.3)

pdf(M, c(4,7), elementwise = FALSE)
pdf(B3, c(4,7), elementwise = FALSE)*c(0.7,0.3)+pdf(N3, c(4,7), elementwise = FALSE)*c(0.7,0.3)


M2 <- Mixture(Normal(mu=c(0,1),1), Normal(mu=c(1,2),1), Normal(mu=c(2,3),1), weights = c(0.5,0.3,0.2))
M2
pdf(M2, c(0,1,0))
pdf(Normal(mu=c(0,1),1), c(0,1,0))*0.5 + pdf(Normal(mu=c(1,2),1), c(0,1,0))*0.3 + pdf(Normal(mu=c(2,3),1), c(0,1,0))*0.2

cdf(M2, c(0,1,0))
cdf(Normal(mu=c(0,1),1), c(0,1,0))*0.5 + cdf(Normal(mu=c(1,2),1), c(0,1,0))*0.3 + cdf(Normal(mu=c(2,3),1), c(0,1,0))*0.2

log(pdf(M2, c(0,1,0)))
pdf(M2, c(0,1,0), log=TRUE)
log_pdf(M2, c(0,1,0))

#' @export
mean.Mixture <- function(x, ...) {
  ellipsis::check_dots_used()
  setNames(x$mu, names(x))
}

#' @export
variance.Mixture <- function(x, ...) {
  setNames(x$sigma^2, names(x))
}


pdf.Mixture <- function(d, x, log = FALSE, ...) {
  Z <- Reduce("+", mapply(function(A,b) A*b, lapply(d$distributions, pdf, x, ...), d$weights, SIMPLIFY = FALSE))
  if (log) log(Z) else Z
}

cdf.Mixture <- function(d, x, lower.tail = TRUE, log.p = FALSE, ...) {
  Z <- Reduce("+", mapply(function(A,b) A*b, lapply(d$distributions, cdf, x, ...), d$weights, SIMPLIFY = FALSE))
  if (!lower.tail) Z <- 1 - Z
  if (log.p) log(Z) else Z
}


log_pdf.Mixture <- function(d, x, ...) {
  Z <- mapply(function(A,b) A+log(b), lapply(d$distributions, pdf, x, log = TRUE), d$weights, SIMPLIFY = FALSE)
  normalize <- Reduce(pmax, Z)
  log(Reduce("+", lapply(Z, function(X) exp(X - normalize)))) + normalize
}



quantile.Mixture <- function(x, probs, lower.tail = TRUE, log.p = FALSE, ...) {
  tol <- .Machine$double.eps^0.5 #get_opt("tol") or argument
  sub <- 1e-10 #get_opt("sub") or argument
  
  control <- list(...)
  d1 <- length(x$distributions[[1]])
  d2 <- length(x$distributions)
  elementwise <- control$elementwise
  if(is.null(elementwise)) elementwise <- d1 == length(probs)
  if(d1 != length(probs)) elementwise <- FALSE
  Z <- if(elementwise) matrix(numeric(length(probs)), nrow = 1) else matrix(numeric(d1*length(probs)), nrow = d1)
  
  nat <- is.na(probs)
  Z[,nat] <- probs[nat]
  if(elementwise && any(nat)) x$distributions <- lapply(x$distributions, function(y) y[!nat])
  p <- probs[!nat]
  if (log.p)
    p <- exp(p)
  if (!lower.tail)
    p <- 1 - p
  zz <- matrix(numeric(dim(Z)[1]*length(p)), nrow = dim(Z)[1])
  ok <- p >= 0 & p <= 1
  p <- p[ok]
  if(elementwise && any(nat)) x$distributions <- lapply(x$distributions, function(y) y[ok])
  z <- matrix(numeric(dim(Z)[1]*length(p)), nrow = dim(Z)[1])
  z[,p == 0] <- unname(support(M)[,1])
  z[,p == 1] <- unname(support(M)[,2])
  inside <- p < 1 & p > 0
  if (sum(inside) > 0) {
    p <- p[inside]
    if(elementwise && any(nat)) x$distributions <- lapply(x$distributions, function(y) y[inside])
    qf <- function(p) {
      interval <- range(unlist(lapply(x$distributions, function(X) quantile(X, p, elementwise = elementwise)), use.names = FALSE))
      if (interval[1] == interval[2]) {return(interval[1])}
      uniroot(function(x) p(O, x) - p, interval = interval, tol = tol, extendInt = "upX")$root
    }
    qq <- unlist(lapply(p, qf))
    z[inside] <- qq
  }
  zz[ok] <- z
  zz[!ok] <- NaN
  Z[!nat] <- zz
  Z
}


support.Mixture <- function(d, drop = TRUE, ...) {
  ellipsis::check_dots_used()
  V <- apply(Reduce(cbind, lapply(d$distributions, support)),1,range)
  make_support(V[1,], V[2,], d, drop = drop)
}

#' @exportS3Method
is_discrete.Mixture <- function(d, ...) {
  ellipsis::check_dots_used()
  apply(Reduce(cbind, lapply(d$distributions, is_discrete)),2, all)
}

#' @exportS3Method
is_continuous.Mixture <- function(d, ...) {
  ellipsis::check_dots_used()
  apply(Reduce(cbind, lapply(d$distributions, is_continuous)),2, all)
}


Reduce("+", mapply(function(A,b) A*b, lapply(M$distributions, pdf, c(4,7,100)),M$weights, SIMPLIFY = FALSE))
pdf(B3, c(4,7,100))*c(0.7,0.3)+pdf(N3, c(4,7,100))*c(0.7,0.3)
