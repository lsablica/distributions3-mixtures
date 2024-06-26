#' @export
#'
Mixture <- function(..., weights){ 
  ellipsis::check_dots_used()
  UseMethod("Mixture")
}

#' @export
#'
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

#' @export
#'
format.Mixture <- function (x, digits = pmax(3L, getOption("digits") - 3L), ...) {
  cl <- class(x)[1L]
  if (length(x) < 1L) {
    return(character(0))
  }
  n <- names(x)
  d1 <- length(x$distributions[[1]])
  d2 <- length(x$distributions)
  if (is.null(attr(x, "row.names"))) 
    attr(x, "row.names") <- 1L:length(x)
  class(x) <- "data.frame"
  f <- sprintf("%s distribution (%s)", cl, apply(rbind(apply(matrix(sapply(x$distribution, format, digits, ...),d1,d2),1, function(y){paste(y, collapse = ", ")} ), 
                                                       apply(matrix(sapply(x$weights, function(y) format(unlist(y), digits, ...)),d1,d2),1, function(z) paste(z, collapse = ", "))), 
                                                 2L, function(p) paste(names(x), "=", as.vector(p), collapse = ", ")))
  setNames(f, n)
}

#' @export
#'
weights <- function(X){ 
  UseMethod("weights")
}

#' @export
#'
weights.Mixture <- function(X){
  unname(Reduce(rbind, X$weights))
}

#' @export
#'
distributions <- function(X){ 
  UseMethod("distributions")
}

#' @export
#'
distributions.Mixture <- function(X){
  X$distributions
}

#' @export
#'
`[.Mixture` <- function(x, condition) {
  x$distributions <- lapply(x$distributions, function(y) y[condition])
  x$weights <- lapply(x$weights, function(y) y[condition])
  x
}




#' @export
mean.Mixture <- function(x, ...) {
  ellipsis::check_dots_used()
  setNames(rowSums(sapply(x$distributions,mean)*t(weights(x))), names(x))
}

#' @export
variance.Mixture <- function(x, ...) {
  ellipsis::check_dots_used()
  setNames(rowSums((sapply(x$distributions,variance) + sapply(x$distributions,mean)^2)*t(weights(x))) - mean(x)^2, names(x))
}

#' @export
#'
pdf.Mixture <- function(d, x, log = FALSE, ...) {
  Z <- Reduce("+", mapply(function(A,b) A*b, lapply(d$distributions, pdf, x, ...), d$weights, SIMPLIFY = FALSE))
  if (log) log(Z) else Z
}

#' @export
#'
cdf.Mixture <- function(d, x, lower.tail = TRUE, log.p = FALSE, ...) {
  Z <- Reduce("+", mapply(function(A,b) A*b, lapply(d$distributions, cdf, x, ...), d$weights, SIMPLIFY = FALSE))
  if (!lower.tail) Z <- 1 - Z
  if (log.p) log(Z) else Z
}

#' @export
#'
log_pdf.Mixture <- function(d, x, ...) {
  Z <- mapply(function(A,b) A+log(b), lapply(d$distributions, pdf, x, log = TRUE), d$weights, SIMPLIFY = FALSE)
  normalize <- Reduce(pmax, Z)
  log(Reduce("+", lapply(Z, function(X) exp(X - normalize)))) + normalize
}


#' @export
#'
quantile.Mixture <- function(x, probs, lower.tail = TRUE, log.p = FALSE, ...) {
  tol <- .Machine$double.eps^0.5 #get_opt("tol") or argument
  
  control <- list(...)
  d1 <- length(x$distributions[[1]])
  d2 <- length(x$distributions)
  elementwise <- control$elementwise
  if(is.null(elementwise)) elementwise <- d1 == length(probs)
  if(d1 != length(probs)) elementwise <- FALSE
  Z <- if(elementwise) matrix(numeric(length(probs)), nrow = 1) else matrix(numeric(d1*length(probs)), nrow = d1)
  nat <- is.na(probs)
  Z[,nat] <- probs[nat]
  if(elementwise && any(nat)) x <- x[!nat]
  p <- probs[!nat]
  if (log.p) p <- exp(p)
  if (!lower.tail) p <- 1 - p
  zz <- matrix(numeric(dim(Z)[1]*length(p)), nrow = dim(Z)[1])
  ok <- p >= 0 & p <= 1
  p <- p[ok]
  if(elementwise && any(!ok)) x <- x[ok]
  z <- matrix(numeric(dim(Z)[1]*length(p)), nrow = dim(Z)[1])
  if(elementwise){
    z[,p == 0] <- unname(support(M)[,1])[p == 0]
    z[,p == 1] <- unname(support(M)[,2])[p == 1]
  } else{
    z[,p == 0] <- unname(support(M)[,1])
    z[,p == 1] <- unname(support(M)[,2])
  }
  inside <- p < 1 & p > 0
  if (sum(inside) > 0) {
    p <- p[inside]
    if(elementwise && any(!inside)) x <- x[inside]
    qf <- function(p, x) {
      interval <- range(unlist(lapply(x$distributions, function(X) quantile(X, p, elementwise = elementwise)), use.names = FALSE))
      if (interval[1] == interval[2]) {return(interval[1])}
      uniroot(function(z) cdf(x, z) - p, interval = interval, tol = tol, extendInt = "upX")$root
    }
    newdim <- length(x$distributions[[1]])
    qq <- sapply(seq_len(newdim), function(i){
       X <- x[i]
      if(elementwise){
        qf(p[i], X)
      } else{
        unlist(lapply(p, qf, X))
      }
    })
    z[,inside] <- t(qq)
  }
  zz[,ok] <- z
  zz[,!ok] <- NaN
  Z[,!nat] <- zz
  if(elementwise){ 
    Z[,,drop=TRUE]
  } else{
    colnames(Z) <- paste("q",make_suffix(probs, digits = pmax(3L, getOption("digits") - 3L)), sep = "_")
    Z
  } 
}

#' @export
#'
random.Mixture <- function(x, n = 1L, drop = TRUE, ...) {
  n <- make_positive_integer(n)
  if (n == 0L) {
    return(numeric(0L))
  }
  d1 <- length(x$distributions[[1]])
  d2 <- length(x$distributions)
  Z <- matrix(0, nrow = d1, ncol = n)
  weights <- weights(x)
  sapply(seq_len(d1), function(i){
    t <- sample(d2, n, replace = TRUE, prob = weights[,i])
    tt <- table(t)
    lapply(seq_along(tt), function(j) {
      nn <- tt[j]
      dist <- as.numeric(names(nn))
      Z[i,t == dist] <<- random(x[i]$distributions[[dist]], nn)
    })
  })
  colnames(Z) <- paste("r", make_suffix(seq_len(n), digits = pmax(3L, getOption("digits") - 3L)), sep = "_")
  Z
}

#' @export
#'
support.Mixture <- function(d, drop = TRUE, ...) {
  ellipsis::check_dots_used()
  V <- apply(Reduce(cbind, lapply(d$distributions, support)),1,range)
  make_support(V[1,], V[2,], d, drop = drop)
}

#' @exportS3Method
is_discrete.Mixture <- function(d, ...) {
  ellipsis::check_dots_used()
  apply(Reduce(cbind, lapply(d$distributions, is_discrete)),1, all)
}

#' @exportS3Method
is_continuous.Mixture <- function(d, ...) {
  ellipsis::check_dots_used()
  apply(Reduce(cbind, lapply(d$distributions, is_continuous)),1, all)
}

