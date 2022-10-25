N1 <- Normal(mu = c(100))
N2 <- Normal(mu = c(105))
M0 <- Mixture(N1, N2, weights = c(0.7,0.3))
M0
pdf(M0, c(4,7,100))
pdf(N1, c(4,7,100))*0.7 + pdf(N2, c(4,7,100))*0.3


B3 <- Binomial(size = c(8,10))
N3 <- Normal(mu = c(-100,100))
M <- Mixture(B3, N3, weights = c(.7,.3))
M
M <- Mixture(B3, N3, weights = matrix(c(0.7,0.3,0.5,0.5)))
M

pdf(M, c(4,7,100))
pdf(B3, c(4,7,100))*c(0.7,0.5)+pdf(N3, c(4,7,100))*c(0.3,0.5)

pdf(M, c(4,7))
pdf(B3, c(4,7))*c(0.7,0.5)+pdf(N3, c(4,7))*c(0.3,0.5)

pdf(M, c(4,7), elementwise = FALSE)
pdf(B3, c(4,7), elementwise = FALSE)*c(0.7,0.5) +pdf(N3, c(4,7), elementwise = FALSE)*c(0.3,0.5)


M2 <- Mixture(Normal(mu=c(0,1),1), Normal(mu=c(1,2),1), Normal(mu=c(2,3),1), weights = c(0.5,0.3,0.2))
M2
pdf(M2, c(0,1,0))
pdf(Normal(mu=c(0,1),1), c(0,1,0))*0.5 + pdf(Normal(mu=c(1,2),1), c(0,1,0))*0.3 + pdf(Normal(mu=c(2,3),1), c(0,1,0))*0.2

cdf(M2, c(0,1,0))
cdf(Normal(mu=c(0,1),1), c(0,1,0))*0.5 + cdf(Normal(mu=c(1,2),1), c(0,1,0))*0.3 + cdf(Normal(mu=c(2,3),1), c(0,1,0))*0.2

log(pdf(M2, c(0,1,0)))
pdf(M2, c(0,1,0), log=TRUE)
log_pdf(M2, c(0,1,0))

probs = c(NA, 1.5, 1, 0.5,0.7,0.35, -4)
quantile(M2, probs)
cdf(M2, c(0.6574192,1.6574192))
quantile(M, probs)
cdf(M, c(3,6)) ; cdf(M, c(2.99,5.99))
quantile(M0, c(0.3,0.5))

quantile(M2, c(0.3,0.5))
cdf(M2,quantile(M2, c(0.3,0.5)))

random(M2, 5)
random(M, 5)
random(M0, 5)

M2[2]
support(M)
is_discrete(M)
is_continuous(M)
is_continuous(M2)
weights(M2)
mean(M2)
variance(M2)
