# Trying to show that the mutual information converges to a limit in quantization
Tmisc()
corrmatrix <- function(cor) {
  X <- matrix(cor, 2,2)
  diag(X) <- 1
  return(X)
}
corr <- corrmatrix(0.5)
nbins <- c(2, 3, 10, 20, 50, 100) 
# nbins <- 4
Hx <- MI <- rep(NA, length(nbins))
for(i in 1:length(nbins)) {
  # i <- 1
  N <- nbins[i]
  n <- N - 1 
  s <- seq(0, 1, length.out = n+2)
  q <- qnorm(s)
  P <- matrix(NA, N, N)
  for(j in 1:N) {
    for(k in 1:N) {
      # j <- 1;k <- 1
      lower <- c(q[k], q[j]) 
      upper <- c(q[k+1], q[j+1])
      P[j,k] <- pmvnorm(lower=lower, upper=upper, sigma=corr)
    }
    Px <- colSums(P)
    Py <- rowSums(P)
    Pind <- outer(Py, Px)
    MI[i] <- sum(P * (log(P) - log(Pind)))
    Hx[i] <- -sum(Px * log(Px))
  }
}

print(MI)
print(exp(Hx))

