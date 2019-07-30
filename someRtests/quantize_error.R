Tmisc()
N <- 10000

for(e in seq(0.01, 0.2, 0.01)) {
  varXb <- 1-e
  Xb <- rnorm(N, sd = sqrt(varXb))
  epsilon <- rnorm(N, sd=sqrt(1 - varXb))
  Y <- Xb + epsilon
  
  varY <- var(Y)
  l <- lm(Y ~ Xb)
  Yhat <- predict(l)
  res2 <- mean(residuals(l)^2)
  nbins <- round(1/res2)
  for(i in 1:length(nbins)) {
    # i <- 1
    cat <- cut(Yhat, breaks=quantile(Yhat, probs=seq(0,1, by=1/nbins[i])), include.lowest=TRUE)
    Ytilde <- ave(Yhat,cat, FUN=mean)
    fill.in.results.table(e=e, nbins=nbins[i], res.err=mean((Y - Ytilde)^2))
    
  }
}
