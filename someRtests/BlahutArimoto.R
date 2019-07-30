Tmisc()
#' Seeks to find p(T|X) for a particular p(X), d(x,t), and D
#' Let X \in {1,2,3,4,5}, T \in {1.5, 3, 4.5}
#' d(x,t) = (x-t)^2
#' 

setwd0("~/WORK/Projects/Theory/")
filename <- Rfilename("BlahutArimoto")
# set.seed(1000)

Xval <- 2:6
Tval <- c(1.5, 3, 4.5)
nX <- length(Xval)
nT <- length(Tval)
X1 <- rep(1, nX)
T1 <- rep(1, nT)

d <- (outer(X1, Tval) - outer(Xval, T1))^2
min.d <- apply(d, 1, FUN=min)
d <- d - outer(min.d, T1) # Normalized distortion measure

epsilon <- 1e-10
Beta <- exp(seq(-2,2, length=20))

rcat <- function(n) {
  x <- runif(n)
  return(x/sum(x))
}
pT.x_update <- function(pT, beta, d) {
  z <- exp(-beta*d) * outer(X1, pT)
  rs <- rowSums(z)
  pT.x <- z / outer(rs, T1)
  return(pT.x)
}
pT_update <- function(pT.x, pX) {
  z <- colSums(outer(pX, T1) * pT.x)
  return(as.vector(z))
}
D.KL <- function(pX, pT, pT.x) {
  pXT <- outer(pX, T1) * pT.x
  KL0 <- log(pXT) - (log(outer(X1, pT)) + log(outer(pX, T1)))
  KL <- pXT * KL0
  return(sum(KL))
}

pX <- 1:nX; pX <- pX / sum(pX)
D <- RD <- rep(NA, length(Beta))
pT_saved <- pT.x_saved <- list()

for(i in 1:length(Beta)) {
  # i <- 1
  beta <- Beta[i]
  pT <- rcat(nT) # initialization
  
  pT.x <- pT.x_update(pT=pT, beta=beta, d=d)
  
  R.old <- Inf
  R <- D.KL(pX=pX, pT=pT, pT.x=pT.x)
  
  while(R.old - R >= epsilon) {
    pT.x <- pT.x_update(pT=pT, beta=beta, d=d)
    pT <- pT_update(pT.x=pT.x, pX = pX)
    R.old <- R
    R <- D.KL(pX=pX, pT=pT, pT.x=pT.x)
  }
  RD[i] <- R
  pT_saved[[i]] <- pT
  pT.x_saved[[i]] <- pT.x
  D[i] <- sum(d * pT.x * outer(pX, T1))
  
}

HX <- -sum(pX * log(pX))

PDF(paste0(filename, ".pdf"))
plot(D, RD, type="o", ylim=c(0,HX), xlim=c(0, 1))
abline(h=HX)
dev.copy2PDF()

HT <- sapply(pT_saved, function(pT) -sum(pT*log(pT)))
