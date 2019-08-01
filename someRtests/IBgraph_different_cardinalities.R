#' Trying to draw a graph to show bifurcations in the Information Bottleneck curve
#' when the cardinality of Xhat changes 
#' Formula of an ellipse: x^2/a + y^2/b = 1. 
#' We're interested in the lower-left quadrant: 
#' y = -sqrt(b(1 - x^2 /a)) for -1 <= x <= 0
#' Actually let us just set a=b=1, and scale the circle as necessary: 
Tmisc()

f <- function(x) {
  return(-sqrt(1-x^2))
}

x <- seq(-1, 0, length=1000)

par(pty='s')
plot(x,f(x), type="l", xaxt='n', yaxt='n', xlab="-I(T;Y)", ylab="I(T;X)", 
     xlim=c(-1,0.2), ylim=c(-1,0.2), xaxs="i",yaxs="i")
# plot(x,f(x), type="l", xlab="-I(T;Y)", ylab="I(T;X)")
S <- c(0.1, 0.2, 0.3, 0.4, 0.5)
for(i in 1:length(S)) {
  # i  <- 1
  s <- S[i]
  
  yscale <- s
  xscale <- s
  xstart <- s
  arcstart <- s
  
  x1 <- seq(-1, -arcstart, length=100)
  ystart <- f(xstart)
  y1 <- f(x1)
  # Scaling
  X1 <- x1 - max(x1)
  Y1 <- y1 - min(y1)
  X1 <- X1 * yscale
  Y1 <- Y1 * xscale
  X1 <- X1 + max(x1)
  Y1 <- Y1 + min(y1)
  lines(X1, Y1)
  points(max(x1), min(y1))
  text(min(X1), max(Y1)+0.05, paste0("|T|=",i+1))
  
}

PDF(file="IBgraph_different_cardinalities.pdf", width=5, height=5)
setwd0("/Users/tshmak/WORK/Projects/Theory/someRtests/")
dev.copy2PDF()


