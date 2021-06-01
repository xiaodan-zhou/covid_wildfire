#BSpline Basis Construction
bspline <- function(x, df, bdeg=3, cyclic=FALSE, xl=min(x), xr=max(x)){
  x <- as.matrix(x,ncol=1)
  
  ndx <- df - bdeg
  
  # as outlined in Eilers and Marx (1996)
  dx <- (xr - xl) / ndx
  t <- xl + dx * (-bdeg:(ndx+bdeg))
  S <- (0 * x + 1) %*% t
  X <- x %*% (0 * t + 1)
  P <- (X - S) / dx
  B <- (S <= X) & (X < (S + dx))
  r <- c(2:length(t), 1)
  
  for (k in 1:bdeg)
    B <- (P * B + (k + 1 - P) * B[ ,r]) / k
  
  B <- B[,1:(ndx+bdeg)]
  
  if (cyclic == 1){
    
    for (i in 1:bdeg)
      B[ ,i] <- B[ ,i] + B[ ,df-bdeg+i]    
    
    B <- B[ , 1:(df-bdeg)]
    
  }
  
  return(B)
  
}

# Highest Posterior Density
hpd <- function(x, alpha = 0.05){
  
  n <- length(x)
  m <- round(n * alpha)
  x <- sort(x)
  y <- x[(n - m + 1):n] - x[1:m]
  z <- min(y)
  k <- which(y == z)[1]
  c(x[k], x[n - m + k])
  
}

# custom boxplot limits
boxxy <- function(x) {
  r <- quantile(x, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}

outside <- function(x) {
  subset(x, x < hpd(x)[1] | x > hpd(x)[2])
}
