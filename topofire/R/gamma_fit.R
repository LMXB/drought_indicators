#fits a gamma distrbution to a vector
#returns the shape and rate parameters

gamma_fit <- function(p) {
  gamma_ <- data.frame()
  q <- p
  q <- q[!is.na(q)]
  
  pzero <- sum(q==0) / length(q)
  
  avg <- mean(q[q > 0.0])
  
  alpha <- 0.0
  beta  <- avg
  gamm  <- 1.0
  
  pgz <- length(q[q > 0.0])
  
  if ( pgz >= 1) {
    alpha <- log(avg) - sum(log(q[q > 0.0])) / pgz 
    gamm <- (1.0 + sqrt(1.0 + 4.0 * alpha / 3.0)) / (4.0 * alpha)
    beta  <- avg / gamm
  } 
  gamma_ <- list(shape=gamm, rate= (1/beta))
  
  return(gamma_)
}