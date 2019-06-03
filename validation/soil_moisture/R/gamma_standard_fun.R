gamma_standard_fun <- function(x) { 
  #if soil moisture is 0, replace it with really dry
  if(any(x == 0, na.rm = T)){
    index = which(x == 0)
    x[index] = 0.01
  }
  fit.gamma = gamma_fit(x)
  fit.cdf = pgamma(x, shape = fit.gamma$shape, rate = fit.gamma$rate)
  standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
  return(standard_norm) 
}