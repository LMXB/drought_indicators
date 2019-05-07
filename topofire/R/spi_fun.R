spi_fun <- function(x) { 
  #if precip is 0, replace it with 0.5%tile (really dry)
  if(any(x == 0, na.rm = T)){
    index = which(x == 0)
    x[index] = 0.01
  }
  fit.gamma = gamma_fit(x)
  fit.cdf = pgamma(x, shape = fit.gamma$shape, rate = fit.gamma$rate)
  #if precip is 0, replace it with 0.1%tile (really dry)
  # if(any(fit.cdf == 0, na.rm = T)){
  #   index = which(fit.cdf == 0)
  #   fit.cdf[index] = quantile(fit.cdf,0.001)
  # }
  standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
  return(standard_norm[length(standard_norm)]) 
}
