x = rgamma(50, 5, 1)

source("/home/zhoylman/drought_indicators/spi_app/R/gamma_fit.R")

# emperical version
gamma_fit = function(p) {
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

#l-moment version.
gamma_fit_2 = function(x){
  x = as.numeric(x)
  #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
  pwm = lmomco::pwm.ub(x)
  #Probability-Weighted Moments to L-moments
  lmoments_x = lmomco::pwm2lmom(pwm)
  #fit generalized logistic
  fit.pelgam = lmomco::pargam(lmoments_x)
  #compute probabilistic cdf using lmomco
  fit.cdf = lmomco::cdfgam(x, fit.pelgam)
  #return just distrobution parameters
  # fit.params = list(shape = as.numeric(fit.pelgam$para[1]),
  #                   rate = as.numeric(fit.pelgam$para[2]))
  return(fit.cdf)
}

fit.gamma = gamma_fit(x)
fit.cdf = pgamma(x, shape = fit.gamma$shape, rate = fit.gamma$rate)
plot(x, fit.cdf)

####
fit.cdf2 = gamma_fit_2(x)
plot(x, fit.cdf2)

#same
plot(fit.cdf,fit.cdf2)
abline(0,1)

#for spi
plot(qnorm(fit.cdf, mean = 0, sd = 1), qnorm(fit.cdf2, mean = 0, sd = 1))
abline(0,1)
