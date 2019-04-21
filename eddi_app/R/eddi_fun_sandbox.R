x = integrated_pet[77,]

eddi_fun <- function(x) {
  #first try log logistic
  tryCatch(
    {
      x = as.numeric(x)
      #Plotting-Position Sample Probability-Weighted Moments (following Hobbins et al., 2016)
      pwm = pwm.pp(x, A = -0.33, B = 0.33)
      #Probability-Weighted Moments to L-moments
      lmoments_x = pwm2lmom(pwm)
      #fit generalized logistic
      fit.parglo = parglo(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfglo(x, fit.parglo)
      #compute standard normal equivelant
      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)])
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}

test = rank(-x)

prob = (test - 0.33)/(length(test + 0.33))

W = sqrt(-2*log(prob))

EDDI = W - ()
