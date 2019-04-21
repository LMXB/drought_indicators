library(glogis)
library(PearsonDS)
library(gsl)
library(lmomco)
library(actuar)

x = as.numeric(integrated_diff[74,])

x = as.numeric(x)
#fit lmoments
lmoments_x = lmoms(x)
#fit pearson type 3
fit.parglo = parglo(lmoments_x)
#fit.parlogglo = c(exp(fit.parglo$para[1]), 1/fit.parglo$para[2])
#compute probabilistic cdf 
fit.cdf = cdfglo(x, fit.parglo)
#compute standard normal equivelant
standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)


#New way
x = as.numeric(x)
#Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
pwm = pwm.ub(x)
#Probability-Weighted Moments to L-moments
lmoments_x = pwm2lmom(pwm)
#fit generalized logistic
fit.parglo = parglo(lmoments_x)

#test to make sure the packages compute the same reults
fortran_vec = c(lmoments_x$lambdas[1:2], lmoments_x$ratios[3])
test = lmom::pelglo(fortran_vec)

#compute probabilistic cdf 
fit.cdf = cdfglo(x, fit.parglo)
#compute standard normal equivelant
standard_norm2 = qnorm(fit.cdf, mean = 0, sd = 1)



spei_fun <- function(x) {
  #first try log logistic
  tryCatch(
    {
      x = as.numeric(x)
      #Unbiased Sample Probability-Weighted Moments (following Beguer ́ıa et al 2014)
      pwm = pwm.ub(x)
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
    #next try generalized logistic
    error=function(cond) {
      x = as.numeric(x)
      fit.loglogistic = glogisfit(x)
      fit.cdf = pglogis(x,location = fit.loglogistic$parameters['location'], scale = fit.loglogistic$parameters['scale'], 
                        shape = fit.loglogistic$parameters['shape'], lower.tail = TRUE, log.p = FALSE)
      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)])
    },
    #next try pearson type 3
    error=function(cond) {
      x = as.numeric(x)
      #fit lmoments
      lmoments_x = lmoms(x)
      #fit pearson type 3
      fit.pearson = parpe3(lmoments_x)
      #compute probabilistic cdf 
      fit.cdf = cdfpe3(x, fit.pearson)
      #compute standard normal equivelant
      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)])
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}


spei_fun(x)

spei = current_spei

test = integrated_diff

test$SPEI = spei




test2 = as.numeric(integrated_diff[77,])

dPIII<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)

fitdist(x, distr="lognormal", method="mle")





