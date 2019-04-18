library(glogis)
library(PearsonDS)
library(gsl)

spei_fun <- function(x) {
  #first try log logistic
  tryCatch(
    {
      x = as.numeric(x)
      fit.loglogistic = glogisfit(x)
      fit.cdf = pglogis(x,location = fit.loglogistic$parameters['location'], scale = fit.loglogistic$parameters['scale'], 
                        shape = fit.loglogistic$parameters['shape'], lower.tail = TRUE, log.p = FALSE)
      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)])
    },
    #next try pearson
    error=function(cond) {
      x = as.numeric(x)
      fit.pearson = pearsonFitML(x)

      fit.cdf = ppearson(x, a = fit.pearson$a, b = fit.pearson$b, location = fit.pearson$location,
                         scale = fit.pearson$scale, params = fit.pearson, lower.tail = TRUE, log.p = FALSE)

      standard_norm = qnorm(fit.cdf, mean = 0, sd = 1)
      return(standard_norm[length(standard_norm)])
    },
    #else return NA
    error=function(cond) {
      return(NA)
    })
}

x = as.numeric(integrated_diff[77,])

fitdistrplus::fitdist(x, "lnorm")

spei_fun(x)

spei = current_spei

test = integrated_diff

test$SPEI = spei




test2 = as.numeric(integrated_diff[77,])

dPIII<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)

fitdist(x, distr="lognormal", method="mle")





