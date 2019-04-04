if ("glogis" %in% rownames(installed.packages()) == FALSE) {install.packages("glogis")}

library(glogis)

x = rglogis(100, location = -150, scale = 6, shape = 100)

X <- sort(x)

e_cdf <- 1:length(X) / length(X)

dist_param = glogisfit(x)

test = pglogis(x,location = dist_param$parameters['location'], scale = dist_param$parameters['scale'], 
               shape = dist_param$parameters['shape'], lower.tail = TRUE, log.p = FALSE)

fit.cdf = data.frame(x=x, y = test)
#sort for plotting
#fit.cdf = fit.cdf[order(x),]

par(mfrow=c(1,2))

plot(X, e_cdf)
points(fit.cdf$x,fit.cdf$y, col = "red")

#compute standard normal equivelants
standard_norm = qnorm(fit.cdf$y, mean = 0, sd = 1)
plot(standard_norm, fit.cdf$y)
