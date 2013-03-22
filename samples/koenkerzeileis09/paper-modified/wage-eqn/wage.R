# Analysis of the Wage Equation Data

# Dispersion for GLIM was done with deviance residuals
dispersion <- function(object, type = "deviance")
  sum(residuals(object, type = type)^2)/df.residual(object)


## Negative Binomial log-likelihood for given theta
theta2logLik <- function(theta) logLik(glm(y ~ log(z), data = d,
  weights = 1/m, family = negative.binomial(theta)))





d <- read.table("data",header=TRUE)

g1 <- glm(y ~ log(z),weights = 1/m, family = "quasipoisson",data = d) 
g2 <- glm(y ~ log(z) + I(log(z)^2),weights = 1/m, family = "quasipoisson",data = d) 
g3 <- glm(y ~ log(z) + I(log(z)^2),weights = 1/m, subset = (z < 300000), 
	family = "quasipoisson",data = d) # Note that JAE paper mentions 500000 cutoff 
g4 <- glm(y ~ log(log(z)), weights = 1/m, family = "quasipoisson",data = d) 
sg1 <- summary (g1,dispersion = dispersion(g1))
sg2 <- summary (g2,dispersion = dispersion(g2))
sg3 <- summary (g3,dispersion = dispersion(g3))
sg4 <- summary (g4,dispersion = dispersion(g4))

# Negative Binomial Model:  Optimize Likelihood by brute force 
require(MASS)
thetahat <- optimize(theta2logLik, c(0.1, 10), maximum = TRUE)$maximum
## -> 1.512412
g5 <- glm(y ~ log(log(z)), weights = 1/m, 
	family = negative.binomial(theta = thetahat),data = d) 
## as the weights seemed to be the problem, try re-scaling to get pseudo-case-weights
fm <- glm.nb(y ~ log(log(z)), data = d, weights = max(m)/m)
gm <- glm.nb(y ~ log(log(z)), data = d, weights = 1/m)
#fm$theta
## -> 1.512429



