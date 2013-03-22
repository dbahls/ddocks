# Three questions about glm/glm.nb

# 1.  glm.nb() seems to expect case weights -- when given fractional weights
# as in the attached source file it has trouble optimizing, although
# brute force optimization with glm and family = negative.binomial()
# works.  We are aware that in theta.m[dlm] it is clearly documented that case 
# weights are assumed, but  this is less clear in glm.nb.
# 
# 2.  Also illustrated in the source  file is the asymmetry in the way weights
# influence vcov estimates for glm and glm.nb due to the case weights assumption.
# 
# 3.  Finally,  we have observed that my old NAG/GLIM results were apparently based
# on deviance residuals while the R default is Pearson, this is easily remedied as in
# the example in the source file, but again, the documentation might mention this
# since the difference in the default behaviors of R and GLIM seemed surprising.


## generate some artificial data
set.seed(123)
dat <- data.frame(x = rnorm(50, mean = 7, sd = 2.3),
  m = sample(1:25, 50, replace = TRUE))		  
dat$y <- rnbinom(50, mu = exp(1 + 0.25 * dat$x), size = 1.5)

## fitting quasi-poisson works fine
fm_qp <- glm(y ~ x, data = dat, weights = 1/m, family = quasipoisson)

## 1.  negative binomial has convergence problems
library("MASS")
fm_nb1 <- glm.nb(y ~ x, data = dat, weights = 1/m)

## that cannot be overcome by increasing maxit -- yields error
#fm_nb2 <- glm.nb(y ~ x, data = dat, weights = 1/m, maxit = 1000)

## problem seems to be scaling of the weights 
fm_nb3 <- glm.nb(y ~ x, data = dat, weights = max(m)/m)

## which agrees with glm via brute force optimization
theta2logLik <- function(theta) logLik(glm(y ~ x, data = dat,
  weights = 1/m, family = negative.binomial(theta)))
thetahat <- optimize(theta2logLik, c(0.1, 10), maximum = TRUE)$maximum 
fm_nb4 <- glm(y ~ x, weights = 1/m, 
	family = negative.binomial(theta = thetahat),data = dat) 

## 2.  Then the estimation works fine, but the standard errors are changed
## which seems to be handled differently in glm() and glm.nb():
## In glm.nb() weights are real case-weights so that doubling
## will halve variances whereas doubling has no effect in glm().

fm_qp1 <- glm(y ~ x, data = dat, family = quasipoisson, weights = max(m)/m)
fm_qp2 <- glm(y ~ x, data = dat, family = quasipoisson, weights = max(m)/m * 2)
all.equal(coef(fm_qp1), coef(fm_qp2))
all.equal(vcov(fm_qp1), vcov(fm_qp2))

fm_nb1 <- glm.nb(y ~ x, data = dat, weights = max(m)/m)
fm_nb2 <- glm.nb(y ~ x, data = dat, weights = max(m)/m * 2)
all.equal(coef(fm_nb1), coef(fm_nb2))
all.equal(vcov(fm_nb1), vcov(fm_nb2))


# 3. Dispersion for GLIM was done with deviance residuals
g <- glm(y ~ x,weights = 1/m, family = "quasipoisson",data = dat) 

dispersion <- function(object, type = "deviance")
  sum(residuals(object, type = type)^2)/df.residual(object)

sg1 <- summary(g)
sg2 <- summary(g,dispersion = dispersion(g)) # Agrees with GLIM

