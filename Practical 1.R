library(nimble)
library(coda)

b0 <- 1
b1 <- 0.5
std.dev <- 0.3
n <- 10
x <- runif(n,0,3)
y <- b0 + b1 * x + rnorm(n, 0, std.dev)

##  BAYESIAN BIT USING NIMBLE  ##

code <- nimbleCode({
  # Decribe the Likelihood Function for the data
  # looping through each datapoint
  for(i in 1:n){
    # that datum is a draw from a Normal distribution with a mean and a precision
    y[i]   ~ dnorm(mu[i], inv.var)
    # the mean is the prediction made by a straight line with intercept and slope
    mu[i] <- beta0 + beta1 * x[i]
  }
  
  # Priors for parameters
  # our prior knowledge of the intercept is very vague...Normal distribution with mean zero and very low precision. Essentially this says the intercept could be any value.
  beta0 ~ dnorm(0, 0.0001)
  # our prior knowledge of the slope is very vague...Normal distribution with mean zero and very low precision. Essentially this says the intercept could be any value.
  beta1 ~ dnorm(0, 0.0001)
  # tell R that the precision is determined by the standard deviation we wish to infer
  inv.var <- 1 / (sigma * sigma)
  # and that standard deviation lacks any prior information esxcept that it must be greater than one and less than twenty. Why did we not choose a Normal distribution for this parameter? Because we can't risk it going negative, which would break the algorithm.
  sigma ~ dunif(0, 20)
}
)

consts <- list(n = n)

data <- list(y = y, x = x)

inits <- list(beta0 = rnorm(1),
              beta1 = rnorm(1),
              sigma = runif(1))

mcmc.out <- nimbleMCMC(code = code, constants = consts, data = data, inits = inits,
                       nchains = 3, niter = 20000, nburnin = 2000, thin = 10, 
                       samplesAsCodaMCMC = TRUE, summary = TRUE,
                       monitors = c("beta0", "beta1", "sigma"))

mcmc.out$summary

plot(mcmc.out$samples)
