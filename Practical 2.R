library(nimble)
library(coda)

b0 <- 1
b1 <- 0.5
std.dev <- 0.3
n <- 10
x <- runif(n,0,3)
y <- b0 + b1 * x + rnorm(n, 0, std.dev)


code <- nimbleCode({
  # Likelihood
  #loop through the datapoints
  for(i in 1:n){
    #each datum is a draw from a Normal distribution with expected value mu and precision inv.var
    y[i] ~ dnorm(mu[i], inv.var)
    #the expected value is determined by an intercept, a slope and the value of the explanatory variable (a linear model)
    mu[i] <- beta0 + beta1 * x[i]
  }
  
  # Priors for parameters
  # we provide (effectively) no information about the intercept and slope, hence very broad, flat Normal distributions
  beta0 ~ dnorm(0, 0.0001)
  beta1 ~ dnorm(0, 0.0001)
  #precision is the inverse of the square of the residual standard deviation
  inv.var <- 1 / (sigma * sigma)
  #and we claim that the residual std dev lies somewhere between zero and twenty. If it turns out to be bigger than 20, the inference will not find it, so the Uniform distribution is a bit risky. However our prior distribution for the std deviation  MUST NOT allow negative values, or we will break everything. 
  sigma ~ dunif(0, 20)
})

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

#make the posterior coda chains available for analysis
m1.MCMC.mat <- as.matrix(mcmc.out$samples)
m1.MCMC.dat <- as.data.frame(m1.MCMC.mat)

#and now here's the endgame...what proportion of the slopes were less than or equal to zero?
p.MCMC <- length(m1.MCMC.dat$beta1[m1.MCMC.dat$beta1 <= 0]) / length(m1.MCMC.dat$beta1)
print(round(p.MCMC,4))
