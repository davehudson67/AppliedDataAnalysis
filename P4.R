y <- v_data$Wealth

mm1 <- model.matrix(~Pop * Occ, data = v_data)

code <- nimbleCode({
  # Likelihood
      for(i in 1:n){
        y[i] ~ dnorm(mu[i], inv.var)
        mu[i] <- beta[1] * x[i, 1] + beta[2] * x[i, 2] + beta[3] * x[i, 3] +  beta[4] * x[i, 4] + beta[5] * x[i, 5] + beta[6] * x[i, 6] + beta[7] * x[i, 7] + beta[8] * x[i, 8]
      }
      
  # Priors for parameters
	    for(j in 1:8){
    	  beta[j] ~ dnorm(0, 0.0001)}
        inv.var <- 1 / (sigma * sigma)
        sigma ~ dunif(0, 20)
      }
  )


inits <- function(){
  list(beta = rnorm(8),sigma = runif(1))
}

data <- list(y = y, x = mm1)

consts <- list(n = length(y))

mcmc.out <- nimbleMCMC(code = code, constants = consts, data = data, inits = inits,
                               nchains = 3, niter = 20000, nburnin = 2000, thin = 10, 
                               samplesAsCodaMCMC = TRUE, summary = TRUE,
                               monitors = c("beta", "sigma"))


mcmc.out$summary


###################################

v_data$Wealth.scale <- as.numeric(scale(v_data$Wealth))
v_data$Pop.scale <- as.numeric(scale(v_data$Pop))
mm1 <- model.matrix(~Occ * Pop.scale, data = v_data)
y <- as.numeric(v_data$Wealth.scale)

code <- nimbleCode({
  # Likelihood
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], inv.var)
    mu[i] <- beta[1] * x[i, 1] + beta[2] * x[i, 2] + beta[3] * x[i, 3] +  beta[4] * x[i, 4] + beta[5] * x[i, 5] + beta[6] * x[i, 6] + beta[7] * x[i, 7] + beta[8] * x[i, 8]
  }
  
  # Priors for parameters
  for(j in 1:8){
    beta[j] ~ dnorm(0, 0.0001)}
  inv.var <- 1 / (sigma * sigma)
  sigma ~ dunif(0, 20)
}
)


inits <- function(){
  list(beta = rnorm(8), sigma = runif(1))
}

data <- list(y = y, x = mm1)

consts <- list(n = length(y))

mcmc.out <- nimbleMCMC(code = code, constants = consts, data = data, inits = inits,
                       nchains = 3, niter = 20000, nburnin = 2000, thin = 10, 
                       samplesAsCodaMCMC = TRUE, summary = TRUE,
                       monitors = c("beta", "sigma"))

mcmc.out$summary$all.chains

oldpar <- par(mar = c(5, 15, 4, 2) + 0.1, mgp = c(10, 1, 0))

m1.MCMC.mat <- as.matrix(mcmc.out$samples)
m1.MCMC.dat <- as.data.frame(m1.MCMC.mat)
m1.quantiles <- apply(m1.MCMC.dat, 2, quantile, probs = c(0.025, 0.5, 0.975))
m1.quantiles
ddd <- barplot(m1.quantiles[2, ], horiz = T, xlim = c(-3, 3), col = "white", border = "white", names.arg = c(colnames(mm1), "sigma"), las=1)
mtext("MCMC effect size", side = 1, line = 3)
mtext("predictor", side = 2, line = 9)
arrows(m1.quantiles[1, ], ddd, m1.quantiles[3, ], ddd, code = 3, angle = 90, length = 0.1)
points(m1.quantiles[2, ], ddd, cex = 1.8, pch = 21, bg = "black")
lines(c(0, 0), c(0, 20), lty = 2, lwd = 2)
par(oldpar)


#######################################

v_data$Wealth.scale <- as.numeric(scale(v_data$Wealth))
v_data$Pop.scale <- as.numeric(scale(v_data$Pop))
mm1 <- model.matrix(~Occ * Pop.scale, data = v_data)
dim(mm1)
y <- as.numeric(v_data$Wealth.scale)

code <- nimbleCode({
  # Likelihood
  for(i in 1:n){
    y[i]   ~ dnorm(mu[i], inv.var)
    mu[i] <- inprod(x[i, 1:8], beta[1:8])
    }
  
  # Priors for parameters
  for(j in 1:8){
    beta[j] ~ dnorm(0, 0.0001)}
    inv.var <- 1 / (sigma * sigma)
    sigma ~ dunif(0, 20)
}
)


inits <- function(){
  list(beta = rnorm(8), sigma = runif(1))
}

data <- list(y = y, x = mm1)

consts <- list(n = length(y))

mcmc.out <- nimbleMCMC(code = code, constants = consts, data = data, inits = inits,
                       nchains = 3, niter = 20000, nburnin = 2000, thin = 10, 
                       samplesAsCodaMCMC = TRUE, summary = TRUE,
                       monitors = c("beta", "sigma"))

mcmc.out$summary$all.chains

dim(mm1)

oldpar <- par(mar = c(5, 15, 4, 2) + 0.1, mgp = c(10, 1, 0))

m1.MCMC.mat <- as.matrix(mcmc.out$samples)
m1.MCMC.dat <- as.data.frame(m1.MCMC.mat)
m1.quantiles <- apply(m1.MCMC.dat, 2, quantile, probs = c(0.025, 0.5, 0.975))
m1.quantiles
ddd <- barplot(m1.quantiles[2, ], horiz = T, xlim = c(-3, 3), col = "white", border = "white", names.arg = c(colnames(mm1), "sigma"), las=1)
mtext("MCMC effect size", side = 1, line = 3)
mtext("predictor", side = 2, line = 9)
arrows(m1.quantiles[1, ], ddd, m1.quantiles[3, ], ddd, code = 3, angle = 90, length = 0.1)
points(m1.quantiles[2, ], ddd, cex = 1.8, pch = 21, bg = "black")
lines(c(0, 0), c(0, 20), lty = 2, lwd = 2)
par(oldpar)
