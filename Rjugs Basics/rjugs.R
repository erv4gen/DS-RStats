#specify the model
library("rjags")
library("coda")


mod_string <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu,1.0/sig2)
  }
  mu ~ dt(0.0,1.0/1.0,1)
  sig2 = 1.0
}"
#Set up the model
set.seed(50)

#datapoints1
y <- c(1.2,1.4,-0.5,0.3,0.9,2.3,1.0,1.3,1.9)
#datapoints2
y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)

n = length(y)
data_jags <- list(y=y,n=n)
params <- c("mu")

inits <- function() {
  mu_init = 1.0
  inits <- list("mu" = mu_init)
}

mod = jags.model(textConnection(mod_string),
                 data = data_jags,
                 inits = inits)

#Run the MCMC sample 
#update(mod,500)

mod_sim <-coda.samples(model = mod,
                       variable.names = params,
                       n.iter =5e3)

#Post processing 

plot(mod_sim)
summary(mod_sim)
