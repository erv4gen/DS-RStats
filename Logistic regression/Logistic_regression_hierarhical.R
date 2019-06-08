library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
  #Likelihood
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	#Priors
	for (k in 1:max(ID)) {
	a[k] ~ dnorm(mu, tao_sq)
	}
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	mu ~dnorm(0.0,1.0/1e3)
	tao_sq ~dgamma(1.0/2.0,1.0/2.0)
	
} "
param <- c('a','b')
data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID

mod <- jags.model(textConnection(mod_string),
                  data = data_jags,
                  n.chains = 3,
                  n.adapt = 1e3)

mod_sim <- coda.samples(mod,variable.names = param,n.iter = 5e3)
summary(mod_sim)
plot(mod_sim,ask = TRUE)
mod_csim <-as.mcmc(do.call(rbind,mod_sim))
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
raftery.diag(mod_sim)
effectiveSize(mod_sim)
(dic <-dic.samples(mod,n.iter = 1e3))
