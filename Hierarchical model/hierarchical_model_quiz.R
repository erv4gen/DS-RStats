library('rjags')
library('coda')

dat = read.csv(file="c:/data/Datasets/Coursera/pctgrowth.csv", header=TRUE)
head(dat)



mod_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(theta[grp[i]], prec)
    }
    
    for (j in 1:max(grp)) {
        theta[j] ~ dnorm(mu, r_sq)
    }
    mu ~ dnorm(0.0,1.0/1e6)
    r_sq ~ dgamma(1.0/2.0,1.0*3.0/2.0)
    prec ~ dgamma(2.0/2.0, 1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

set.seed(102)

data_jags = as.list(dat)

params = c("theta")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)
raftery.diag(mod_sim)
## compute DIC
(dic = dic.samples(mod, n.iter=1e3))

head(mod_csim)

(means_theta <- colMeans(mod_csim))


####comparing to anova model
(means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean))
plot(means_anova)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.
