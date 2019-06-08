data('PlantGrowth')
library("rjags")
?PlantGrowth
head(PlantGrowth)
boxplot(weight ~ group, data=PlantGrowth)
lmod = lm(weight ~ group, data = PlantGrowth )
summary(lmod)
anova(lmod)
plot(lmod)

########Mod 1
mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "


set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

(pm_params = colMeans(mod_csim))
coefficients(lmod)


yhat = pm_params[1:3][data_jags$grp]
resid = data_jags$y - yhat
plot(resid)
plot(yhat, resid)

summary(mod_sim)

hpd1 = HPDinterval(mod_csim)

dif1 <- mod_csim[,3] - mod_csim[,1]
dif1_interval <-HPDinterval(dif1)

#is treatment 2 is higher than control group
mean(mod_csim[,3] > mod_csim[,1])

#what is the probability that treatment 1 increase growth by 10%
mean(mod_csim[,3] > 1.1*mod_csim[,1])



dict1 <- dic.samples(model = mod,n.iter = 1e6)

#
hpd1[3,] - hpd1[1,]


#lin model with a baseline mean
mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)

