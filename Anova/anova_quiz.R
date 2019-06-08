data('PlantGrowth')
library("rjags")
?PlantGrowth
head(PlantGrowth)
boxplot(weight ~ group, data=PlantGrowth)
lmod = lm(weight ~ group, data = PlantGrowth )
summary(lmod)
anova(lmod)
plot(lmod)

########Mod 2
mod_string2 = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    for (k in 1:3) {
        prec[k] ~ dgamma(5/2.0, 5*1.0/2.0)
    }
    
    sig = sqrt( 1.0 / prec )
} "


set.seed(82)
str(PlantGrowth)
data_jags2 = list(y=PlantGrowth$weight, 
                 grp=as.numeric(PlantGrowth$group))

params2 = c("mu", "sig")

inits2 = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod2 = jags.model(textConnection(mod_string2), data=data_jags2, inits=inits2, n.chains=3)
update(mod2, 1e3)

mod_sim2 = coda.samples(model=mod2,
                       variable.names=params2,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2)) # combined chains

plot(mod_sim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
effectiveSize(mod_sim2)

(pm_params2 = colMeans(mod_csim2))
coefficients(lmod)


yhat2 = pm_params2[1:3][data_jags$grp]
resid2 = data_jags$y - yhat2
plot(resid2)
plot(yhat2, resid2)

summary(mod_sim2)

HPDinterval(mod_csim2)

#is treatment 2 is higher than control group
mean(mod_csim2[,3] > mod_csim2[,1])

#what is the probability that treatment 1 increase growth by 10%
mean(mod_csim2[,3] > 1.1*mod_csim2[,1])

dict2 <-dic.samples(model = mod2,n.iter = 1e6)

