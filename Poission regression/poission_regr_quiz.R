X = c(1.0 , 0.8, 1.2)
B = c(1.5, -0.3, 1.0)
loge = X %*% B
e = exp(loge)



mod_string2 = " model {
    #Likelihood
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
    }
    
    #Priors
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags2 = as.list(badhealth)

params2 = c("int", "b_badh", "b_age")

mod2 = jags.model(textConnection(mod_string2), data=data_jags2, n.chains=3)
update(mod2, 1e3)

mod_sim2 = coda.samples(model=mod2,
                       variable.names=params2,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))

## convergence diagnostics
plot(mod_sim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
autocorr.plot(mod_sim2)
effectiveSize(mod_sim2)
raftery.diag(mod_sim2)
## compute DIC
(dic2 = dic.samples(mod2, n.iter=1e3))

ppois(q=21,lambda=15*2)



###############################
dat <- read.csv(file='c:\\data\\Datasets\\callers\\callers.csv',header=TRUE)
boxplot(calls/days_active  ~ isgroup2,data=dat)
head(dat)  

model_string <- " model {
                #Likelihood
                for(i in 1:length(calls)) {
                calls[i] ~ dpois(lam[i] * days_active[i])
                log(lam[i]) = b0 +  isgroup2[i] *b[1] + age[i] *b[2]
                }
                #Priors
                b0 ~ dnorm(0.0,1.0/1e2)
                for(j in 1:2) {
                b[j] ~ dnorm(0.0,1.0/1e2)
                }
}"

param1 <- c("b0",'b')
data_jags <- as.list(dat)
mod1 <- jags.model(textConnection(model_string),data = data_jags,n.chains = 3)
update(mod1,1e3)
mod1_sims <-coda.samples(model = mod1,n.iter = 1e5,variable.names = param1)

plot(mod1_sims)
gelman.diag(mod1_sims)
autocorr.diag(mod1_sims)
effectiveSize(mod1_sims)
raftery.diag(mod1_sims)
summary(mod1_sims)

mod1_csim <- as.mcmc(do.call(rbind,mod1_sims))
mean(mod1_csim[,'b[1]']>0)
    