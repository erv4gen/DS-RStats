
library("rjags")
dat = read.csv("c:/data/Datasets/Coursera/callers.csv", header=TRUE)
head(dat)

boxplot(calls/days_active  ~ isgroup2,data=dat)
  

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

xd <- -2.56194 + 1.0 * 1.54839 + -0.06542 * 29.0
(lam <- exp(xd))
(yd <-ppois(lambda = lam*30.0,q = 3))
