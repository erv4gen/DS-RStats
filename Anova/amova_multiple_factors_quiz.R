library("car")
library('rjags')
data("Anscombe")
head(Anscombe)
?Anscombe

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

data_jags2 = as.list(data.frame(Xc))

mod_string2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dgamma(1.0/2.0,1.0*1.0/2.0)
        mu[i] = b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    for (i in 1:3) {
        b[i] ~ ddexp(1.0, 2.0)
    }
    
} "
param2 = c('b')

mod2 <- jags.model(textConnection(mod_string2),
                   data =data_jags2,
                   n.chains = 3)
mod2_sim <- coda.samples(mod2,variable.names = param2,n.iter = 5e3)
mod2_csim <- as.mcmc(do.call(rbind,mod2_sim))
plot(mod2_sim)
summary(mod2_sim)
