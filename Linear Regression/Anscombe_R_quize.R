library("car")  # load the 'car' package
library("rjags")

data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables
attach(Anscombe)
#top urban cities
head(Anscombe[order(-Anscombe$urban),])

#top urban education expencies
head(Anscombe[order(-Anscombe$education),])

lmod1 = lm(education ~ income+young+urban)
summary(lmod1)


plot(lmod1)
plot(resid(lmod1))
plot(predict(lmod1),resid(lmod1))
qqnorm(resid(lmod1))


######mod1
mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
data_jags = as.list(Anscombe)

param = c('sig','b')
inits1 = function() {
    inits = list('b'=rnorm(3,0.0,100.0),'prec'=rgamma(1,1.0,1.0))
} 
mod1 <- jags.model(textConnection(mod_string),
                   data = data_jags,
                   inits = inits1,
                   n.chains = 3
                   )
mod1_sim <- coda.samples(model = mod1,
                         variable.names = param,
                         n.iter=2e3)
mod1_csim <- as.mcmc(do.call(rbind,mod1_sim))

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
traceplot(mod1_sim)
gelman.plot(mod1_sim)

summary(mod1_sim)


######mod2
mod_string2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
data_jags2 = list(education= Anscombe$education,
                     income = Anscombe$income ,
                     young = Anscombe$young)

inits2 = function() {
    inits = list('b'=rnorm(2,0.0,100.0),'prec'=rgamma(1,1.0,1.0))
} 
mod2 <- jags.model(textConnection(mod_string2),
                   data = data_jags2,
                   inits = inits2,
                   n.chains = 3
)
mod2_sim <- coda.samples(model = mod2,
                         variable.names = param,
                         n.iter=2e3)

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
traceplot(mod2_sim)
gelman.plot(mod2_sim)

summary(mod2_sim)




######mod3
mod_string3 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*young[i]*income[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
data_jags3 = list(education= Anscombe$education,
                  income = Anscombe$income ,
                  young = Anscombe$young)

inits3 = function() {
    inits = list('b'=rnorm(3,0.0,100.0),'prec'=rgamma(1,1.0,1.0))
} 
mod3 <- jags.model(textConnection(mod_string3),
                   data = data_jags3,
                   inits = inits3,
                   n.chains = 3
)
mod3_sim <- coda.samples(model = mod3,
                         variable.names = param,
                         n.iter=2e3)

gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
traceplot(mod3_sim)
gelman.plot(mod3_sim)

summary(mod3_sim)





##########find the best model

dic.samples(mod1,n.iter = 1e6)
dic.samples(mod2,n.iter = 1e5)
dic.samples(mod3,n.iter = 1e5)

