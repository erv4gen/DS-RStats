library("COUNT")
library("rjags")
data("badhealth")
?badhealth
head(badhealth)
any(is.na(badhealth))
hist(badhealth$numvisit, breaks=20)
min(badhealth$numvisit)
sum(badhealth$numvisit==0)

plot(jitter(log(numvisit+ 0.1)) ~ jitter(age), 
     data=badhealth,
     subset=badh==0,
     xlab="age", 
     ylab="log(visits)")

points(jitter(log(numvisit + 0.1)) ~ jitter(age),
       data=badhealth, 
       subset=badh==1, 
       col="red")


mod_string = " model {
    #Likelihood
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    #Priors
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

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
dic = dic.samples(mod, n.iter=1e3)


#residuals
X = as.matrix(badhealth[,-1])
X = cbind(X, with(badhealth, badh*age))
head(X)
(pmed_coef = apply(mod_csim, 2, median))

llam_hat = pmed_coef["int"] + X %*% pmed_coef[c("b_badh", "b_age", "b_intx")]
lam_hat = exp(llam_hat)

hist(lam_hat)
resid = badhealth$numvisit - lam_hat
plot(resid)

plot(lam_hat, badhealth$numvisit)
abline(0.0, 1.0)


plot(lam_hat[which(badhealth$badh==0)], resid[which(badhealth$badh==0)], xlim=c(0, 8), ylab="residuals", xlab=expression(hat(lambda)), ylim=range(resid))
points(lam_hat[which(badhealth$badh==1)], resid[which(badhealth$badh==1)], col="red")

var(resid[which(badhealth$badh==0)])
var(resid[which(badhealth$badh==1)])



summary(mod_sim)



#we have two persons 35 years old. Second one has a bad helth. What the probability 
#that the second one gonna visit a doctor more often

x1 <- c(0,35,0)
x2 <- c(1,35,35)

loglam1 <- mod_csim[,'int'] + mod_csim[,c(2,1,3)] %*% x1
loglam2 <- mod_csim[,'int'] + mod_csim[,c(2,1,3)] %*% x2

lam1 <- exp(loglam1)
lam2 <- exp(loglam2)

plot(density(lam1))
plot(density(lam2))

n_sim <- length(lam1)
y1 = rpois(n=n_sim, lambda=lam1)
y2 = rpois(n=n_sim, lambda=lam2)

plot(table(factor(y1, levels=0:18))/n_sim, pch=2, ylab="posterior prob.", xlab="visits")
points(table(y2+0.1)/n_sim, col="red")
mean(y2>y1)     


##############mod2

mod2_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/0.2^2)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/0.01^2)
} "

mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

curve(dnorm(x, mean=0.0, sd=sqrt(1e4)), from=-3.0, to=3.0, ylim=c(0.0, 3.0), lty=2,
      main="b_badh", ylab="density", xlab="b_badh")
curve(dnorm(x, mean=0.0, sd=0.2), from=-3.0, to=3.0, col="red", lty=2, add=TRUE)
lines(density(mod_csim[,"b_badh"]))
lines(density(mod2_csim[,"b_badh"]), col="red")
legend("topleft", legend=c("noninformative prior", "posterior", "skeptical prior", "posterior"),
       lty=c(2,1,2,1), col=rep(c("black", "red"), each=2), bty="n")



curve(dnorm(x, mean=0.0, sd=sqrt(1e4)), from=-0.05, to=0.05, ylim=c(0.0, 140.0), lty=2,
      main="b_intx", ylab="density", xlab="b_intx")
curve(dnorm(x, mean=0.0, sd=0.01), from=-0.05, to=0.05, col="red", lty=2, add=TRUE)
lines(density(mod_csim[,"b_intx"]))
lines(density(mod2_csim[,"b_intx"]), col="red")
legend("topleft", legend=c("noninformative prior", "posterior", "skeptical prior", "posterior"),
       lty=c(2,1,2,1), col=rep(c("black", "red"), each=2), bty="n")

mean(mod2_csim[,"b_intx"] > 0) 
