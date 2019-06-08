library("car")
library('coda')
data("Leinhardt")
library("rjags")

#?Leinhardt
head(Leinhardt)


str(Leinhardt)

pairs(Leinhardt)

plot(infant ~ income, data=Leinhardt)


hist(Leinhardt$infant)

hist(Leinhardt$income)


Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data=Leinhardt)


lmod = lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)


dat = na.omit(Leinhardt)




mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i]
    }
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "





#########mod1
set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat), 
                  log_income=dat$logincome)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), 
                  data=data1_jags,
                  inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)


mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains



plot(mod1_sim)

gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)

summary(mod_sim)
summary(lmod)


#########mod2

mod2_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "


data2_jags = list(y=dat$loginfant, n=nrow(dat), 
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=='yes'))

params2 = c("b", "sig")


inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), 
                  data=data2_jags,
                  inits=inits2, n.chains=3)
update(mod2, 1000) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5000)


mod2_csim = do.call(rbind, mod2_sim) # combine multiple chains


plot(mod2_sim)
  
gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)
effectiveSize(mod2_sim)

summary(mod2_sim)
summary(lmod)




#########mod3

mod3_string = " model {
    for (i in 1:n) {
        y[i] ~ dt(mu[i],tau, df)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    df = 2.0 + nu
    nu ~dexp(1.0) 
    tau ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt(1.0*tau*df/(df -2.0))
} "

data3_jags = list(y=dat$loginfant, n=nrow(dat), 
                  log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=='yes'))

params3 = c("b", "sig")


inits3 = function() {
  inits = list("b"=rnorm(3,0.0,100.0),
               "tau"=rgamma(1,1.0,1.0),
               "nu"=rexp(1,1))
}

mod3 = jags.model(textConnection(mod3_string), 
                  data=data3_jags,
                  inits=inits3, n.chains=3)
update(mod3, 1000) # burn-in

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5000)


mod3_csim = do.call(rbind, mod3_sim) # combine multiple chains


plot(mod3_sim)

gelman.diag(mod3_sim)
autocorr.diag(mod3_sim)
effectiveSize(mod3_sim)

summary(mod3_sim)
summary(lmod)





#####################


#Residuals

#BAD residuals
lmod0 = lm(infant ~ income ,data=Leinhardt)
plot(resid(lmod0))
plot(predict(lmod0),resid(lmod0))
#theoretical quantile of the normal distribution against sample quantiles of the resiguals on y axis. 
# if the residual cames from the normal distribution, we'll see the straight line 
qqnorm(resid(lmod0)) 

#GOD residuals mod1
X= cbind(rep(1.0,data1_jags$n),data1_jags$log_income)

pm_params1 = colMeans(mod1_csim)
yhat1 = drop(X %*% pm_params1[1:2])
resid1 = data1_jags$y - yhat1


head(rownames(df)[order(resid1,decreasing = TRUE)])
sd(resid1)


#GOD residuals mod2
X2= cbind(rep(1.0,data2_jags$n),data2_jags$log_income,data2_jags$is_oil)

pm_params2 = colMeans(mod2_csim)
yhat2 = drop(X2 %*% pm_params2[1:3])
resid2 = data2_jags$y - yhat2



head(rownames(df)[order(resid2,decreasing = TRUE)])
sd(resid2)


#GOD residuals mod3
X3= cbind(rep(1.0,data3_jags$n),data3_jags$log_income,data3_jags$is_oil)

pm_params3 = colMeans(mod3_csim)
yhat3 = drop(X3 %*% pm_params3[1:3])
resid3 = data3_jags$y - yhat3
sd(resid3)

#compare both
par(mfrow=c(3,1))
plot(resid1)
plot(resid2)
plot(resid3)

plot(yhat1,resid1)
plot(yhat2,resid2)
plot(yhat3,resid3)

qqnorm(resid1)
qqnorm(resid2)


###############
dic.samples(mod1,n.iter = 1e3)
dic.samples(mod2,n.iter = 1e3)
dic.samples(mod3,n.iter = 1e3)
