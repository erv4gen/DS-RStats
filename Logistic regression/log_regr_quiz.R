library("MASS")
library("rjags")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )
boxplot(dat)
mod_glm <- glm(Correct/Trials ~ Age + OME + Loud + Noise,
               data=dat,
               wdeights = Trials,
               family= 'binomial'
               )

resid = residuals(mod_glm,type='deviance')
plot(resid)

plot(fitted(mod_glm), dat$Correct/dat$Trials)

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)


mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "
params = c("b0", "b")
data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).

mod = jags.model(textConnection(mod_string),
                 data=data_jags, 
                 n.chains=3)

update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=3e5)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

summary(mod_csim)

plot(mod_sim, ask=TRUE)


raftery.diag(mod_csim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

dic1 = dic.samples(mod1, n.iter=1e3)

#Using the posterior mean estimates of the model 
#coefficients, create a point estimate of the probability
#of correct responses for a child of age 60 months, 
#with high OME, using a coherent stimulus of 50 decibels. 
#Round your answer to two decimal places.

xd = -7.28246 + 0.01874 *(60) + -0.24327 *(0) + 0.17172 * (50) + 1.57754 *(0)

y = 1.0 / (1+exp(-1.0*xd))

pm_coef <- colMeans(mod_csim)
px_hat = pm_coef['b0'] + X %*% pm_coef[1:4]
p_hat = 1.0 / (1.0 + exp(-1.0*px_hat))

(tab0.7 = table(p_hat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)
