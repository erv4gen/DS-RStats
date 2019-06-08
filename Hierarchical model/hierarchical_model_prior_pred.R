library('rjags')

dat <- read.table(file='c:/data/Datasets/Coursera/cookies.dat',header = TRUE)
head(dat)
table(dat$location)
boxplot(chips ~ location , data = dat)

set.seed(112)

n_samp <-500

alpha_pri <- rexp(n_samp,rate=1.0/2.0)
beta_pri <- rexp(n_samp, rate = 5.0)

mu_pri <- alpha_pri/beta_pri
sig_pri <- sqrt(alpha_pri / beta_pri^2)
summary(mu_pri)
summary(sig_pri)

lam_pri <- rgamma(n_samp,shape = alpha_pri,rate = beta_pri)
summary(lam_pri)

lam_pri <- lam_pri[1:5]

y_pri <- rpois(150,lambda = rep(lam_pri,each=30) )
summary(y_pri)

#####baisian model

model_string <- ' model {
    
    #Likelihood
    for(i in 1:length(chips)) {
    chips[i] ~ dpois(lam[location[i]])
    }
    
    #Priors
    for(j in 1:max(location)) {
    lam[j] ~ dgamma(alpha,beta)
    }
    mu ~ dgamma(2.0,1.0/5.0)
    sig ~ dexp(1.0)
    
    alpha = mu^2 / sig^2
    beta = mu / sig^2
}'

set.seed(113)

data_jags <- as.list(dat)
param <- c('lam','mu','sig')

mod <- jags.model(textConnection(model_string),
                  data = data_jags,
                  n.chains = 3)
#bur-in
update(mod,1e3)

mod_sim <- coda.samples(model = mod,
                        variable.names = param,
                        n.iter = 5e3)
mod_csim <- as.mcmc(do.call(rbind,mod_sim))
plot(mod_sim,ask=TRUE)

autocorr.diag(mod_sim)
raftery.diag(mod_sim)

(dic1 <- dic.samples(model = mod,n.iter = 5e3))

#Model Checking
#obsvation res
(pm_param <-colMeans(mod_csim))

yhat <- rep(pm_param[1:5],each=30)

resid <- dat$chips - yhat
plot(resid)
plot(jitter(yhat),resid)
var(resid[yhat<7])

var(resid[yhat>11])

#location residuals. how the group's mean different from
#overall mean

lam_res <- pm_param[1:5] - pm_param['mu']
plot(lam_res)
abline(h=0,lty=2)

summary(mod_sim)


######################################
#######posterior predictive simulation

n_sim <-nrow(mod_csim)
post_alpha <- mod_csim[,'mu'] ^2 / mod_csim[,'sig'] ^2
post_beta <- mod_csim[,'mu'] / mod_csim[,'sig'] ^2
lam_pred = rgamma(n_sim,shape = post_alpha, rate = post_beta)
hist(lam_pred)

#what the prob that lamd (mean) in the new place will be greater than 15
mean(lam_pred >15)

#predict posterial samples
y_pred <- rpois(n_sim,lambda = lam_pred)

hist(y_pred)
#what the prob that y will be more than 15
mean(y_pred > 15)

#predict amount of cookies for location 1
y_pred1 <- rpois(n_sim, lambda = mod_csim[,'lam[1]'])
hist(y_pred1)
mean(y_pred1 <15)
