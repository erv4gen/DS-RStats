library('coda')


#####Functions
lg <- function(mu,n,ybar) {
	mu2 <- mu^2
	return(n * (ybar *mu - mu2/2) - log(1.0 +mu2))
}

mh <- function(n,ybar,n_iter, mu_init,cand_sd) {
	mu_out = numeric(n_iter)
	accept = 0
	mu_now = mu_init
	lg_now = lg(mu=mu_now,n=n,ybar)
	
	for(i in 1:n_iter) {
		mu_cand = rnorm(1,mean=mu_now,sd=cand_sd)	
		lg_cand = lg(mu=mu_cand,n=n,ybar=ybar)
		
		lalpha = lg_cand - lg_now
		alpha = exp(lalpha)

		u = runif(1)
		if(u <alpha) {
			mu_now = mu_cand
			accept = accept +1
			lg_now = lg_cand
		}
		mu_out[i] = mu_now
	
	}
	list(mu=mu_out,accept=accept/n_iter)
	
}


####datasets 
#datapoints1
y <- c(1.2,1.4,0.5,0.3,1.3,2.3,1.0,1.3,1.2,.2,1.4,0.5,0.7,0.9,2.3,1.4,1.2,1.9,2.1,1.9,1.3,1.3,.2,1.5)
#datapoints2
y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)

#datapoints3
y = c (-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
############


#Show the initial data points and prior proba
ybar <-mean(y)
n <-length(y)

hist(y,freq=FALSE,xlim = c(-1,4))   
points(y,rep(0,n))
points(ybar,0.0,pch=19)
curve(dt(x,df=1),lty=2,add=TRUE)

#########################
#post sampling
set.seed(43)
sd_div = 0.97
sd_div = 1.5
mu_init = 0.0
post <- mh(n=n,ybar=ybar,n_iter=1e4,mu_init=mu_init,cand_sd=sd_div)
traceplot(as.mcmc(post$mu))
str(post)

#post analysis
post$mu_keep <- post$mu[-c(1:100)]
plot(density(post$mu_keep))
plot(density(post$mu_keep),xlim=c(0,2))
points(y,rep(0,n),add=TRUE)
points(ybar,0.0,pch=19,add=TRUE)
curve(dt(x,df=1),lty=2,add=TRUE)

#auticorrelation analysis
autocorr.plot(as.mcmc(post$mu),lag.max = 100)
autocorr.diag(as.mcmc(post$mu))
effectiveSize(as.mcmc(post$mu))

thin_interval <- 60
thin_index <- seq(from=60,1e3,by=60)
head(thin_index)

par(mfrow=c(2,1))
traceplot(as.mcmc(post$mu_keep))
autocorr.plot(as.mcmc(post$mu_keep[thin_index]))

raftery.diag(as.mcmc(post$mu_keep))




####multiple chains
set.seed(61)
nsim = 500

post1 <- mh(n=n,ybar=ybar,n_iter=nsim,mu_init=15.0,cand_sd=0.4)
post1$accept

post2 <- mh(n=n,ybar=ybar,n_iter=nsim,mu_init=5.0,cand_sd=0.4)
post2$accept

post3 <- mh(n=n,ybar=ybar,n_iter=nsim,mu_init=7.0,cand_sd=0.1)
post3$accept

post4 <- mh(n=n,ybar=ybar,n_iter=nsim,mu_init=23.0,cand_sd=0.5)
post4$accept

post5 <- mh(n=n,ybar=ybar,n_iter=nsim,mu_init=17.0,cand_sd=0.4)
post5$accept

pmc <- mcmc.list(as.mcmc(post1$mu),
                 as.mcmc(post2$mu),
                 as.mcmc(post3$mu),
                 as.mcmc(post4$mu),
                 as.mcmc(post5$mu))
coda::traceplot(pmc)
coda::gelman.diag(pmc)
coda::gelman.plot(pmc)

#when we got efficient samples, we can calculate statistics 
# directly from the distribution
summary(as.mcmc(post$mu_keep))
#posterial proba that m greater than 1
mean(post$mu_keep >1)
