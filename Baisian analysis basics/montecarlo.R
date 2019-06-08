set.seed(32)


#exp 1##########################
m = 100000
a = 2.0
b = 1.0/3.0

theta = rgamma(n=m,shape=a,rate=b)

hist(theta,freq=F)
curve(dgamma(x,shape=a,rate=b),col="blue",add=TRUE)

sum(theta/m)
mean(theta) #mean of the simulations
a/b #mean of the true value

var(theta) #sample variance of theta

a/b^2 #true variance 

#how many values is less than five in the sample
mean(theta<5.0)

#how many of thous are actually true
pgamma(q=5.0,shape =a,rate =b)

#standart error for that variance 
se = sd(theta<5.0) / sqrt(m)

#show that values
head(theta[theta<5.0])


#show 90% quantile of the simulation
quantile(theta,probs=0.90)

#show the true .9 quantile 
qgamma(p=0.9,shape=a,rate=b)

#standart error
sqrt(var(theta)/m)
#or
stderr = sd(theta)/sqrt(m)

#confidence interval 
2.0 * stderr
lower_edge = mean(theta) - 2.0 *stderr
higher_edge = mean(theta) + 2.0 *stderr


#exp 2##########################
#values foloved Bin(10,phi)
#phi follows Beta(2,2)
m=1e5

#for loop code
y= numeric(m)
phi = numeric(m)

for(i in 1:m) {
phi[i] = rbeta(1,2,2)
y[i] = rbinom(1,size=10,prob=phi[i]) }

#vectorize code

phi = rbeta(n=m,2,2)
y = rbinom(n=m,size = 10, prob = phi)

hist(phi,freq=F)
curve(dbeta(x,2,2),add=TRUE,col="blue")

#how many success by event
table(y)

# probability of success by event
table(y) / m


