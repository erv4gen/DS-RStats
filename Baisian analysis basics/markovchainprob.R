	
Qi = matrix(c(0,1),nrow=1)
Q = matrix(c(0,1,0.3,0.7),nrow=2,byrow=TRUE)
Qi %*% Q %*% Q

Q = matrix(c(0.0, 0.5, 0.0, 0.0, 0.5,
             0.5, 0.0, 0.5, 0.0, 0.0,
             0.0, 0.5, 0.0, 0.5, 0.0,
             0.0, 0.0, 0.5, 0.0, 0.5,
             0.5, 0.0, 0.0, 0.5, 0.0), 
           nrow=5, byrow=TRUE)


n = 5000
x = numeric(n)
x[1] = 1 # fix the state as 1 for time 1
for (i in 2:n) {
  x[i] = sample.int(5, size=1, prob=Q[x[i-1],]) # draw the next state from the intergers 1 to 5 with probabilities from the transition matrix Q, based on the previous value of X.
}


table(x) / n


#cont excample

set.seed(38)

n=1500
x = numeric(n)
phi = -0.6
for(i in 2:n) {
x[i] = rnorm(1,mean=phi*x[i-1],sd=1.0)
}
plot.ts(x)
x[4]

hist(x,freq=FALSE)
curve(dnorm(x, mean=0.0, sd=sqrt(1.0/(1.0-phi^2))), col="red", add=TRUE)
legend("topright", legend="theoretical stationary\ndistribution", col="red", lty=1, bty="n")

