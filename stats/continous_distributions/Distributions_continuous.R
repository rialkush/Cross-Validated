#########################################################
# COUNTINUOUS DISTRIBUTIONS 
#########################################################

##############################
# UNIFORM(a,b)
##############################

a<-0
b<-1
n<- 1000 #Sample size
x1<-runif(n, a, b)
s<-seq(a,b,length=11)
par(mfrow=c(1,1))
hist(x1, col=5,probability = T, breaks =s, main='UNIFORM(0,1)')
lines(dunif(s,a,b)~s, col=2, lwd=2, lty=2)

a<-2.3
b<-5.8

x2<-a+(b-a)*x1
ss<-a+s*(b-a)

par(mfrow=c(3,1))
hist(x1, col=5,probability = T, breaks =s, main='UNIFORM(0,1)')
lines(dunif(s,0,1)~s, col=2, lwd=2, lty=1)

hist(x2, col=7,probability = T, breaks =ss, main='UNIFORM(a,b)')
lines(dunif(ss,a,b)~ss, col=2, lwd=2, lty=2)

plot(dunif(s,0,1)~s, xlim=c(0,b),ylim=c(0,1), col=2, lwd=2, type='l')
lines(dunif(ss,a,b)~ss, col=2, lwd=2, lty=2)

#############################
# EXPONENTIAL (LAMBDA)
#############################

lambda<-.2 #rate
mu=1
n<-1000 #sample size
x1 <- rexp(n, lambda)
s<-seq(0,max(x1),by=0.5)
par(mfrow=c(1,1))
hist(x1,probability = T,breaks = 100, col=8, main='EXPONENTIAL')
lines(dexp(s,lambda)~s, col=2, lwd=3, lty=2)

###############################
# GAMMA(alpha,lambda)
################################

alpha<-3 # shape
lambda<-.2 # rate 

n<-1000 #sample size
x1 <- rgamma(n, shape=alpha, scale=lambda)
s<-seq(0,max(x1),by=0.05)
par(mfrow=c(1,1))
hist(x1,probability = T,breaks = 100, col=8, main='GAMMA(alpha,lambda)')
lines(dgamma(s, shape=alpha, scale=lambda)~s, col=2, lwd=3)

#################################
# NORMAL(mu, sigma)
#################################

mu=0  # location 
sigma=1 # scale

n<-1000
x1<-rnorm(n, mu,sigma)
s<-seq(min(x1),max(x1),by=0.05)

par(mfrow=c(1,1))
hist(x1,probability = T,breaks = 100, col=8, main='NORMAL(0,1)')
lines(dnorm(s, mean=0, sd=1)~s, col=2, lwd=3)

y1<-x1
s1<-s
y2<-2+0.5*x1
s2<-2+0.5*s
y3<- -3+1.5*x1
s3<- -3+1.5*s

par(mfrow=c(3,1))
hist(y1,probability = T,breaks = 100, col=8, xlim=c(-7,4))
lines(dnorm(s1, mean=0, sd=1)~s1, col=2, lwd=3)

hist(y2,probability = T,breaks = 100, col=4, xlim=c(-7,4))
lines(dnorm(s2, mean=2, sd=0.5)~s2, col=2, lwd=3)
hist(y3,probability = T,breaks = 100, col=5, xlim=c(-7,4))
lines(dnorm(s3, mean= -3, sd=1.5)~s3, col=2, lwd=3)

cat("mean(Y1)=", mean(y1), "sd(Y1)=",sd(y1),"\n")
cat("mean(Y2)=", mean(y2), "sd(Y2)=",sd(y2),"\n")
cat("mean(Y3)=", mean(y3), "sd(Y2)=",sd(y3),"\n")

############################
# Chi^2-distribution
############################

df<-5
x1<-rchisq(n = 1000,df=df)
par(mfrow=c(1,1))
s<-seq(0,max(x1),length=100)
hist(x1, probability = T, breaks=100,  ylim=c(0,.2), main="chi squared")
lines(dchisq(s,df)~s, col='red', lwd=2)

par(mfrow=c(1,1))
curve(dchisq(x,1), xlim=c(0,10), ylim=c(0,.6), col='red', lwd=3)
curve(dchisq(x,2), add=T, col='green', lwd=3)
curve(dchisq(x,3), add=T, col='blue', lwd=3)
curve(dchisq(x,5), add=T, col='orange', lwd=3)
abline(h=0,lty=3)
abline(v=0,lty=3)
legend(par('usr')[2], par('usr')[4], xjust=1,
       c('df=1', 'df=2', 'df=3', 'df=5'),
       lwd=3,
       lty=1,
       col=c('red', 'green', 'blue', 'orange')
)
title(main='Chi^2 Distributions')

##################################
# t-DISTRIBUTION
##################################

df<-4
x1<-rt(n = 1000,df=df)
par(mfrow=c(1,1))
s<-seq(min(x1),max(x1),length=100)
hist(x1, probability = T, breaks=100, main="t-DISTRIBUTION" )
lines( dt(s,df )~s, col='red', lwd=2 )

par(mfrow=c(1,1))
curve( dt(x,1), xlim=c(-4,4), ylim=c(0,.4), col='red', lwd=2 )
curve( dt(x,2), add=T, col='blue', lwd=2 )
curve( dt(x,5), add=T, col='green', lwd=2 )
curve( dt(x,10), add=T, col='orange', lwd=2 )
curve( dnorm(x), add=T, lwd=3, lty=3 )
title(main="Student T distributions")
legend(par('usr')[2], par('usr')[4], xjust=1,
       c('df=1', 'df=2', 'df=5', 'df=10', 'N(0,1)'),
       lwd=c(2,2,2,2,2), 
       lty=c(1,1,1,1,3),
       col=c('red', 'blue', 'green', 'orange', par("fg")))


######################################
# Lognormal distribution 
#######################################

mu=3  
sigma=.2

n<-10000

x1<-rnorm(n, mean=mu,sd=sigma)
y1<-exp(x1) # rlnorm(n,mu, sigma)
s<-seq(min(y1),max(y1), length=100)

hist(y1,breaks = 100, probability = T, col=3, main = "Lognormal distribution ")
lines(dlnorm(s,mu,sigma)~s, col=2, lwd=2)
