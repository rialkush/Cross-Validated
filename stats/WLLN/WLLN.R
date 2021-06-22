# Bernoulli distribution of parameter p=0.7
n <- 100
x <- sample(c(0,1), n, replace=T, prob=c(.3,.7))
par(mfrow=c(2,1))
plot(x, type='h',main="Bernoulli variables, prob=(.3,.7)")
barplot(table(x)/n, ylim = c(0,1))

# Bernoulli distribution of parameter p=0.8 and X =-1 and +1
n <- 1000
x <- sample(c(-1,1), n, replace=T, prob=c(.3,.7))
par(mfrow=c(2,1))
plot(x, type='h', main="Bernoulli variables, prob=(.3,.7)")
barplot(table(x)/n, ylim = c(0,1), main = "Bar plot")

# Cummulative sums  X =-1 and +1 with scaling 1/sqrt(n)
n <- 1000
x1 <- sample(c(-1,1), n, replace=T, prob=c(.3,.7))
x2 <- sample(c(-1,1), n, replace=T, prob=c(.5,.5))
par(mfrow=c(2,1))
plot(cumsum(x1)/sqrt(n), type='l',main="Cummulative sums with  P(X=1)=0.7=1-P(X=-1) ")
plot(cumsum(x2)/sqrt(n), type='l',main="Cummulative sums   P(X=1)=0.5=1-P(X=-1) ")

# Cummulative sums  X =-1 and +1 with scaling 1/(n)
n <- 100
nn<-((1:n))
x1 <- sample(c(-1,1), n, replace=T, prob=c(.3,.7))
x2 <- sample(c(-1,1), n, replace=T, prob=c(.5,.5))
par(mfrow=c(2,1))
plot(cumsum(x1)/(nn), type='l',
     main="(Cummulative sums)/sample size, P(X=1)=0.7=1-P(X=-1)  ")
abline(h=0.4, col=2,lwd=2, lty=3)
plot(cumsum(x2)/(nn), type='l',
     main="(Cummulative sums)/sample size,  P(X=1)=0.5=1-P(X=-1) ")
abline(h=0.0, col=2,lwd=2, lty=3)

