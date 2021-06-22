## Exponential distribution

n = 10 # sample size
lambda = 1/5 # parameter
B = 10000 # number of Monte Carlo samples

# Generate B samples of data, each of size n
# Rows hold the samples (10000 rows)
data = matrix(rexp(n*B,lambda),nrow=B,ncol=n)

# Calculate sample mean for each row (sample)
sample.mean = apply(data,1,mean)

# Make histogram of 10000 sample means (one calculated from each row)
# This is the Monte Carlo distribution
hist(sample.mean,xlab="Sample mean",prob=TRUE,
     xlim=c(min(sample.mean),max(sample.mean)),
     ylab="Sampling distribution of the sample mean",
     main="",col="lightblue")

# Overlay normal density to assess the approximation
lines(sort(sample.mean),
      dnorm(sort(sample.mean),mean(sample.mean),sd(sample.mean)),
      col="red",lwd=2)

##########################################################################

## ? distribution

n = 10 # sample size
lambda = 1/5 # parameter
B = 10000 # number of Monte Carlo samples

# Generate B samples of data, each of size n
# Rows hold the samples (10000 rows)
data = matrix(rexp(n*B,lambda),nrow=B,ncol=n)

# Calculate sample mean for each row (sample)
sample.mean = apply(data,1,mean)

# Make histogram of 10000 sample means (one calculated from each row)
# This is the Monte Carlo distribution
hist(sample.mean,xlab="Sample mean",prob=TRUE,
     xlim=c(min(sample.mean),max(sample.mean)),
     ylab="Sampling distribution of the sample mean",
     main="",col="lightblue")

# Overlay normal density to assess the approximation
lines(sort(sample.mean),
      dnorm(sort(sample.mean),mean(sample.mean),sd(sample.mean)),
      col="red",lwd=2)

##########################################################################
