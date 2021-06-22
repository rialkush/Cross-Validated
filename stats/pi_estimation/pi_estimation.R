## Pi estimation
## Range of X 
a1<-0
b1<-1
## Range of Y
a2<-0
b2<-1
itrn<- 10000 # iteration number 
x<-runif(itrn,a1,b1) # generate X
y<-runif(itrn,a2,b2) # generate Y
pi_true<-rep(pi,itrn) # True value of pi
pi_hat<-array(0,dim=c(itrn))
count<-array(0,dim=c(itrn))
xx<-seq(a1,b1,by=0.01) # Sequence on [0,1]

for(i in 1 : itrn){
  count [i]<- (x[i]^2+y[i]^2<1) # (X,Y) in circle or not 
  pi_hat[i]<-4*(sum(count)/i) # How many in cirecle amonge 'i' trials 
}

# Plot
plot(pi_hat, type = 'l')
lines(pi_true, col=2)

s0<- which (count==0) # points out of circle 
s1<- which (count==1) # points in circle 
plot(y[s0]~x[s0], pch = 20, cex = 0.5, xlab="X", ylab = "Y")
lines (y[s1]~x[s1], col=2, type='p', pch = 20, cex = 0.5)
lines(sqrt(1-(xx)^2)~xx , col=3, lwd=4) # equation of circle 
