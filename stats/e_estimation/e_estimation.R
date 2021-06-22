# e estimation 

a1<-1
b1<-2
a2<-0
b2<-1
itrn<- 10000
x<-runif(itrn,a1,b1)
y<-runif(itrn,a2,b2)
e_true<-rep(exp(1),itrn)
e_hat<-array(0,dim=c(itrn))
count<-array(0,dim=c(itrn))

xx<-seq(a1,b1,by=0.01)
for(i in 1 : itrn){
  count [i]<- (x[i]*y[i]<1)
  area<-(sum(count)/i)
  e_hat[i]<- 2^(1/area)
  
}
plot(e_hat, type = 'l')
lines(e_true, col=2)
s0<- which (count==0)
s1<- which (count==1)
plot(y[s1]~x[s1], col=2, pch = 20, cex = 0.5, xlab="X", ylab = "Y")
lines (y[s0]~x[s0], type='p', pch = 20, cex = 0.5)
lines((1/(xx))~xx , col=3, lwd=4)
