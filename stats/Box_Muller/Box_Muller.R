##################################
#Box-Muller method
##################################
set.seed(123)
n<-9000
u1<- runif(n,min = 0,max = 1)
u2<- runif(n,min = 0,max = 1)
x<-sqrt(-2*log(u1))*cos(2*pi*u2)
y<-sqrt(-2*log(u1))*sin(2*pi*u2)
par(mfrow=c(2,2))
plot(u2~u1,xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), type="p", pch = 20, cex = 0.1, col=4)
plot(y~x, xlim=c(-3.5,3.5), ylim=c(-3.5,3.5), type="p",  pch = 20, cex = 0.2, col=6)
plot(density(x), xlim=c(-4.5,4.5),main = "", xlab = "x")
plot(density(y), xlim=c(-4.5,4.5),main = "", xlab= "y")