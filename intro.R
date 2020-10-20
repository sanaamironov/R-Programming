#Vectors
x <- 1:100

#Another way
x = c(1,2,3,4)
#Vectors could be added subtracted multiplied and divided
y = c(7,11, 16, 18)
z= x^y
length(z)
#Factor
q2 = c("Apple", "Banana", "Strawberry", "Blueberry")
q2Factor = as.factor(q2)
as.numeric(q2Factor)
mean(y)
#Data Frame
theDF = data.frame(x,y,q2Factor)
# The folder from where you want to read files, note / and not \
setwd("C:/Users/Sanaa Mironov/Documents/UMBC/CMSC_491/R-Examples")
myData = read.table("ElecMart Sales.csv",header= TRUE, sep=",")
tail(myData)


set.seed(3000)
xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1)
randomdeviates<-rnorm(1000,0,1)

par(mfrow=c(1,3), mar=c(3,4,4,2))

plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Standard Normal", cex.axis=.8)

plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)

hist(randomdeviates, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))