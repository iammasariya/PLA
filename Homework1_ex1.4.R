rm(list=ls())
library(ggplot2)
updateY <- function(line, X)
{
  values <- (line[2,1] - line[1,1]) * (X[,2] - line[1,2]) -
    (line[2,2] - line[1,2]) * (X[,1] - line[1,1])
  return(sign(values))
}

runLearning <- function(X,Y)
{
 none <- F
 
 W <- matrix(1,1,3)
 X <- cbind(rep(1,n),X)
 
 for(i in 1:10000)
 {
   h <- sign(W %*% t(X))
   
   missclassified <- h != Y
   print(missclassified)
   
   if(sum(missclassified) == 0)
   {
     done <- T
     print(i)
     break
   }
   else
   {
     missclassified.X <- X[missclassified, , drop = F]
     missclassified.Y <- Y[missclassified]
     
     # Get one of them
     randomT <- sample(dim(missclassified.X)[1], 1)
     X.t <- missclassified.X[randomT, , drop = F]
     Y.t <- missclassified.Y[randomT]
     
     W <- W + Y.t %*% X.t * 0.2
     print(W)
   }
 }
 
 return(W)
 
}

drawLine <- function(W)
{
  X1 <- c(-2.0, 2.0)
  X2 <- -(W[2]/W[3]) * X1 - W[1]/W[3]
  return(matrix(cbind(X1, X2), 2, 2))
}

line <- matrix(runif(4,-1,1),2,2)
n <- 100
X <- matrix(runif(n*2,-1,1),n,2)
Y <- updateY(line,X)

W <- runLearning(X,Y)
plot(X, pch = Y, xlab = "x-coordinate",ylab = "y-coordinate", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
print(line)
print(X)
print(Y)
print(W)
#plot(X,pch=Y)
abline(lm(line[,2]~line[,1]), col="red")
lines(drawLine(W),col="green")
text(X[,1],X[,2],labels=Y,pos=2)

N <- 10
testX <- matrix(runif(N*2,-1,1),N,2)
testX <- cbind(rep(1,N),testX)
testY <- updateY(line,testX)
testH <- sign(W %*% t(testX))

miss <- 0
for(k in 1:N)
{
  if(testH[k] != testY[k])
  {
    miss <- miss +1
  }
}

miss <- miss/N
print(miss*100)
