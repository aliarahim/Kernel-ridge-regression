K<-K1+K2
k<-k1+k2
lambda<-0.01
 
  ckrrpredict.Y <- c()
for (i in 1:m) {
  ckrrpredict.Y[i] <- train.Y%*%solve(lambda*diag(n)+K)%*%k[,i]
}
ckrrMSE <- mean((ckrrpredict.Y-test.Y)^2)

