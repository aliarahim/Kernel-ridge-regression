advertising<-read.csv("Advertising.csv")
X<-advertising[,2:4]
Y<-advertising[,5]
set.seed(2606)
train<-sample(1:nrow(X),2*nrow(X)/3)
train.X<-X[train,]
train.Y<-Y[train]
test.X<-X[-train,]
test.Y<-Y[-train]
n<-nrow(train.X)
m<-nrow(test.X)
K1<-matrix(0,n,n)
k1<-matrix(0,n,m)
# paramaters
d<-4
lambda1<-0.01
for (i in 1:n){
  for (j in 1:n) {
    kxx=(1+sum(train.X[i,]*train.X[i,]))^d
    kxy=(1+sum(train.X[i,]*train.X[j,]))^d
    kyy=(1+sum(train.X[j,]*train.X[j,]))^d
    if (kxx==0 | kyy==0) {
      res <- 1
    } else {
      res <- kxy/(sqrt(kxx)*sqrt(kyy))+1
    }
    K1[i,j]<-res
  }
  for (l in 1:m) {
    kxx=(1+sum(train.X[i,]*train.X[i,]))^d
    kxy=(1+sum(train.X[i,]*test.X[l,]))^d
    kyy=(1+sum(test.X[l,]*test.X[l,]))^d
    if (kxx==0 | kyy==0) {
      res <- 1
    } else {
      res <- kxy/(sqrt(kxx)*sqrt(kyy))+1
    }
    k1[i,l]<-res
    
  }
}
pkrrpredict.Y <- c()
for (i in 1:m) {
  pkrrpredict.Y[i] <- train.Y%*%solve(lambda1*diag(n)+K1)%*%k1[,i]
}
pkrrMSE <- mean((pkrrpredict.Y-test.Y)^2)
