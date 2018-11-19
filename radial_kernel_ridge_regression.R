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
K2<-matrix(0,n,n)
k2<-matrix(0,n,m)
lambda2<-0.1
gam<-0.001
for (i in 1:n){
  for (j in 1:n) {
    kxx=exp(-gam*sum(train.X[i,]-train.X[i,])^2)
    kxy=exp(-gam*sum(train.X[i,]-train.X[j,])^2)
    kyy=exp(-gam*sum(train.X[j,]-train.X[j,])^2)
    if (kxx==0 | kyy==0) {
      res <-0
    } else {
      res <- (kxy/(sqrt(kxx)*sqrt(kyy)))
    }
    K2[i,j]<-res+1
  }
  for (l in 1:m) {
    kxx=exp(-gam*sum(train.X[i,]-train.X[i,])^2)
    kxy=exp(-gam*sum(train.X[i,]-test.X[l,])^2)
    kyy=exp(-gam*sum(test.X[l,]-test.X[l,])^2)
    if (kxx==0 | kyy==0) {
      res <- 1
    } else {
      res <- (kxy/(sqrt(kxx)*sqrt(kyy)))+1
    }
    k2[i,l]<-res
    
  }
}

rkrrpredict.Y <- c()
for (i in 1:m) {
  rkrrpredict.Y[i] <- train.Y%*%solve(lambda2*diag(n)+K2)%*%k2[,i]
}
rkrrMSE <- mean((rkrrpredict.Y-test.Y)^2)

