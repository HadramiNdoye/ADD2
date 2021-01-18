# load data
load("regression-dataset.Rdata")
# Linear regression
lm.fit <- lm(y~x)
# Polynomial regression
lm.fit1 <- lm(y~poly(x,2, raw=TRUE))
lm.fit2 <- lm(y~poly(x,3, raw=TRUE))
lm.fit3 <- lm(y~poly(x,4, raw=TRUE))
lm.fit4 <- lm(y~poly(x,5, raw=TRUE))
lm.fit5 <- lm(y~poly(x,10, raw=TRUE))
lm.fit6 <- lm(y~poly(x,15, raw=TRUE))
lm.fit7 <- lm(y~poly(x,20, raw=TRUE))
x_grid <- seq(min(x),max(x),by = 0.01)
predict.lm1 <- predict(lm.fit1,newdata = data.frame("x"=x_grid))
plot(x_grid,predict.lm1,type="l",col="blue")
points(x,y,pch=19)
abline(lm.fit,col="red")

# fonction MSE

n <- length(x)
vectp <- c(2,3,4,5,10,15,20)
np <- length(vectp)
ychapi <- matrix(rep(0,n * np),nrow = n,ncol = np)
mse <- rep(0,np)
for (i in 1:np){
  ychapi[,i] <- predict(lm(y~poly(x,vectp[i], raw=TRUE)),newdata = data.frame(x))
  mse[i] <- (1 / n) * sum((y - ychapi[,i])^2)
}
plot(vectp,mse,type = "b")
