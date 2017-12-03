library(e1071)
library(ggplot2)
dfev <- read.csv("fev.csv")
dfev <- dfev[2:6]

dfev <- cbind(dfev[1:3],dfev[5])

g<-ggplot(data=dfev,aes(x =FEV , y = Age)) 
g1 <- g + geom_point(aes(color = Smoker))



#建模型
x <- subset(dfev,select = -Smoker)
y <- dfev$Smoker
plot(x, col = y , pch = 5)

df <- data.frame(x,as.factor(y))
model <- svm(y~.,data = dfev,cost= 10,kernal = "linear", scale = FALSE)
print(model)

#預測
presult <- predict(model,x)

#評估
table(presult, y )

plot(model,df)

make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1, 1], to = grange[2, 1], length = n)
  x2 = seq(from = grange[1, 2], to = grange[2, 2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid = make.grid(x)
ygrid = predict(model, xgrid)
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[model$index, ], pch = 5, cex = 2)
beta = drop(t(model$coefs) %*% x[model$index, ])
beta0 = model$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)
points(x, col = y + 3, pch = 19)
points(x[model$index, ], pch = 5, cex = 2)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2)
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2)


