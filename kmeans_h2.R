str(doc.tfidf)
summary(doc.tfidf)
  #最多可以random多少？

dat = doc.tfidf[,c(5,9)]
plot(dat, main = "文本間的關係", pch =20, cex =2)
#探討特權與學習有沒有關係

set.seed(11)

#隨意拿兩個
km1 = kmeans(dat, 2, nstart=50) #100 < 435 是合理的

以下都分成兩群
#km1$withinss
#km1$cluster
#km1$centers

# Plot results   根據kmean底下的cluster去畫出結果來
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)



set.seed(11)
km1 = kmeans(dat, 2, nstart=50)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)

#ask!
mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:10) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
#不知道分幾群時，可以用這方式，先找到一個穩定的分群(可是分群)，但要思考這是什麼樣的意義？
#例如像這裡可分8群

set.seed(11)
km2 = kmeans(dat, 4, nstart=50)

# Examine the result of the clustering algorithm
km2

plot(dat, col =(km2$cluster +1) , main="K-Means result with 4 clusters", pch=20, cex=2)

