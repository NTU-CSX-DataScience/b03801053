source('MLDM_facebook_new1.R')
#install.packages("devtools")
library(devtools)
#install_github("ggbiplot","vqv")
library(scales)
library(grid)
library(ggbiplot)




testTfidf = doc.tfidf
tfidf.pca <- prcomp(testTfidf)
tfidf.kmeans <- as.factor(kmeansOut$cluster)
#讓文字顯示成中文
#biplot(tfidf.pca,color=c(1,5))
g <- ggbiplot(tfidf.pca, obs.scale = 1, var.scale = 1, 
              groups = tfidf.kmeans, ellipse = TRUE, 
              circle = TRUE, labels = rownames(testTfidf ),ylim=c(0,0), xlim=c(0,0))

g <- g + scale_color_discrete(name = '')
g <- g+theme(text = element_text(family="Microsoft YaHei"))
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top',panel.grid.major.y = element_blank(),axis.text = element_text(colour = "blue"))

print(g)