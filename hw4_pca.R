library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)

filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))


toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs<- docs[(1:11)]

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))


library(NLP)
library(tm)
#library(tmcn)
#Sys.setenv(JAVA_HOME="/Users/liuqieru/Desktop/jdk-9.0.1/")
library(rJava)
library(SnowballC)
library(slam)
library(Matrix)



# corpus to tdm
d.corpus <- Corpus(VectorSource(seg)) 



tdm <- TermDocumentMatrix(d.corpus, 
                          control = list(wordLengths = c(2, Inf)))
#View(inspect(tdm[1:9, 1:11]))
#ass = findAssocs(tdm, "高雄", 0.70)



# tf-idf computation
N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)


doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

library(stats)
kmeansOut <- kmeans(doc.tfidf, 4, nstart = 50)



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

