library(NLP)
library(tm)
#library(tmcn)
#Sys.setenv(JAVA_HOME="/Users/liuqieru/Desktop/jdk-9.0.1/")
library(rJava)
library(SnowballC)
library(slam)
library(Matrix)

# import data
source("hw3txt.R")

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


