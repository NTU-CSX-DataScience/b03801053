
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

