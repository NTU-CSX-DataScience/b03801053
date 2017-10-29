rm(list=ls(all.names=TRUE))
#install.packages("httr")
#install.packages("rjson")
#install.packages("httpuv")
#install.packages("Rfacebook")
#install.packages("plyr")
#install.packages("NLP")
#install.packages("tm")
#install.packages("tmcn")
#install.packages("rvest")
#install.packages("xml2")

#install.packages('tmcn', repo='http://nbcgib.uesc.br/mirrors/cran/')
#Sys.sleep(sample(3:5,1))

library(httr)

library(rjson)
library(httpuv)
library(Rfacebook)
library(plyr)
library(NLP)
#install.packages("tm", repos="http://R-Forge.R-project.org")
library(tm)
library(tmcn)
library(rvest)
library(xml2)

#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_77/")
#install.packages("rJava")
#install.packages("SnowballC")
#install.packages("Slam")
#install.packages("Matrix")
library(rJava)

library(SnowballC)
library(slam)

library(Matrix)

#固定得先存下來
prefex = "https://graph.facebook.com/v2.10/"
#會變得
token  = "EAACEdEose0cBAE5JJugFunJ4KHbHcumZArso3gwkBdylYXzTE54x6sMditZAT4rtjDf8VPKy5pZB6THZB0oNxQlTaeNF1vRjrSlU8v7fjPOaZABAwZAaqgUSE4vHjdit7Nn9dHZCsRA7lJsGbzeYf592Qs0fwJKHAGPV1rZA17MoMiua7BCMjY9Hup5yH6F48HATJKZCzr7cDHAZDZD"

number=1

attrs  = paste0("232716627404/posts?limit=",number,"&until=2017-10-29&since=2017-10-19&access_token=")

url    = paste0(prefex, attrs, token)
res    = GET(url)#GET去湊出URL
data <- content(res)
#p=data[["data"]][[1]]$message
groups= matrix(unlist(data$data))
  
  
filename = paste0(1, ".txt")
write.table(groups,filename)



after  = data$paging$cursors$after
#第一次取出來 2:
nextflg= data$paging[2]

count=1
while(nextflg!=""){

    
count=count+1
nexturl= paste0(prefex,attrs,"&after=",after,"&access_token=")

url = paste0(nexturl, token)

nextres= GET(url)
ndata  = content(nextres)
#p1=ndata[["data"]][[1]]$message
#p1=ndata$data[[1]]$message
ngroups= matrix(unlist(ndata$data))



after  = ndata$paging$cursors$after
#第一次取出來 2:
nextflg= ndata$paging[2]
filename = paste0(count, ".txt")

write.table(ngroups,filename)
}

library(NLP)
library(tm)
library(jiebaRD)#專門處理中文的斷詞
library(jiebaR)#呼叫別人定義好的字典
library(RColorBrewer)
library(wordcloud)
#進行文本清理
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))

#移除可能有問題的符號  呼叫切字的套件
#toSpace <- content_transformer(function(x, pattern) {
#  return (gsub(pattern, " ", x))
#}
#定義清洗：清洗就是把你找到的符號用空白取代
#)

#要清洗掉的東西

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
#定義清洗：清洗就是把你找到的符號用空白取代
)

#docs <- tm_map( docs,function( word ){ gsub( "[A-Za-z0-9]", "", word ) } )
docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "的")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)


StopWords = stopwordsCN( )
docs = tm_map( docs, removeWords, StopWords )
docs1 = tm_map( docs, content_transformer( segmentCN ),
                      nature = TRUE, returnType = 'tm')
#a<-matrix(unlist(docs))


mixseg = worker()
segment <- c("布里斯本","高雄","重劃區","合作會")
new_user_word(mixseg,segment)

#有詞頻之後就可以去畫文字雲
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=1,max.words=20,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(5, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

