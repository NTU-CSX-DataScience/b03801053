rm(list=ls(all.names=TRUE))
install.packages("httr")
install.packages("rjson")
install.packages("httpuv")
install.packages("Rfacebook")
install.packages("plyr")
install.packages("NLP")
install.packages("tm")
install.packages("tmcn")
install.packages("rvest")
install.packages("xml2")



library(httr)

library(rjson)
library(httpuv)
library(Rfacebook)
library(plyr)
library(NLP)

library(tm)
library(tmcn)
library(rvest)
library(xml2)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_77/")
install.packages("rJava")
install.packages("SnowballC")
install.packages("slam")
install.packages("Matrix")
library(rJava)

library(SnowballC)
library(slam)

library(Matrix)



library(httr)
library(rjson)
library(plyr)
#固定得先存下來
prefex = "https://graph.facebook.com/v2.10/"
#會變得
token  = "EAACEdEose0cBADZA0ayYzsLIyd9i7VQ9Brkx5q4cKsQSEfSpEr6QwFieZCNmnOjDpVCsXMf3zPxLpPJgGoaGVhZA3LXDe8hsZBXMVkQfCVpssYEhByeNLUFzlZBX2ZAxs6CWSVxH2WBYpS7uxdZAG2uoMf9wdTnZBc0aDY713UY7VxYyHSnPJr8YQpFiJdM5hJm4gB0sGMWONQZDZD"
number = 3 #會變動的用變數表示 #不變的用字串表示
#湊出來的網址要去跟臉書網址核對
#(抽斷改)把能變得放進來，在用paste0去黏起來

attrs  = paste0("232716627404/posts?since=2017-09-10&until=2017-10-20&limit=",number,"&access_token=")

url    = paste0(prefex, attrs, token)
res    = GET(url)#GET去湊出URL
data <- content(res)

#writeBin(bin, "myfile.txt")

#網頁是jason的格式，所以你用content的格式就可以把jason的內容全部都掃回來
#是因為有rvest這個套件 會把jason回傳的格式轉換，我們才可以看懂
#因為我想要連續拿幾篇的po文來做文字雲，所以用dataframe的格式存取，而沒有用unlist的方式解開list
#但是卻無法連續拿好幾篇，所以在有作文字雲的時候，只抓了一篇po文來
nd <- function(j){data.frame( 
  id = j$id,
  created_time =  j$created_time,
  message = if(!is.null(j$message)) j$message else NA,
  story = if(!is.null(j$story)) j$story else NA
)}
library(plyr)
ndataframe <- do.call("rbind.fill", lapply(data$data, nd))




#data底下的資料去依序進去拿，才可以到下一頁(造結構把單一資料拿出來)
after  = data$paging$cursors$after
#第一次取出來 2:
nextflg= data$paging[2] #paging下cursors的第二格是after 2:拿到after裡面的資料
number=25
while( nextflg != "") #資料裡面有值，就要進到while裡面  去做下一頁存取的動作
{ #此些可以寫成apply
  #
  attrs  = paste0("kikuChen/posts?pretty=0&since=2017-09-10&until=2017-10-20&limit=",number)
  nexturl= paste0(prefex,attrs,"&after=",after,"&access_token=")
  
  url1    = paste0(nexturl, token)
  
  nextres= GET(url1)
  ndata  = content(nextres)
  #ngroups= matrix(unlist(ndata$data))
  nex <- do.call("rbind.fill", lapply(ndata$data, nd))
  
  n <- rbind.fill(ndataframe, nex)
  
  
  after  = ndata$paging$cursors$after
  nextflg= data$paging[2]
  print(n)
  
}

write.table(n,"d.txt")


#文字雲

install.packages("rJava")
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
install.packages("tm")
install.packages("tmcn", repos="http://R-Forge.R-project.org", type="source")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("XML")
install.packages("RCurl")
install.packages("jiebaR")
install.packages("jiebaRD")
library("jiebaR")
library("jiebaRD")
library("tm")
library("RcolorBrewer")
library("wordcloud")


r <-ndataframe$message[1:3]
#if(ndataframe$id=='232716627404_10155031303432405')
#{r <- ndataframe$message}

g =unlist(r)
gtitle <- gsub("\n","",g)
title <- gsub("的","",g)
library(NLP)
library(tm)
dd<- Corpus(VectorSource(title))
t = tm_map( dd ,stripWhitespace )
t= tm_map( dd, removePunctuation )
t = tm_map( dd, removeNumbers )

mixseg = worker()
segment <- c("布里斯本")
new_user_word(mixseg,segment)

#有詞頻之後就可以去畫文字雲
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(t, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=1,max.words=50,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(5, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)


