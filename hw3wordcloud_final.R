rm(list=ls(all.names=TRUE))
#先把需要的package執行看存不存在
library(httr)
library(rjson)
library(httpuv)
library(Rfacebook)
library(plyr)
library(NLP)
library(tm)
library(rvest)
library(xml2)
Sys.setenv(JAVA_HOME="/Users/liuqieru/Desktop/jdk-9.0.1/")
library(rJava)
library(SnowballC)
library(slam)
library(Matrix)

#要先有測試(測試是否可以抓的回來)
#library(httr)
#cURL = "https://graph.facebook.com/v2.10/kikuChen?fields=posts.limit(100)&access_token=EAACEdEose0cBAABEZBUhdq5upNo7muAJE0ccCXjcqH34NEnYkNGtD3ot1wRvnD7QoeDnnACdElpJ9poWtKkWZACHMclnerSJBoL23Fr7NZARZBzmLyxar5FNr9eF95MwECckm5D9z8mIod16ElbB5GxAHpuWLEDbFQnw8PQCDZBr7A64z0doZBJBNkXUp6J3AF60ltwLFfGgZDZD"
#res = httr::GET(cURL)
#posts = httr::content(res)


prefex = "https://graph.facebook.com/v2.10/"
#會變得
token  = "EAACEdEose0cBAN9DUURMUwTObm190UUSeYBADsUJHfNgbGwr8S5bfZAmRPxyq2ArglB22EErcNItwNyosnIn8cNhIKVaexpMNOMZBBSIZBtA5OzmKaZAroOhhw3fMdCNI6bbuc1ITEjJlH8Gwp88XFxCKDpPz6MAgKoNRsLVwlNAqvc2senz86IlDneHl61ZCVwn1VrETIQZDZD"
number=1
#把正確的fb網址接起來
attrs  = paste0("232716627404/posts?limit=",number,"&until=2017-10-31&since=2017-10-25&access_token=")
url    = paste0(prefex, attrs, token)
res    = httr::GET(url)
#GET去湊出URL
#強制跟content說他是屬於httr這個library
data <-  httr::content(res)
#把data先解開他list的結構，再用matrix方式存下來
groups= matrix(unlist(data$data))

#存成檔案(因為要分梯次存，所以藉由count這個變數來存取每一篇文章)
filename = paste0(1, ".txt")
write.table(groups,filename)
#要跳到下一頁
after  = data$paging$cursors$after
nextflg= data$paging[2]

count=1
while(nextflg!= "NULL"){
  
  count=count+1
  attrs  = paste0("232716627404/posts?limit=1&until=2017-10-31&since=2017-10-25&after=",after,"&access_token=")
  #nexturl= paste0(prefex,attrs,"&after=",after)
  
  url = paste0(prefex,attrs,token)
  
  nextres= httr::GET(url)
  ndata  = httr::content(nextres)
  ngroups= matrix(unlist(ndata$data))
  #p1=ndata[["data"]][[1]]$message
  #p1=ndata$data[[1]]$message
  #可用try_catch來測試，while loop停在哪一段 可以記錄走到哪一段停止
  
  after  = ndata$paging$cursors$after
  nextflg = ndata$paging[3]
  
  filename = paste0(count, ".txt")
  
  write.table(ngroups,filename)
}







#要做文字雲
library(NLP)
library(tm)
library(jiebaRD)#專門處理中文的斷詞
library(jiebaR)#呼叫別人定義好的字典
library(RColorBrewer)
library(wordcloud)
#進行文本清理
par(family='STKaiti')#讓文字顯示成中文
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))

#要清洗掉的東西

toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
#定義清洗：清洗就是把你找到的符號用空白取代
)
docs <- tm_map(docs,toSpace,"V1")
docs <- tm_map(docs,toSpace,"\n")
docs <- tm_map(docs,toSpace, "1")
docs <- tm_map(docs,toSpace, "的")
docs <- tm_map(docs,toSpace, "及")
docs <- tm_map(docs,toSpace, "為")
docs <- tm_map(docs,toSpace, "是")
docs <- tm_map(docs,toSpace, "在")
docs <- tm_map(docs,toSpace, "[A-Za-z0-9]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

mixseg = worker()
segment <- c("陳菊","布里斯本","高雄","重劃區","合作會","後勁溪")
new_user_word(mixseg,segment)

#有詞頻之後就可以去畫文字雲
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
#畫出文字雲
wordcloud(freqFrame$Var1,freqFrame$Freq,
          min.freq=3,
          random.order=TRUE,random.color=TRUE, 
          rot.per=.1, colors=rainbow(length(row.names(freqFrame))),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

