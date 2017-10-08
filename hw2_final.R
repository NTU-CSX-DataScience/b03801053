library(dplyr)
library(ggmap)
library(mapproj)

#匯入檔案
H <- read.csv(file.choose())
A <- read.csv(file.choose())

#將中文標題命名為英文
names(H)=c("country","diagnostic_year","diagnostic_month","gender","age","case")
names(A)=c("diagnostic_year","diagnostic_month","gender","age","case","country")

#Q2.在資料中想看在2000年診斷出HIV病毒的15-24歲的男性的人數，五年內檢驗出感染AIDS病毒的人數分佈趨勢是如何？
library(gridExtra)
str(H)

nh<-select(H,country,diagnostic_year,diagnostic_month,case,age,gender) %>%
  filter(country=="台北市",age=="25-34",gender=="男")
nh1 <- group_by(nh,diagnostic_year) %>%
  summarise(sum(case))
names(nh1) <- c("year","total_case")
w1 <- ggplot(data=nh1,aes(x=year,y=total_case,group=1))+geom_line(colour="blue")+geom_point(colour="red")
w1   

na<-select(A,country,diagnostic_year,case,age,gender) %>%
   filter(country=="台北市",age=="25-34",gender=="男")
na1 <- group_by(na,diagnostic_year) %>%
  summarise(sum(case))
names(na1) <- c("year","total_case")
w2<-ggplot(data=na1,aes(x=year,y=total_case,group=1)) +geom_line(color="green")+geom_point(colour="orange")
w2
 p=grid.arrange(w1, w2, nrow=2)
 

 
       
#分析結果發現在15-24歲的年輕男性共有99個人在2000年的時候感染HIV病毒，感染HIV病毒之後，發現在2002年～2006年確診AIDS病毒的人有149個人(一般來說，發病期大約需要2~5年，此階段也是所謂的AIDS期)，雖然測出AIDS病毒的人可能包含2000年前測出HIV病毒而在2002~2006年發病的人，但149-99=50多的這50個人可能是在2000年前測出HIV病毒而再2002~2006年發病的人，因此由2000年得到HIV病毒並在2~5年後發病的人數，我們極可能推論，得到HIV病毒的人，幾乎會在2~5年內發病
       

#Q1.愛滋病多分布在台灣的哪個地區？(畫圖呈現)
#合併個層級醫療院所
hos <- read.csv(file.choose())
hoss<-hos
hoss$city = substr(hoss$醫事機構地址, 1, 3)
nac = select(A,diagnostic_year,case,country) %>%
  filter(diagnostic_year==2017)
group_by(nac,country) %>%
   summarise(sum(case))
names(nac) = c("year","case", "city")
hoss = hoss[,c(6,7,8)]
names(hoss) = c("gpsx", "gpsy","city")
nhoss = hoss %>% group_by(city) %>%
  summarise(mean(gpsx), mean(gpsy))

names(nhoss) = c("city","gpsx", "gpsy")

nac$city <- as.character(nac$city)
str(nhoss)
str(nac)
n <- left_join(nac,nhoss,by = c("city"="city"))

nn<-n <-  n[!is.na(n$gpsx),]
newdata <- select(nn,case,city,gpsx,gpsy)
newdata$case <- as.numeric(newdata$case)
str(newdata)
#names(nn) = c("city","gpsx", "gpsy","case")
map <- get_map(location = 'Taiwan', zoom = 7,
               language = "zh-TW")

ggmap(map)+
  geom_point(aes(x = gpsx , y = gpsy),data=newdata,col="pink",size = newdata$case)

str(nhoss)
str(nac)

#Q3.醫療層級比較多的地方，是否愛滋病發病後存活的年數可以比較久？
#合併死亡的資料

#合併檔案，以縣市為合併依據
newData = inner_join(H, A, by = c("country"="country"))



