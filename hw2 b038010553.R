Titanic <- read.csv(file.choose())
library(dplyr)
dd <- Titanic
#刪除na資料
dd <- dd[!is.na(dd$age),]
dd <- dd[!is.na(dd$fare),] #剔除資料中fare=9999的擔保人

#資料類型整理
#gender,survival <- 轉換成factor  fare<- 比例  age <- 區間資料

#統計敘述
gender <- factor(dd$gender)

table(dd$fare)
dd$fare <- cut(dd$fare, c(0,150,600))


#Q1.活著且class階級最高的人從事商人的工作，是從什麼地方上岸？
temp <- select(dd,survival,gender,joined,class,job) %>%
    filter(survival==1,class==1,job=='Businessman',gender==1)
   #發現女性沒有人是從事商人

temp1 <- select(dd,survival,gender,joined,class,job,age) %>%
  filter(survival==1,class==1,job=='Businessman',gender==0)
  group_by(temp,joined) 
    summarise(temp1,mean(age))
    #發現全部都是男性商人的工作，平均存活年紀在41.875，而且多是來自Southampton與Cherbourg的地區
  
  
#Q2.想尋找class階層為3的人相較於class1階層的人，存活情況(想了解是否class3階層比較低的人會不會比較沒有逃生的機會)ˊ      
temp2 <- select(dd,class,survival) %>%
   filter(class==1)
    aa<-temp2[-which(temp2$survival==1),]
    dr<-length(aa$survival==0)/length(temp2$survival)
   
temp3 <- select(dd,class,survival) %>%
  filter(class==3)
   aa1<-temp3[-which(temp3$survival==1),]
   dr1<-length(aa1$survival==0)/length(temp3$survival)
  



#Q3.class1階級中，從事商人工作的人，購買貴的票的比例一定會比較高嗎？買高得票存活比例會比較高嗎？其存活比例是多少？

temp4 <- select(dd,survival,fare,class,job) %>%
   filter(class==1,job=='Businessman')
   a=sum(temp4$fare=="(0,150]")/length(temp4$fare)
   b=sum(temp4$fare=="(150,600]")/length(temp4$fare)
    aa2<-temp4[-which(temp4$survival==0),]
     dr3<-length(aa2$survival==1)/length(temp4$survival)
     
     
    

