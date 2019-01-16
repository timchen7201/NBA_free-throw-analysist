setwd('/Users/timchen/R/midterm/')
library(dplyr)
library(data.table)
library(stringr)
library(tidyverse)
data=fread('free_throws.csv',stringsAsFactors = F)
 

#################Create make_miss ############
play_id<-paste(data$game_id,data$period,data$time,sep = '-')
data<-mutate(data,Play_id=play_id)
data$make_miss<-ifelse(grepl('make',data$play),1,0)
data$techique<-ifelse(grepl('technical',data$play),TRUE,FALSE)
###############################################
#################individule freethrow status ############

generate_new_data<-function(z){
  if(length(z)==1){
    return(paste(z[1],-1,-1))
  }
  else if(length(z)==2){
    return(paste(z[1],z[2],-1))
  }
  else{
    return(paste(z[1],z[2],z[3]))
  }
} 

ndata<-data%>%group_by(Play_id)%>%summarise(binary_status=generate_new_data(make_miss)
                                            ,tech=sum(techique),
                                            free_throw_player=first(player)
                                            ,time_line=first(time)
                                            ,period=first(period))
ndata <- separate(ndata,binary_status,c("free_1", "free_2", "free_3"),' ')
strsplit(ndata$time_line,':')
interval<-sapply(strsplit(ndata$time_line,':',1),`[`, 1)
ndata$interval<-as.numeric(interval)
View(ndata)
class(ndata$interval)
data2<-fread("Seasons_Stats.csv",stringsAsFactors = F)[Year %in% c(2006:2016)]
#height_weight<-fread('Player_1947-2017.xlsx',stringsAsFactors = F,fill = T)
#if first free throw and second free throw are independent, then we can say that
#p-value is <0.05 with chi-test
two_throws <- filter(ndata,free_2 !=-1)
two_throws
tbl <- table('first throw' = two_throws$free_1, 'second throw' = two_throws$free_2)
chisq.test(tbl)
#accuracy for every single shot
free1_acc<-dim(filter(ndata,free_1==1))[[1]]/dim(ndata)[[1]]
free1_acc<-dim(filter(ndata,free_2==1))[[1]]/dim(filter(ndata,free_2!=-1))[[1]]
# 這裡計算在以time_line,period分群的情形下全聯盟的罰球名中率。attempt是出手次數
# 計算方法是利用table看出free_1、free_2、free_3中-1、0、1的frequency，然後算出總出
# 手數(attempt)跟命中數(hit)
tmp<-ndata%>%group_by(interval,period)%>%summarise(
  attempt=sum(as.vector(table(free_1)))
  +sum(as.vector(table(free_2))[2:3])
  +sum(as.vector(table(free_3))[2:3]),
  hit=as.vector(table(free_1))[2]
  +as.vector(table(free_2))[3]
  +as.vector(table(free_3))[3]
  ,total_percentage= hit/attempt
)%>%filter(period<5)%>%arrange(interval)
 
tmp<-na.omit(tmp) 
p<-ggplot(data = tmp, aes(x =interval, y = total_percentage,group=1))+
  geom_line(aes(color = as.factor(period)), size = 1) +
  facet_wrap( ~ period)
p+scale_x_continuous(breaks=seq(0, 12, 1))

