library(RColorBrewer)#展示颜色版
display.brewer.all()#提取颜色条
cols<-brewer.pal(n=11,name='Grevs')#n为提取颜色数量
pie(rep(1,11),col=cols,labers='',border=NA)#制作任意数量的颜色条
my.colorbar<-colorRampPalette(colors=cols)#my.colorbar是一个函数
cols2<-my.colorbar(500)
pie(rep(1,500),col=cols2,labels='',border=NA)

#统计每年中雨、大雨、暴雨和大暴雨的天数（条形图
library(tidyverse)
path<-paste('D:\\observed station data\\daily precipitation\\data\\','58519.csv',sep='')
prcp<-read.csv(file=path,header=TRUE,stringsAsFactors = FALSE)
year<-str_sub(string=prcp$date,start=1,end=4)
year<-as.numeric(year)
year.unique<-unique(year)[-1]#去掉数据不完整的第一年
year.unique
counts<-sapply(X=year.unique,FUN=function(x){#将每一行作为变量
  ind<-which(year==x)
  group<-cut(prcp$precip[ind],breaks=c(10,24.9,49.9,100,10000),
             include.lowest = TRUE,labels=c('moderate','big','heavy','storm'))
  nums<-table(group)
  nums
})
counts
counts<-t(counts)#矩阵转置
storm<-data.frame(year=year.unique,counts)

#单变量
ggplot(data=storm[1:10,],mapping=aes(x=year,y=moderate))+
  geom_bar(stat='identity',fill=brewer.pal(9,'Set1')[3],
           colour=brewer.pal(9,'Set1')[2],lwd=1.5)
#mapping为映射，fill为条形图内部区域颜色，colour为条形图边框颜色，lwd为线宽度


#双变量
storm2<-data.frame(year=rep(x=storm$year[1:10],times=2),
                   counts=c(storm$moderate[1:10],storm$big[1:10]),
                   types=rep(c('moderate','big'),each=10))
ggplot(data=storm2,mapping=aes(x=year,y=counts,fill=types))+
  geom_bar(stat='identity',position='stack',width=0.6)+
  scale_fill_brewer(palette='Set2')
#scale_fill_brewer:属于标度，控制颜色条
#position:控制是否分列（stack,dodge）


#折线图:单变量
ggplot(data=storm,mapping=aes(x=year,y=moderate))+
  geom_line()
ggplot(data=storm,mapping=aes(x=year,y=moderate))+
  geom_line()+geom_point()

#折线图：双变量
storm3<-gather(data=storm[,1:3],key='group',value='counts',-year)
ggplot(data=storm3,
       mapping=aes(x=year,y=counts,group=group,color=group))+
  geom_line()+geom_point(size=2,shape=21)
?(gather)
