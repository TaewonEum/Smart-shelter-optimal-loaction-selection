-단계구분도 교통량, 미세먼지, 초미세먼지, 녹지비율 코드 
library(ggplot2)
library(maps)
library(maptools)
seoul<-read.csv('data_seoul_child.csv',header=T)
head(seoul)
map_seoul<-read.csv('mapv2_final_seoul.csv',header=T)
head(map_seoul)

#서울 25개구 코드 확인
pro.list<- names(table(map_seoul$시군구명))
xx<-vector();yy<-vector()

#위도 경도 좌표 계산
for ( jj in 1:length(pro.list)){
  xx[jj]<-mean(subset(map_seoul,시군구명==pro.list[jj])$long)
  yy[jj]<-mean(subset(map_seoul,시군구명==pro.list[jj])$lat)
}
tab.x.y<-cbind(pro.list,xx,yy)
head(tab.x.y)

data5<-seoul[sort.int(seoul[,1],index.return=T)$ix,]
head(data5)

ggplot(seoul,aes(map_id=region,fill=교통량))+
  geom_map(map=map_seoul,alpha=0.3,colour='white',size=0.1)+
  theme(legend.position=c(0.1,0.8))+
  scale_fill_gradientn(colours=c('yellow','red'))+
  expand_limits(x=map_seoul$long,y=map_seoul$lat)+coord_fixed()+
  labs(x = "",y = "",title = "서울시 교통량")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.ticks = element_blank(), axis.text.y=element_blank())+
  theme(axis.ticks = element_blank(),axis.text.x=element_blank())+
  geom_text(x=xx,y=yy+400,label=data5$교통량,size=3,col=4)+
  geom_text(x=xx,y=yy-600,label=pro.list,size=3,col=1)

ggplot(seoul,aes(map_id=region,fill=녹지비율))+
  geom_map(map=map_seoul,alpha=0.3,colour='white',size=0.1)+
  theme(legend.position=c(0.1,0.8))+
  scale_fill_gradientn(colours=c('yellow','red'))+
  expand_limits(x=map_seoul$long,y=map_seoul$lat)+coord_fixed()+
  labs(x = "",y = "",title = "서울시 녹지비율")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.ticks = element_blank(), axis.text.y=element_blank())+
  theme(axis.ticks = element_blank(),axis.text.x=element_blank())+
  geom_text(x=xx,y=yy+400,label=data5$녹지비율,size=3,col=4)+
  geom_text(x=xx,y=yy-600,label=pro.list,size=3,col=1)

ggplot(seoul,aes(map_id=region,fill=미세먼지))+
  geom_map(map=map_seoul,alpha=0.3,colour='white',size=0.1)+
  theme(legend.position=c(0.1,0.8))+
  scale_fill_gradientn(colours=c('yellow','red'))+
  expand_limits(x=map_seoul$long,y=map_seoul$lat)+coord_fixed()+
  labs(x = "",y = "",title = "서울시 구별 미세먼지 평균")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.ticks = element_blank(), axis.text.y=element_blank())+
  theme(axis.ticks = element_blank(),axis.text.x=element_blank())+
  geom_text(x=xx,y=yy+400,label=data5$미세먼지,size=3,col=4)+
  geom_text(x=xx,y=yy-600,label=pro.list,size=3,col=1)

ggplot(seoul,aes(map_id=region,fill=초미세먼지))+
  geom_map(map=map_seoul,alpha=0.3,colour='white',size=0.1)+
  theme(legend.position=c(0.1,0.8))+
  scale_fill_gradientn(colours=c('yellow','red'))+
  expand_limits(x=map_seoul$long,y=map_seoul$lat)+coord_fixed()+
  labs(x = "",y = "",title = "서울시 구별 초미세먼지 평균")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.ticks = element_blank(), axis.text.y=element_blank())+
  theme(axis.ticks = element_blank(),axis.text.x=element_blank())+
  geom_text(x=xx,y=yy+400,label=data5$초미세먼지,size=3,col=4)+
  geom_text(x=xx,y=yy-600,label=pro.list,size=3,col=1)