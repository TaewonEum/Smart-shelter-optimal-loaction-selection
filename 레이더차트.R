-최종 선정 입지 중 최종 점수 상위 5개의 버스정류장을 5개 분류 
(승차 총 승객수, 하차 총 승객수, 대기오염 노출 점수, 예상 매연 점수, 노선 개수)별로 점수를 매겨 시각화한 레이더 차트
library(MASS)
library(ggplot2)
library(fmsb)
library(doBy)
final=read.csv("C:/KJW/data intern/proj/22개에노선개수추가.csv",header=T)
final=final[1:6,]
head(final)
mean_by_Type2<-summaryBy(승차총승객수 + 하차총승객수+ 예상매연점수 + 대기오염노출점수 + 노선개수 ~ 정류소명,data=final)
mean_by_Type2

names(mean_by_Type2)[2]="승차총승객수"
names(mean_by_Type2)[3]="하차총승객수"
names(mean_by_Type2)[4]="대기오염노출점수"
names(mean_by_Type2)[5]="예상매연점수"
names(mean_by_Type2)[6]="노선개수"

df2<-mean_by_Type2[,c(2:6)]
df_radarchart <- function(df) {
  df <- data.frame(df)
  dfmax <- apply(df, 2, max)
  dfmin <- apply(df, 2, min)
  as.data.frame(rbind(dfmax, dfmin, df))
}

mean_by_Type<- df_radarchart(df2)

row.names(mean_by_Type)<-c('max','min',names(table(final$정류소명)))
names(mean_by_Type)
d=as.data.frame(mean_by_Type)
write.csv(d,"끝2.csv")

final

#색깔설정
#install.packages("RColorBrewer")
library(RColorBrewer)
coul<- brewer.pal(6,"Spectral")
colors_border<-coul
c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart(df = mean_by_Type, # The data frame to be used to draw radarchart seg = 6, # The number of segments for each axis
           pty = 16, # A vector to specify point symbol: Default 16 (closed circle)
           pcol = colors_border, #A vector of color codes for plot data
           plty = 1, # A vector of line types for plot data
           plwd = 4, # A vector of line widths for plot data
           cglty = 1,
           cglcol="grey",
           title = c("버스정류장 최종입지 레이더차트 (최종점수 상위 5개)") # putting title at the top-middle
)
legend("topleft", legend = mean_by_Type2$정류소명, col=colors_border, lty = 1, lwd = 2)