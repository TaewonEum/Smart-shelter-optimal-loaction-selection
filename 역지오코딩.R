-X좌표, Y좌표로 주어진 버스정류장 위치를 역지오코딩하여 주소로 변환
####위경도를 주소로 변환####
#install.packages("ggmap")
library(ggmap)
library(tidyverse)
library(leaflet)
library(httr)
library(jsonlite)
library(glue)

#데이터 불러오기
data<-read.csv("C:/Users/user/OneDrive - (주)알투컴/바탕 화면/project/smartgreen/data/geocoding2.csv",header=T)
head(data)
data$busstop_name<-enc2utf8(data$busstop_name)
data$busstop<-as.character(data$busstop_name)

#APIkey 입력
api_key <- ""

#빈 데이터 프레임 생성
result<-data.frame(lon=numeric(0),lat=numeric(0),시=character(0),구=character(0), 동=character(0))
result

#역 지오코딩(카카오 이용)
for (i in 1:nrow(data)){
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json',
        query = list(x=data$lon[i],y=data$lat[i]),
        add_headers(Authorization = paste0("KakaoAK ", api_key))) %>%
    content(as = 'text') %>%
    fromJSON()
  
  result = result %>% add_row(
    lon = data$lon[i],
    lat = data$lat[i],
    시 = data_list$documents$region_1depth_name[2],
    구 = data_list$documents$region_2depth_name[2],
    동 = data_list$documents$region_3depth_name[2]
  )
}

head(result)

#결과 저장
data_result<-data.frame("버스정류장 이름"=data$busstop_name,"노선 개수"=data$bus_cnt,result)
head(data_result)

write.csv(data_result,"C:/Users/user/OneDrive - (주)알투컴/바탕 화면/project/smartgreen/data/geocoding3.csv",row.names=T)