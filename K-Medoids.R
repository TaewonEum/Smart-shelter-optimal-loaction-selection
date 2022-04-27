2. K-Medoids
## PAM (K-Medoids)
library(cluster)
pamframe<- pam(frame_scaled[,c(6,17)],3)
plot(pamframe, main="X축: 교통량, Y축: 초미세먼지+미세먼지비율합 k-medoids(k=3)군집분석한 결과")
summary(pamframe, parameters = TRUE)

# 시각화
library("factoextra")
fviz_cluster(pamframe, geom = "point", ellipse.type = "norm")

# 각 개체가 어느 군집으로 분류되었는지 확인
pamframe$clustering
frame_scaled$kmedoids<- pamframe$clustering

#동클러스터링
2. K-Medoids
## PAM (K-Medoids)
library(cluster)
pamframe<- pam(frame_scaled[,c(6,7)],3)
plot(pamframe, main="X축: 유동인구수, Y축: 미세먼지취약계층인구수 k-medoids(k=3)군집분석한 결과")
summary(pamframe, parameters = TRUE)

# 시각화
library("factoextra")
fviz_cluster(pamframe, geom = "point", ellipse.type = "norm")

# 각 개체가 어느 군집으로 분류되었는지 확인
pamframe$clustering
frame_scaled$kmedoids<- pamframe$clustering
