3. Ward’s Method를 이용한 계층적 군집 분석

#데이터 불러오고 확인하기
df<-read.csv(file="C:/KJW/data intern/proj/전처리fin/2019_구별_어린이_노인_녹지면적_미세먼지_성분_교통량_FIN.csv", header=TRUE)
head(df)
library("factoextra")

## 행 변환 작업
# 첫번째 열을 행 이름으로 지정하고 싶기 때문에 name이라는 변수로 지정해서 추출
name<- df[,1]

#데이터 프레임을 만들면서 name 변수를 행 이름으로 지정해줌
frame<- data.frame(df, row.names=name)

#첫번째 열을 지움
frame<- frame[,-1]
head(frame)

# 결측치 확인
colSums(is.na(frame))

# 군집분석을 할 때 변수별 척도가 다를 경우 
# 큰 척도의 변수 값에 군집의 결과가 휘둘릴 수 있어서 정규화를 먼저 진행
# PCA 하기 전에 정규화되어 있어야 함
frame_scaled<- as.data.frame(scale(frame))
head(frame_scaled)

# 변수로 사용할 열 추가 생성하기
frame_scaled$취약계층비율= frame_scaled[,4]+frame_scaled[,5]
frame_scaled$대기오염물질= frame_scaled[,10]+frame_scaled[,11]+frame_scaled[,12]
frame_scaled$초미세미세합= frame_scaled[,13]+frame_scaled[,14]
head(frame_scaled)

# 상관계수 확인
cor.test(frame_scaled$초미세미세합, frame_scaled$교통량, method = "pearson")

# 교통량, 초미세미세 합
## k값 선정(군집 갯수 선정)
library(NbClust)
nc<-NbClust(frame_scaled[,c(6,17)],distance="euclidean",min.nc=2,max.nc=5,method="average")
#According to the majority rule, the best number of clusters is  3

## 거리행렬 구하기
d.mat<- dist(frame_scaled[,c(6,17)], method="euclidean")
as.matrix(d.mat)[1:4,] #거리 계산 결과

## 계층적 클러스터링 (Ward's Method 이용)
# 덴드로그램 시각화
hc.frame_scaled<- hclust(d.mat, "ward.D")
plot(hc.frame_scaled, hang=-1, cex=1)

## k값을 3로 하는 것이 좋겠다고 판단했으니 3개의 군집으로 데이터를 나눔
clust.member<- cutree(hc.frame_scaled, k=3)
clust.member #군집1, 군집2, 군집3
rect.hclust(hc.frame_scaled, k=3)

frame_scaled$Ward.교통량.초미세미세합<- clust.member

#자치구 라벨링
frame_scaled$자치구<- rownames(frame_scaled)

#동클러스터링
3. Ward’s Method를 이용한 계층적 군집 분석
#데이터 불러오고 확인하기
df<-read.csv(file="C:/KJW/data intern/proj/전처리fin/구별_동별_어린이19세_노인_수_비율_유동인구_fin_.csv", header=TRUE)
head(df)

## 행 변환 작업
# 두번째 열(동)을 행 이름으로 지정하고 싶기 때문에 name이라는 변수로 지정해서 추출
name<- df[,2]

#데이터 프레임을 만들면서 name 변수를 행 이름으로 지정해줌
frame<- data.frame(df, row.names=name)
#첫번째 열(구)을 지움
frame<- frame[,-1]
#두번째 열(동)을 지움
frame<- frame[,-1]
head(frame)

# 변수로 사용할 열 추가 생성하기
frame$취약계층인구수= frame[,2]+frame[,3]
head(frame)

# 결측치 확인
colSums(is.na(frame))

# 군집분석을 할 때 변수별 척도가 다를 경우 
# 큰 척도의 변수 값에 군집의 결과가 휘둘릴 수 있어서 정규화를 먼저 진행
# PCA 하기 전에 정규화되어 있어야 함
frame_scaled<- as.data.frame(scale(frame))
head(frame_scaled)

write.csv(frame_scaled, "C:/KJW/data intern/proj/전처리fin/유동인구_미먼취약_정규화.csv")

# 최적 클러스터갯수 구하기 NbClust 패키지 이용
library(NbClust)
nc<-NbClust(frame_scaled[,c(6,7)],distance="euclidean",min.nc=2,max.nc=5,method="average")

## 거리행렬 구하기
d.mat<- dist(frame_scaled[,c(6,7)], method="euclidean")
as.matrix(d.mat)[1:4,] #거리 계산 결과

## 계층적 클러스터링 (Ward's Method 이용)
# 덴드로그램 시각화
hc.frame_scaled<- hclust(d.mat, "ward.D")
plot(hc.frame_scaled, hang=-1, cex=1)

## k값을 3로 하는 것이 좋겠다고 판단했으니 3개의 군집으로 데이터를 나눔
clust.member<- cutree(hc.frame_scaled, k=3)
clust.member #군집1, 군집2, 군집3
rect.hclust(hc.frame_scaled, k=3)

frame_scaled$계층적<- clust.member

#자치구 라벨링
frame_scaled$자치동<- rownames(frame_scaled)