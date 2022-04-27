1. K-Means
###kmeans-cluster
###교통량, 미세먼지  초미세먼지
library(ggplot2)
library(NbClust)
군집final=클러스터링[,c(7,14,15)]
head(군집final)
군집final$미세먼지표준=scale(군집final$미세먼지)
군집final$초미세먼지표준=scale(군집final$초미세먼지)
군집final$교통량표준=scale(군집final$교통량)
군집final=군집final[,-c(1,2,3)]
군집final$미세먼지총합=군집final$미세먼지표준+군집final$초미세먼지표준
head(군집final)
군집final=군집final[,c(3,4)]

nc <- NbClust(군집final, min.nc = 2, max.nc = 5, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
kmeans <- kmeans(군집final, centers = 3, iter.max = 10000)
kmeans$centers
군집final$cluster <- as.factor(kmeans$cluster)
SP=qplot(교통량표준, 미세먼지총합, colour = cluster, data = 군집final,main="k-means 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)
SP=qplot(교통량표준, 미세먼지총합, colour = ward, data = 군집final,main="ward 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)
SP=qplot(교통량표준, 미세먼지총합, colour = medois, data = 군집final,main="k-medois 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)
SP=qplot(교통량표준, 미세먼지총합, colour = gaussian, data = 군집final,main="gaussian 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)

#동 클러스터링
1. K-Means
#동클러스터링
군집final=클러스터링[,c(14,15)]
head(군집final)
군집final$유동인구표준화=scale(군집final$유동인구표준화)
군집final$취약계층표준화=scale(군집final$취약계층표준화)

군집final=군집final[,c(14,15)]
head(군집final)

nc <- NbClust(군집final, min.nc = 2, max.nc = 5, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

kmeans <- kmeans(군집final, centers = 3, iter.max = 10000)
kmeans$centers

군집final$cluster <- as.factor(kmeans$cluster)
군집final$행정구역=클러스터링$행정구역

SP=qplot(유동인구표준화, 취약계층효준화, colour = cluster, data = 군집final,main="k-means 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)

SP=qplot(유동인구표준화, 취약계층효준화, colour = ward, data = 군집final,main="ward 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)

SP=qplot(유동인구표준화, 취약계층효준화, colour = mdeois, data = 군집final,main="k-medois 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)

SP=qplot(유동인구표준화, 취약계층효준화, colour = gaussian, data = 군집final,main="gaussian 군집분석결과")
SP+ geom_text(aes(label=행정구역), size=4, vjust=0)
