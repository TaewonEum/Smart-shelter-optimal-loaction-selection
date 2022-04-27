1. Gaussian Mixture Method
## Mclust (Mixture of Gaussian)
#install.packages ("mclust")
# E= 'equal', v= 'variable', I= 'coordinate axes'
library(mclust)
mc <-Mclust(frame_scaled[,c(6,17)], G=3)
summary(mc, parameters = TRUE)

#Mclust EEI (diagonal, equal volume and shape) model with 3 components
plot.Mclust(mc)
library("factoextra")
fviz_cluster(mc, geom = "point", ellipse.type = "norm")
mc

# 각 개체가 어느 군집으로 분류되었는지 확인
mc$classification

frame_scaled$gaussian.mix<- mc$classification

frame_scaled

#행정동 선정을 위한 클러스터링
4. Gaussian Mixture Method
## Mclust (Mixture of Gaussian)
#install.packages ("mclust")
library(mclust)
mc <-Mclust(frame_scaled[,c(6,7)], G=3)
summary(mc, parameters = TRUE)

#Mclust VEI (diagonal, equal shape) model with 3 components:  
plot.Mclust(mc)
library("factoextra")
fviz_cluster(mc, geom = "point", ellipse.type = "norm")
mc

# 각 개체가 어느 군집으로 분류되었는지 확인
mc$classification

frame_scaled$gaussian.mix<- mc$classification
frame_scaled