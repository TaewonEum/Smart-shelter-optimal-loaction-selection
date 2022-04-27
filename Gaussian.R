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

# �� ��ü�� ��� �������� �з��Ǿ����� Ȯ��
mc$classification

frame_scaled$gaussian.mix<- mc$classification

frame_scaled

#������ ������ ���� Ŭ�����͸�
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

# �� ��ü�� ��� �������� �з��Ǿ����� Ȯ��
mc$classification

frame_scaled$gaussian.mix<- mc$classification
frame_scaled