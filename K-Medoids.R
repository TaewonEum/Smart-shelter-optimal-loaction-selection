2. K-Medoids
## PAM (K-Medoids)
library(cluster)
pamframe<- pam(frame_scaled[,c(6,17)],3)
plot(pamframe, main="X��: ���뷮, Y��: �ʹ̼�����+�̼����������� k-medoids(k=3)�����м��� ���")
summary(pamframe, parameters = TRUE)

# �ð�ȭ
library("factoextra")
fviz_cluster(pamframe, geom = "point", ellipse.type = "norm")

# �� ��ü�� ��� �������� �з��Ǿ����� Ȯ��
pamframe$clustering
frame_scaled$kmedoids<- pamframe$clustering

#��Ŭ�����͸�
2. K-Medoids
## PAM (K-Medoids)
library(cluster)
pamframe<- pam(frame_scaled[,c(6,7)],3)
plot(pamframe, main="X��: �����α���, Y��: �̼������������α��� k-medoids(k=3)�����м��� ���")
summary(pamframe, parameters = TRUE)

# �ð�ȭ
library("factoextra")
fviz_cluster(pamframe, geom = "point", ellipse.type = "norm")

# �� ��ü�� ��� �������� �з��Ǿ����� Ȯ��
pamframe$clustering
frame_scaled$kmedoids<- pamframe$clustering