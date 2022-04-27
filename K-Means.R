1. K-Means
###kmeans-cluster
###���뷮, �̼�����  �ʹ̼�����
library(ggplot2)
library(NbClust)
����final=Ŭ�����͸�[,c(7,14,15)]
head(����final)
����final$�̼�����ǥ��=scale(����final$�̼�����)
����final$�ʹ̼�����ǥ��=scale(����final$�ʹ̼�����)
����final$���뷮ǥ��=scale(����final$���뷮)
����final=����final[,-c(1,2,3)]
����final$�̼���������=����final$�̼�����ǥ��+����final$�ʹ̼�����ǥ��
head(����final)
����final=����final[,c(3,4)]

nc <- NbClust(����final, min.nc = 2, max.nc = 5, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
kmeans <- kmeans(����final, centers = 3, iter.max = 10000)
kmeans$centers
����final$cluster <- as.factor(kmeans$cluster)
SP=qplot(���뷮ǥ��, �̼���������, colour = cluster, data = ����final,main="k-means �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)
SP=qplot(���뷮ǥ��, �̼���������, colour = ward, data = ����final,main="ward �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)
SP=qplot(���뷮ǥ��, �̼���������, colour = medois, data = ����final,main="k-medois �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)
SP=qplot(���뷮ǥ��, �̼���������, colour = gaussian, data = ����final,main="gaussian �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)

#�� Ŭ�����͸�
1. K-Means
#��Ŭ�����͸�
����final=Ŭ�����͸�[,c(14,15)]
head(����final)
����final$�����α�ǥ��ȭ=scale(����final$�����α�ǥ��ȭ)
����final$������ǥ��ȭ=scale(����final$������ǥ��ȭ)

����final=����final[,c(14,15)]
head(����final)

nc <- NbClust(����final, min.nc = 2, max.nc = 5, method = "kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

kmeans <- kmeans(����final, centers = 3, iter.max = 10000)
kmeans$centers

����final$cluster <- as.factor(kmeans$cluster)
����final$��������=Ŭ�����͸�$��������

SP=qplot(�����α�ǥ��ȭ, ������ȿ��ȭ, colour = cluster, data = ����final,main="k-means �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)

SP=qplot(�����α�ǥ��ȭ, ������ȿ��ȭ, colour = ward, data = ����final,main="ward �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)

SP=qplot(�����α�ǥ��ȭ, ������ȿ��ȭ, colour = mdeois, data = ����final,main="k-medois �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)

SP=qplot(�����α�ǥ��ȭ, ������ȿ��ȭ, colour = gaussian, data = ����final,main="gaussian �����м����")
SP+ geom_text(aes(label=��������), size=4, vjust=0)