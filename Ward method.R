3. Ward��s Method�� �̿��� ������ ���� �м�

#������ �ҷ����� Ȯ���ϱ�
df<-read.csv(file="C:/KJW/data intern/proj/��ó��fin/2019_����_���_����_��������_�̼�����_����_���뷮_FIN.csv", header=TRUE)
head(df)
library("factoextra")

## �� ��ȯ �۾�
# ù��° ���� �� �̸����� �����ϰ� �ͱ� ������ name�̶�� ������ �����ؼ� ����
name<- df[,1]

#������ �������� ����鼭 name ������ �� �̸����� ��������
frame<- data.frame(df, row.names=name)

#ù��° ���� ����
frame<- frame[,-1]
head(frame)

# ����ġ Ȯ��
colSums(is.na(frame))

# �����м��� �� �� ������ ô���� �ٸ� ��� 
# ū ô���� ���� ���� ������ ����� �ֵѸ� �� �־ ����ȭ�� ���� ����
# PCA �ϱ� ���� ����ȭ�Ǿ� �־�� ��
frame_scaled<- as.data.frame(scale(frame))
head(frame_scaled)

# ������ ����� �� �߰� �����ϱ�
frame_scaled$����������= frame_scaled[,4]+frame_scaled[,5]
frame_scaled$����������= frame_scaled[,10]+frame_scaled[,11]+frame_scaled[,12]
frame_scaled$�ʹ̼��̼���= frame_scaled[,13]+frame_scaled[,14]
head(frame_scaled)

# ������ Ȯ��
cor.test(frame_scaled$�ʹ̼��̼���, frame_scaled$���뷮, method = "pearson")

# ���뷮, �ʹ̼��̼� ��
## k�� ����(���� ���� ����)
library(NbClust)
nc<-NbClust(frame_scaled[,c(6,17)],distance="euclidean",min.nc=2,max.nc=5,method="average")
#According to the majority rule, the best number of clusters is  3

## �Ÿ���� ���ϱ�
d.mat<- dist(frame_scaled[,c(6,17)], method="euclidean")
as.matrix(d.mat)[1:4,] #�Ÿ� ��� ���

## ������ Ŭ�����͸� (Ward's Method �̿�)
# ����α׷� �ð�ȭ
hc.frame_scaled<- hclust(d.mat, "ward.D")
plot(hc.frame_scaled, hang=-1, cex=1)

## k���� 3�� �ϴ� ���� ���ڴٰ� �Ǵ������� 3���� �������� �����͸� ����
clust.member<- cutree(hc.frame_scaled, k=3)
clust.member #����1, ����2, ����3
rect.hclust(hc.frame_scaled, k=3)

frame_scaled$Ward.���뷮.�ʹ̼��̼���<- clust.member

#��ġ�� �󺧸�
frame_scaled$��ġ��<- rownames(frame_scaled)

#��Ŭ�����͸�
3. Ward��s Method�� �̿��� ������ ���� �м�
#������ �ҷ����� Ȯ���ϱ�
df<-read.csv(file="C:/KJW/data intern/proj/��ó��fin/����_����_���19��_����_��_����_�����α�_fin_.csv", header=TRUE)
head(df)

## �� ��ȯ �۾�
# �ι�° ��(��)�� �� �̸����� �����ϰ� �ͱ� ������ name�̶�� ������ �����ؼ� ����
name<- df[,2]

#������ �������� ����鼭 name ������ �� �̸����� ��������
frame<- data.frame(df, row.names=name)
#ù��° ��(��)�� ����
frame<- frame[,-1]
#�ι�° ��(��)�� ����
frame<- frame[,-1]
head(frame)

# ������ ����� �� �߰� �����ϱ�
frame$�������α���= frame[,2]+frame[,3]
head(frame)

# ����ġ Ȯ��
colSums(is.na(frame))

# �����м��� �� �� ������ ô���� �ٸ� ��� 
# ū ô���� ���� ���� ������ ����� �ֵѸ� �� �־ ����ȭ�� ���� ����
# PCA �ϱ� ���� ����ȭ�Ǿ� �־�� ��
frame_scaled<- as.data.frame(scale(frame))
head(frame_scaled)

write.csv(frame_scaled, "C:/KJW/data intern/proj/��ó��fin/�����α�_�̸����_����ȭ.csv")

# ���� Ŭ�����Ͱ��� ���ϱ� NbClust ��Ű�� �̿�
library(NbClust)
nc<-NbClust(frame_scaled[,c(6,7)],distance="euclidean",min.nc=2,max.nc=5,method="average")

## �Ÿ���� ���ϱ�
d.mat<- dist(frame_scaled[,c(6,7)], method="euclidean")
as.matrix(d.mat)[1:4,] #�Ÿ� ��� ���

## ������ Ŭ�����͸� (Ward's Method �̿�)
# ����α׷� �ð�ȭ
hc.frame_scaled<- hclust(d.mat, "ward.D")
plot(hc.frame_scaled, hang=-1, cex=1)

## k���� 3�� �ϴ� ���� ���ڴٰ� �Ǵ������� 3���� �������� �����͸� ����
clust.member<- cutree(hc.frame_scaled, k=3)
clust.member #����1, ����2, ����3
rect.hclust(hc.frame_scaled, k=3)

frame_scaled$������<- clust.member

#��ġ�� �󺧸�
frame_scaled$��ġ��<- rownames(frame_scaled)