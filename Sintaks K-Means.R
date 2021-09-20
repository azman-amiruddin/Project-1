library(readxl)
library(factoextra)
library(ggplot2)
library(moments)
library(fpc)
library(cluster)
data_skripsi_dillah <- read_excel("D:/FILE KULIAH/SEMESTER 8/Dillah/DATA KESEHATAN SULSEL 2017.xlsx",
                                col_types = c("skip", "numeric","numeric", "numeric", "numeric", 
                                              "numeric","numeric", "numeric")) 
View(data_skripsi_dillah)
summary(data_skripsi_dillah)
x1 <- data_skripsi_dillah$X1
x2 <- data_skripsi_dillah$X2
x3 <- data_skripsi_dillah$X3
x4 <- data_skripsi_dillah$X4
x5 <- data_skripsi_dillah$X5
x6 <- data_skripsi_dillah$X6
x7 <- data_skripsi_dillah$X7
skewness(x1)
kurtosis(x1)
skewness(x2)
kurtosis(x2)
skewness(x3)
kurtosis(x3)
skewness(x4)
kurtosis(x4)
skewness(x5)
kurtosis(x5)
skewness(x6)
kurtosis(x6)
skewness(x7)
kurtosis(x7)

#Jumlah cluster optimal
silhouette_score <- function(k){
  km <- kmeans(data_skripsi_dillah, k)
  ss <- silhouette(km$cluster, dist(data_skripsi_dillah))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, 
     xlab='Number of clusters', 
     ylab='Average Silhouette Scores', frame=FALSE)
fviz_nbclust(data_skripsi_dillah,kmeans, method = "silhouette")

#Melakukan PCA mengatasi Multikolinearitas 
PCA<-prcomp(data_skripsi_dillah)
PCA
Variansi<-PCA$sdev^2
Variansi
summary(PCA)
PCA_scores<-PCA$x[,1:2]
PCA_scores

#membuat matriks jarak (D)
D<-dist(PCA_scores,method = "euclidean")
D^2
centers <- PCA_scores[sample(nrow(PCA_scores), 5),]
centers
#Inisialisasi Centroid Cluster 
#Hasil Cluster 
clustering <- kmeans(PCA_scores, centers=2, iter.max =1)
clustering$cluster
plotcluster(PCA_scores, clustering$cluster) 
points(clustering$centers, col = 1:2, pch = 8, cex = 2)

centers <- PCA_scores[sample(nrow(PCA_scores), 5),]
centers
#Inisialisasi Centroid Cluster 
clustering$centers
#Hasil Cluster 
clustering <- kmeans(data_skripsi_dillah, centers=2, iter.max =10)
clustering$cluster
plotcluster(data_skripsi_dillah, clustering$cluster) 
points(clustering$centers, col = 1:2, pch = 8, cex = 2)

clustering$size
clustering$withinss
clustering$betweenss

#Validasi cluster dengan silhouette (K-Means)
sil <- silhouette(clustering$cluster, dist(data_skripsi_dillah))
fviz_silhouette(sil)
