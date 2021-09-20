library(readxl)
library(factoextra)
Data_dillah <- read_excel("D:/FILE KULIAH/SEMESTER 8/Dillah/DATA KESEHATAN SULSEL 2017.xlsx",
                           col_types = c("skip","numeric","numeric", "numeric",
                                         "numeric","numeric","numeric","numeric")) 
Data_dillah
summary(Data_dillah)
View(Data_dillah)
#Uji KMO 
KMO <- function (x)
{ 
  x <- subset(x,complete.cases(x))
  r <- cor(x)
  r2 <- r^2 
  i <- solve(r)
  d <- diag (i)
  p2 <- (-i/sqrt(outer(d,d)))^2
  diag(r2) <- diag(p2) <- 0
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  return(list(KMO=KMO)) 
}
KMO(Data_dillah) #menampilkan nilai KMO
#Jumlah Cluster Optimal
fviz_nbclust(Data_dillah, FUN = hcut, method = "silhouette")
#menghitung korelasi variable 
R<-cor(Data_dillah) 
R # menampilkan korelasi variable
View(R)

#Melakukan PCA mengatasi Multikolinearitas 
PCA<-prcomp(Data_dillah)
PCA
Variansi<-PCA$sdev^2
Variansi
summary(PCA)
PCA_scores<-PCA$x[,1:2]
PCA_scores

#membuat matriks jarak (D)
D<-dist(PCA_scores,method = "euclidean")
D^2
#Analisis Cluster 
library(cluster)
kluster_hirarki<-hclust(D^2,method = "ward.D")
str(as.dendrogram(kluster_hirarki)) #proses average
dendrogram<-plot(kluster_hirarki) #Menampilkan dendrogram
#menentukan banyaknya kelompok
group<-cutree(kluster_hirarki,2)
kelompok<-cbind(group)
kelompok
plot2 <- plot(kluster_hirarki,hang=-1,col='black',
              main='Cluster Dendrogram WARD ',
              sub='',xlab='Indeks Kabupaten/Kota',ylab='Indeks Jarak')
rect.hclust(kluster_hirarki, k=2, border= 2:5)

#Validasi cluster dengan silhouette (ward)
sil <- silhouette(kelompok, dist(D^2))
fviz_silhouette(sil)
