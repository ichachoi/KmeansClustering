#install package
install.packages("factoextra")
install.packages("tidyverse")
install.packages("fpc")
install.packages("NbClust")
library(cluster) 
library(factoextra)
library(tidyverse)
library(fpc)
library(NbClust)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(psych)

#untuk import data exell ke R
datakm=read.delim("clipboard")
#cek data setelah import
datakm
view(datakm)

#melihat tabel setelah di isi dengan mean
View(datakm)

#mengisi data yang kosong (NA) dengan menghapus data yang kosong tersebut
#coba=na.omit(data)

#standarisasi data/transformasi dengan scale
datafix=scale(datakm)
datafix=scale(datakm[, 3:15])
View(datafix)

#clust yg optimal
fviz_nbclust(datafix, kmeans, method = "silhouette") # metode silhouette
fviz_nbclust(datafix, kmeans, method = "wss")
geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
fviz_nbclust(datafix, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)
  
set.seed(123)
gap_stat <- clusGap(datafix, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 500) # metode gap statistic
fviz_gap_stat(gap_stat)

# K-means clustering
set.seed(123)
km.res <- kmeans(datafix, 4, iter.max = 100, nstart = 2)
km.res 

summary(datafix)


#get cluster means
aggregate(ddatafix, by=list(km.res$cluster), FUN=mean)

ggplot(datafix, aes(Kategori.Harga, Stok.1, Stok.2,Stok.3,Stok.4,Stok.5,Stok.6,Stok.7,
                    Stok.8,Stok.9,Stok.10,Stok.11,Stok.12))+geom_point()

#mengembalikan nilai centroid
datakm %>%
  mutate(Klaster = km.res$cluster) %>%
  group_by(Klaster) %>%
  summarise(Mean_Kategori.Harga = mean(Kategori.Harga),Mean_Stok.1 = mean(Stok.1), 
            Mean_Stok.2 = mean(Stok.2),Mean_Stok.3 = mean(Stok.3),Mean_Stok.4 = mean(Stok.4),
            Mean_Stok.5 = mean(Stok.5),Mean_Stok.6 = mean(Stok.6),Mean_Stok.7 = mean(Stok.7),
            Mean_Stok.8 = mean(Stok.8),Mean_Stok.9 = mean(Stok.9),Mean_Stok.10 = mean(Stok.10),
            Mean_Stok.11 = mean(Stok.11),Mean_Stok.12 = mean(Stok.12))


hasil <- data.frame(kh = Kategori.Harga, stok1 = Mean_Stok.1)

#melihat nilai standar deviasi
describeBy(datafix$IQ, datafix$Kategori.Harga)

#melihat hasil klastering
print(km.res)

# Visualize kmeans clustering
fviz_cluster(km.res, datafix, ellipse.type = "norm")+
  theme_minimal()

fviz_cluster(km.res, datafix,
             palette = c("pastel2"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

# Visualize silhouhette information
require("cluster")
sil <- silhouette(km.res$cluster, dist(datafix))
fviz_silhouette(sil)

el <- wss(km.res$cluster, dist(datafix))
fviz_wss(el)
#untuk melihat index silhoutte
summary(sil)

si.sumkmean <- summary(sil)

#average silhoutte setiap cluster
si.sumkmean$clus.avg.widths

#total average silhouette
si.sumkmean$avg.width

#ukuran setiap cluster
si.sumkmean$clus.sizes

# Identify observation with negative silhouette
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]
