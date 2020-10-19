##### WELCOME #####

### Práctica de aprendizaje No Supervisado con K-Means
#### Álvaro Barrio Hernández
#### alvaro.barrio.hernandez@gmail.com


#### Librerías ####
install.packages("ggplot2")
install.packages("plotrix")
install.packages("purrr")
install.packages("cluster")
install.packages("gridExtra")
install.packages("grid")
install.packages("NbClust")
install.packages("factoextra")
install.packages("tinytex")
install.packages("tidyverse")
install.packages("umap")
install.packages("readr")
install.packages("Rtsne")

library(umap)
library(ggplot2)
library(plotrix)
library(purrr)
library(cluster) 
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
library(tinytex)
library(tidyverse)
library(readr)
library(Rtsne)



#### 1. Primer visualizado de los datos ####

#Carga de datos
customer_data=read.csv("Mall_Customers.csv")

#Información Básica
str(customer_data)
names(customer_data)
head(customer_data)
summary(customer_data)

#Abrir una nueva ventana con los datos
View(customer_data) 




#### 2. Visualización ####


#### 2.1 Género (Gender) ####

#Gráfico de Barras
ggplot(data = customer_data, aes(Gender)) +
  geom_bar(aes(fill = Gender)) + 
  theme_minimal() +
  ggtitle("Gráfico de barras por Género") +
  xlab("Contador") + 
  ylab("Género") +
  geom_text(stat='count', aes(x = Gender, label = ..count..), vjust = -0.5)

#Piechart 
ggplot(customer_data, aes(x=factor(1), fill=Gender))+
  geom_bar(width = 1)+
  coord_polar("y") +
  geom_text(stat='count', aes(x = Gender, label = ..count..), vjust = -14)


#### 2.2 Edad (Age) ####
summary(customer_data$Age)

#Histograma
ggplot(data = customer_data, aes(Age)) +
  geom_histogram(aes(Age), binwidth = 1, color = "blue", fill = "gold") + 
  theme_minimal() +
  ggtitle("Histograma de frecuencia por Edad") +
  xlab("Edad") + 
  ylab("Frecuencia") +
  geom_text(stat='count', aes(x = Age, label = ..count..), vjust = -1)

#Histograma con Género
ggplot(data = customer_data, aes(Age)) +
  geom_histogram(aes(Age, fill = Gender), binwidth = 1, color = "blue") + 
  theme_minimal() +
  ggtitle("Histograma de frecuencia por Edad") +
  xlab("Edad") + 
  ylab("Frecuencia") +
  geom_text(stat='count', aes(x = Age, label = ..count..), vjust = -1)

#Boxplot
ggplot(customer_data, aes(Age))+
  geom_boxplot(aes(Age), color = "blue", fill = "gold") +
  theme_minimal() +
  ggtitle("Boxplot") +
  xlab("Age") +
  coord_flip() 


#### 2.3 Ingresos Anuales (Annual Income) ####
summary(customer_data$Annual.Income..k..)

#Histograma
ggplot(data = customer_data, aes(Annual.Income..k..)) +
  geom_histogram(aes(Annual.Income..k..), binwidth = 10, color = "blue", fill = "gold") + 
  theme_minimal() +
  ggtitle("Histograma para Ingresos Anuales") +
  xlab("Ingreso Anual") + 
  ylab("Frecuencia") +
  geom_text(stat='count', aes(x = Annual.Income..k.., label = ..count..), vjust = -1)

#Histograma añadiendo Género
ggplot(data = customer_data, aes(Annual.Income..k..)) +
  geom_histogram(aes(Annual.Income..k.., fill = Gender), binwidth = 10, color = "blue") + 
  theme_minimal() +
  ggtitle("Histograma para Ingresos Anuales") +
  xlab("Ingreso Anual") + 
  ylab("Frecuencia")

#Densidad
ggplot(customer_data) +
  geom_density(aes(Annual.Income..k..), fill = 'steelblue') +
  xlim(-5, 170)

#Densidad añadiendo Género
ggplot(customer_data) +
  geom_density(aes(Annual.Income..k.., fill = Gender)) +
  xlim(-5, 170)

#Densidad añadiendo Género por separado
ggplot(customer_data) +
  geom_density(aes(Annual.Income..k.., fill = Gender)) +
  facet_grid(Gender~., scales = 'free') +
  xlim(-5, 170)


#### 2.4. Puntuación en Gasto (Spending Score)
summary(customer_data$Spending.Score..1.100.)

#Boxplot
ggplot(customer_data, aes(Spending.Score..1.100.))+
  geom_boxplot(color = "blue", fill = "gold") +
  theme_minimal() +
  ggtitle("Boxplot for Descriptive Analysis of Spending Score") +
  xlab("Age") 

#Histograma
ggplot(customer_data, aes(Spending.Score..1.100.)) +
  geom_histogram(color = "blue", fill = "gold", bins = 10 ) +
  geom_text(stat='count', aes(x = Spending.Score..1.100., label = ..count..), vjust = -1)

#Histograma con Género
ggplot(customer_data) + 
  geom_histogram(aes(Spending.Score..1.100., fill = Gender), color = "blue", bins = 10) 




#### 3. Algoritmo de k-Means ####


#### 3.1 Elbow Method ####
set.seed(333)

#Funcion para calcular la suma de cuadrados total intra-cluster
  iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")


#### 6.2 Método de Silueta ####
k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")))

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")))

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")))

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")))

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")))

k7<-kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")))

k8<-kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")))

k9<-kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")))

k10<-kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")))

#Visualizar el valor óptimo
fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")


#### 6.3 Método de Estadística de brecha ####
set.seed(333)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

#Tomamos 6
k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6



#### 4. Clústers #### 
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)

pcclust$rotation[,1:2]

#Clústers
set.seed(333)

#Ingreso & Gasto
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

#Gasto & Edad
ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")



#### 5. Principal Components Analysis (PCA) & t-distributed stochastic neighbor embedding (t-sne)####
#PCA
kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))



#TSNE
tsne <- Rtsne(customer_data[,3:5], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
summary(tsne)
exeTimeTsne<- system.time(Rtsne(customer_data[,3:5], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))
## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, xlab ="K-means", ylab="classes", col=kCols(digCluster))

