#kmeans clustering
USArrests
us_arrest1=USArrests
str(us_arrest1)

install.packages("tidyverse") #data manipulation
install.packages("cluster") #clusterting
install.packages("factoextra") #cluster plot
library(tidyverse)
library(cluster)
library(factoextra)
library(caTools)

split_us=sample.split(us_arrest1,SplitRatio = 0.8)
tr_us=subset(us_arrest1,split_us=="TRUE")
ts_us=subset(us_arrest1,split_us=="FALSE")
tr_us=scale(tr_us)
k_means_cluster=kmeans(tr_us,centers = 2,nstart = 25)
k_means_cluster

#ploting cluster
fviz_cluster(k_means_cluster,data = tr_us)
k3=kmeans(tr_us,centers = 3,nstart = 25)
k4=kmeans(tr_us,centers = 4,nstart = 25)
k5=kmeans(tr_us,centers = 5,nstart = 25)
#plot to compare
p1=fviz_cluster(k_means_cluster,geom = "points",data = tr_us)+ggtitle("k=2")
p2=fviz_cluster(k3,geom = "points",data = tr_us)+ggtitle("k=3")
p3=fviz_cluster(k4,geom = "points",data = tr_us)+ggtitle("k=4")
p4=fviz_cluster(k5,geom = "points",data = tr_us)+ggtitle("k=5")
library(gridExtra)
tr_us
grid.arrange(p1,p2,p3,p4,nrow=2)
pre_us=(k_means_cluster$cluster)
pre_us
#Elbow method
Elbow=function(k){
  kmeans(tr_us,k,nstart = 10)$tot.withinss
 
}
#k=1 to k=15
k.values=c(1:15)
k.values
Elbow_values=map_dbl(k.values,Elbow)
Elbow_values
plot(k.values,Elbow_values,xlab="number of cluster",ylab="total withins",col="blue",type="b")
library(ggplot2)
qplot(k.values,Elbow_values,col="red",xlab = "number of clusters",ylab = "total withinss")
Elbow(4)

