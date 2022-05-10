# loading packages
library(readxl)
library(ClusterR)
library(cluster)
library(NbClust)
library(factoextra)
library(caret)
library(ggplot2)
library(ggbiplot)
library(devtools)

wineDf <- read_excel("D:/IIT/Year 2 Sem 2/Data Mining & Machine Learning/CW/Whitewine_v2.xlsx")
dim(wineDf)
boxplot(wineDf)

#Removing outliers
withoutOutliers <- wineDf
removeOutlier <- function(wineCol){
  return (subset(withoutOutliers, wineCol > ((quantile(wineCol, .25)) - 1.5*(IQR(wineCol))) & wineCol < ((quantile(wineCol, .75)) + 1.5*(IQR(wineCol)))))  
}

withoutOutliers <- removeOutlier(withoutOutliers$`fixed acidity`)
withoutOutliers <- removeOutlier(withoutOutliers$`volatile acidity`)
withoutOutliers <- removeOutlier(withoutOutliers$`citric acid`)
withoutOutliers <- removeOutlier(withoutOutliers$`residual sugar`)
withoutOutliers <- removeOutlier(withoutOutliers$`chlorides`)
withoutOutliers <- removeOutlier(withoutOutliers$`free sulfur dioxide`)
withoutOutliers <- removeOutlier(withoutOutliers$`total sulfur dioxide`)
withoutOutliers <- removeOutlier(withoutOutliers$`density`)
withoutOutliers <- removeOutlier(withoutOutliers$`pH`)
withoutOutliers <- removeOutlier(withoutOutliers$`sulphates`)
withoutOutliers <- removeOutlier(withoutOutliers$`alcohol`)
dim(withoutOutliers)
boxplot(withoutOutliers)

#Normalization 
wineDf.Norm <- as.data.frame(scale(withoutOutliers))
boxplot(wineDf.Norm)

# NBClust method
print(dim(wineDf.Norm))
clusterNo=NbClust(wineDf.Norm,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

#Elbow method
k = 2:10
WSS = sapply(k, function(k) {kmeans(wineDf.Norm, centers=k)$tot.withinss})
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

# Silhouette method
fviz_nbclust(wineDf.Norm, kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")

set.seed(23)
# Clustering for k = 2
kmeans.2 <- kmeans(wineDf.Norm, centers = 2, nstart = 20)
kmeans.2 # Finding WSS
fviz_cluster(kmeans.2, data = wineDf.Norm,
             palette = c("#636A2B", "#63E5FF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)             
cm.2 <- table(wineDf.Norm$quality, kmeans.2$cluster)
cm.2
qColumn.2<-factor(wineDf.Norm$quality)
quality.2<-as.numeric(qColumn.2)
confusionMatrix(data=as.factor(c(as.factor(kmeans.2$cluster))), reference=as.factor(quality.2))

#WSS, BSS and TSS
kmeans.2$withinss
kmeans.2$betweenss
kmeans.2$totss

# Clustering for k = 3
kmeans.3 <- kmeans(wineDf.Norm, centers = 3, nstart = 20)
kmeans.3 # Finding WSS
fviz_cluster(kmeans.3, data = wineDf.Norm,
             palette = c("#636A2B", "#63E5FF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)             
kmeans.3$cluster
cm.3 <- table(wineDf.Norm$quality, kmeans.3$cluster)
cm.3
qColumn.3<-factor(wineDf.Norm$quality)
quality.3<-as.numeric(qColumn.3)
confusionMatrix(data=as.factor(c(as.factor(kmeans.3$cluster))), reference=as.factor(quality.3))

#WSS, BSS and TSS
kmeans.3$withinss
kmeans.3$betweenss
kmeans.3$totss

# Clustering for k = 4
kmeans.4 <- kmeans(wineDf.Norm, centers = 4, nstart = 20)
kmeans.4 #Finding WSS
fviz_cluster(kmeans.4, data = wineDf.Norm,
             palette = c("#636A2B", "#63E5FF", "#E7B800", "#800000"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)             
cm.4 <- table(wineDf.Norm$quality, kmeans.4$cluster)
cm.4
qColumn.4<-factor(wineDf.Norm$quality)
quality.4<-as.numeric(qColumn.4)
confusionMatrix(data=as.factor(c(as.factor(kmeans.4$cluster))), reference=as.factor(quality.4))

#WSS, BSS and TSS
kmeans.4$withinss
kmeans.4$betweenss
kmeans.4$totss

# Principle Component Analysis
wineDf.pca <- prcomp(wineDf.Norm[,c(1:11)], center = TRUE, scale. = TRUE)
summary(wineDf.pca)
stn.Devn <- wineDf.pca$sdev
prin.Vari <- stn.Devn^2
prin.Vari[1:11]
prop.Varex <- prin.Vari/sum(prin.Vari)
prop.Varex

#Printing the biplot
ggbiplot(wineDf.pca, labels=rownames(wineDf.Norm$quality))

#Printing the scree plot
plot(prop.Varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#Printing the cumulative scree plot
plot(cumsum(prop.Varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

wine.pca.as_df <- as.data.frame(wineDf.pca$x[,1:9])
summary(wineDf.pca)

# PCA Clustering for k = 2
pca.kmeans <- kmeans(wine.pca.as_df, centers = 2, nstart = 20)
pca.kmeans
fviz_cluster(pca.kmeans, data = wine.pca.as_df,
             palette = c("#636A2B", "#63E5FF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
pca.cm <- table(wineDf.Norm$quality, pca.kmeans$cluster)
pca.cm
pcacolumn<-factor(wineDf.Norm$quality)
quality_numeric<-as.numeric(pcacolumn)
confusionMatrix(data=as.factor(c(as.factor(pca.kmeans$cluster))), reference=as.factor(quality_numeric))

#WSS, BSS and TSS for PCA
pca.kmeans$withinss
pca.kmeans$betweenss
pca.kmeans$totss
