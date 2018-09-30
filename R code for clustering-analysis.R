install.packages("HSAUR2")
install.packages("scatterplot3d")
install.packages("dbscan")
install.packages("fpc")
install.packages("factoextra")
install.packages("mclust")
install.packages("mlbench")
## Question 1 i)

library("HSAUR2")

library(scatterplot3d)

with(planets, {
                scatterplot3d(mass, period, eccen,
                color="blue", pch=19,
                type="h", 
                main="3-D Scatterplot planets dataset",
                xlab="mass (relative to Jupiter)",
                ylab="period (days)",
                zlab="radial eccentricity")})


## Question 1 ii)

summary(planets)

## Question 1 iii)

planets2 <- log1p(planets)  ## i used log1p xhich computes log(1+x)

## Question 1 iv)


library (dbscan)

kNNdistplot(planets2[1:3], k=7)
abline(h = 0.70, lty = 2)

#The aim of this plot is to determine the “knee”, which corresponds to the optimal eps parameter.
#It can be seen that the optimal eps value is around a distance of 0.70 as the the
#gradient of the slope starts to sharpen abruptly.


## Question 1 v)

res <- dbscan(planets2, 0.70, 7)
print(res)

#The algorithm created 3 clusters and 9 noise points (they dont belong to any of the clusters)
#The first cluster consists of 19 planets , the second has 66 and the third 7 planets 

## Question 1 vi)

#library(factoextra)
#fviz_cluster(res, planets2, geom = "point")  


res$cluster <- as.factor(res$cluster)
res$cluster2 <- as.factor(res$cluster)
colors <- c("black", "red", "blue","green")
colors <- colors[as.numeric(res$cluster)]

with(planets2, {
  scatterplot3d(mass, period, eccen, 
                color=colors, pch=19,
                grid=TRUE,
                box=TRUE,
                angle=75,
                main="3-D Scatterplot of clusters (planets2 dataset)",
                xlab="mass (relative to Jupiter)",
                ylab="period (days)",
                zlab="radial eccentricity")})

legend('bottomright' , legend = levels(res$cluster2),
       col = c("black", "red", "blue","green"), horiz=FALSE,pch = 19)

## Question 1 vii)

library(mclust)


clPairs(planets2[1:3], cl = res$cluster,symbols = 16, col = c("black","red","blue","green"),lower.panel = NULL )

clPairsLegend("bottomleft", class = levels(res$cluster), 
                        col = c("black","red","blue","green"),
                        pch = 20, 
                        cex=0.8,
                        horiz=TRUE,
                        title = "Clusters for Planets2 DataSet",
                        text.font=4, 
                        bg='lightgrey')




##  Cluster 1 consists of planets with small mass, very short periods and in general small radial eccentricity
##  Cluster 2 consists of planets with the same mass as cluster 1 but slighltly larger periods
##  Cluster 3 contains planets with very large mass and periods
## *keep in mind that mass , periods and radial eccentricity are the log-transformed output(log1+x) of the real variables 

## Question 2 i)

library(mclust)


res2 <- Mclust(planets)
summary(res2)


## Question 2 ii)

summary(res2 , parameters = TRUE)

## In this case the best model according to BIC is a non equal covariance model with 3 components or clusters.
## The three components or clusters are diagonal with varying volume and shape

## Question 2 iii)

plot(res2 , what="BIC")

plot(res2 , what="BIC" , ylim =c(-2300 , -1800))

## The best model is selected using the Bayesian Information Criterion or BIC. 
## A large BIC score indicates strong evidence for the corresponding model.
## As it can be observed from the second plot the VVI model (varying volume and shape and the orientation is the identity (I) or “coordinate axes.)
## with 3 components o clusters as the  bic value is the largest among all the other models.

## Question 2 iv)


clPairs(planets, cl=res2$classification,symbols = 16, col = c("green","red","blue"),lower.panel = NULL )

res2$classificationleveles <- as.factor(res2$classification)

clPairsLegend("bottomleft", class = levels(res2$classificationleveles), 
              col = c("green","red","blue"),
              pch = 20, 
              cex=0.8,
              horiz=TRUE,
              title = "Clusters for Planets DataSet",
              text.font=4, 
              bg='lightgrey')

## Cluster 1 consists of planets with almost equal size with Jupite very short periods and radial eccentricity
## Cluster 2 consists of planets with larger mass and periods.
## Cluster 3 contains planets with much larger periods and in general larger mass 
## The radial eccentricity seems to be the same among cluster 2 and cluster 3

## Question 2 v)

colors2 <- c("green","red","blue")
colors2 <- colors2[as.numeric(res2$classification)]

with(planets, {
  scatterplot3d(mass, period, eccen, 
                color=colors2, pch=19, 
                grid=TRUE,
                box=TRUE,
                angle=75,
                main="3-D Scatterplot of clusters (planets dataset)",
                xlab="mass (relative to Jupiter)",
                ylab="period (days)",
                zlab="radial eccentricity")})

legend("bottomright" , legend = levels(res2$classificationleveles),
       col = c("green","red","blue"), horiz=FALSE,pch = 19)

##   The planets2 is the log-transformed (log1+x) dataset of the initial dataset (planets)
## and so there is much more variability in the second case.
##   Different algorithms were also used for clustering
##   As a result although the number of the clusters are the same in both cases , 
## the size of the clusters and some of their charachteristics vary because of the tranformation of the data
##   The variable period seems to be the dominant variable in this clustering as it takes  much larger values than the others (mean = 666.531 and max =5360)
##   In addition the first clustering method had 9 observations that didn't belong to any of the three clusters.

## Question 3

library(factoextra)
data1 <- multishapes
library(mlbench)
set.seed(42) #
data2 <- mlbench.smiley()
data3 <- mlbench.cassini(1000)
data4 <- mlbench.circle(1000)
data5 <- mlbench.spirals(1000, cycles = 3, sd = .05)
data6 <- mlbench.threenorm(1000, d = 2)
data7 <- mlbench.shapes(1000)

## Question 3 i)

plot(data1) # 5 clusters
plot(data2) # 4 clusters
plot(data3) # 3 clusters
plot(data4) # 2 clusters
plot(data5) # 2 clusters
plot(data6) # 2 clusters
plot(data7) # 4 clusters

# convert the dataset to dataframes which makes it easier to work with
data1new <- as.data.frame(data1)
data2new <- as.data.frame(data2)
data3new <- as.data.frame(data3)
data4new <- as.data.frame(data4)
data5new <- as.data.frame(data5)
data6new <- as.data.frame(data6)
data7new <- as.data.frame(data7)

data1new <- data1new[-3]
data2new <- data2new[-3]
data3new <- data3new[-3]
data4new <- data4new[-3]
data5new <- data5new[-3]
data6new <- data6new[-3]
data7new <- data7new[-3]

## Question 3 ii)

# i will answer the following questions as follows:
# For each dataset i will use all five algorithms. Particular for hierarchical clustering algorithm i will use 
# five different linkage methods (ward, complete, single, average and centroid) using only the Euclidean distance
# Then i will plot all results 
# together and comment on the performance of the different approaches 
# assuming that the plots in the first part of this question (split by color and shape) point to the “right” clustering. 
# These are going to be the base line for all comments regarding the performance of each algorithm

library(cluster)
par(mfrow=c(3,3))

########################data2#################################

##Hierarchical Clustering
data2_eucl <- dist(data2new, method = 'euclidean')
data2_eucl_m <- as.matrix(data2_eucl)

data2_eucl_ward <- hclust(data2_eucl, method='ward.D')
data2_eucl_single <- hclust(data2_eucl, method='single')
data2_eucl_complete <- hclust(data2_eucl, method='complete')
data2_eucl_average <- hclust(data2_eucl, method='average')
data2_eucl_centroid <- hclust(data2_eucl, method='centroid')

data2_eucl_ward_groups <- cutree(data2_eucl_ward, k = 4)
data2_eucl_single_groups <- cutree(data2_eucl_single, k = 4)
data2_eucl_complete_groups <- cutree(data2_eucl_complete, k = 4)
data2_eucl_average_groups <- cutree(data2_eucl_average, k = 4)
data2_eucl_centroid_groups <- cutree(data2_eucl_centroid, k = 4)

plot(data2new , col = data2_eucl_ward_groups , pch=20 , main ="ward")
plot(data2new , col = data2_eucl_single_groups , pch=20 , main="single")
plot(data2new , col = data2_eucl_complete_groups , pch=20 , main = "complete")
plot(data2new , col = data2_eucl_average_groups , pch=20 , main ="average")
plot(data2new , col = data2_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data2new, 4, nstart=25)
plot(data2new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data2new)
#summary(fit)
plot(data2new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
#kNNdistplot(data2new, k=7)
#abline(h = 0.10, lty = 2)
resdb <- dbscan(data2new, 0.1, 7)
#print(resdb)
plot(data2new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data2new, k = 4)
plot (data2new , col = fit.kmpam$clustering , pch=20 , main="Pam")


## it seems that for the hierarchical clustering algorithm the ward and the siggle method worked really well.
## In addition the DBSCAN algorithm also managed to cluster correctly the data
## All the other methods-algorithms failed to find the correct clusters as they created separate
## clusters for the "smile" or "nose".

##############################data1################################################

par(mfrow=c(3,3))

##Hierarchical Clustering
data1_eucl <- dist(data1new, method = 'euclidean')
data1_eucl_m <- as.matrix(data1_eucl)

data1_eucl_ward <- hclust(data1_eucl, method='ward.D')
data1_eucl_single <- hclust(data1_eucl, method='single')
data1_eucl_complete <- hclust(data1_eucl, method='complete')
data1_eucl_average <- hclust(data1_eucl, method='average')
data1_eucl_centroid <- hclust(data1_eucl, method='centroid')

data1_eucl_ward_groups <- cutree(data1_eucl_ward, k = 5)
data1_eucl_single_groups <- cutree(data1_eucl_single, k = 5)
data1_eucl_complete_groups <- cutree(data1_eucl_complete, k = 5)
data1_eucl_average_groups <- cutree(data1_eucl_average, k = 5)
data1_eucl_centroid_groups <- cutree(data1_eucl_centroid, k = 5)

plot(data1new , col = data1_eucl_ward_groups , pch=20 , main ="ward")
plot(data1new , col = data1_eucl_single_groups , pch=20 , main="single")
plot(data1new , col = data1_eucl_complete_groups , pch=20 , main = "complete")
plot(data1new , col = data1_eucl_average_groups , pch=20 , main ="average")
plot(data1new , col = data1_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data1new, 5, nstart=25)

plot(data1new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data1new)
#summary(fit)
plot(data1new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
#kNNdistplot(data1new, k=5)
#abline(h = 0.15, lty = 2)
resdb <- dbscan(data1new, 0.15, 5)
#print(resdb)
plot(data1new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data1new, k = 5)
plot (data1new , col = fit.kmpam$clustering , pch=20 , main="Pam")

## The hierarchical clustering algorithm created different number of clusters using different linkage methods 
## but none of them performed a correct clustering.
## Only the DBCAN algorithm did the job for this dataset

#######################data3#######################################################

par(mfrow=c(3,3))

##Hierarchical Clustering
data3_eucl <- dist(data3new, method = 'euclidean')
data3_eucl_m <- as.matrix(data3_eucl)

data3_eucl_ward <- hclust(data3_eucl, method='ward.D')
data3_eucl_single <- hclust(data3_eucl, method='single')
data3_eucl_complete <- hclust(data3_eucl, method='complete')
data3_eucl_average <- hclust(data3_eucl, method='average')
data3_eucl_centroid <- hclust(data3_eucl, method='centroid')

data3_eucl_ward_groups <- cutree(data3_eucl_ward, k = 3)
data3_eucl_single_groups <- cutree(data3_eucl_single, k = 3)
data3_eucl_complete_groups <- cutree(data3_eucl_complete, k = 3)
data3_eucl_average_groups <- cutree(data3_eucl_average, k = 3)
data3_eucl_centroid_groups <- cutree(data3_eucl_centroid, k = 3)

plot(data3new , col = data3_eucl_ward_groups , pch=20 , main ="ward")
plot(data3new , col = data3_eucl_single_groups , pch=20 , main="single")
plot(data3new , col = data3_eucl_complete_groups , pch=20 , main = "complete")
plot(data3new , col = data3_eucl_average_groups , pch=20 , main ="average")
plot(data3new , col = data3_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data3new, 3, nstart=25)

plot(data3new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data3new)
#summary(fit)
plot(data3new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
#kNNdistplot(data3new, k=6)
#abline(h = 0.15, lty = 2)
resdb <- dbscan(data3new, 0.15, 6)
#print(resdb)
plot(data3new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data3new, k = 3)
plot (data3new , col = fit.kmpam$clustering , pch=20 , main="Pam")


## DBSCAN algorithm (with k=6 and h=0.15) and hierarchical clustering algorithm with single linkage method performed the correct clustering 
## for this dataset. Hierarchical clustering with ward linkage method as well as pam algorithm
## had a few mistakes.
## All the other algorithms failed to cluster successfully the dataset

##########################data4###################################################


par(mfrow=c(3,3))

##Hierarchical Clustering
data4_eucl <- dist(data4new, method = 'euclidean')
data4_eucl_m <- as.matrix(data4_eucl)

data4_eucl_ward <- hclust(data4_eucl, method='ward.D')
data4_eucl_single <- hclust(data4_eucl, method='single')
data4_eucl_complete <- hclust(data4_eucl, method='complete')
data4_eucl_average <- hclust(data4_eucl, method='average')
data4_eucl_centroid <- hclust(data4_eucl, method='centroid')

data4_eucl_ward_groups <- cutree(data4_eucl_ward, k = 2)
data4_eucl_single_groups <- cutree(data4_eucl_single, k = 2)
data4_eucl_complete_groups <- cutree(data4_eucl_complete, k = 2)
data4_eucl_average_groups <- cutree(data4_eucl_average, k = 2)
data4_eucl_centroid_groups <- cutree(data4_eucl_centroid, k = 2)

plot(data4new , col = data4_eucl_ward_groups , pch=20 , main ="ward")
plot(data4new , col = data4_eucl_single_groups , pch=20 , main="single")
plot(data4new , col = data4_eucl_complete_groups , pch=20 , main = "complete")
plot(data4new , col = data4_eucl_average_groups , pch=20 , main ="average")
plot(data4new , col = data4_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data4new, 2, nstart=25)

plot(data4new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data4new)
#summary(fit)
plot(data4new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
#kNNdistplot(data4new, k=5)
#abline(h = 0.1, lty = 2)
resdb <- dbscan(data4new, 0.1, 5)
#print(resdb)
plot(data4new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data4new, k = 2)
plot (data4new , col = fit.kmpam$clustering , pch=20 , main="Pam")


## It seems that none of the algorithms managed to cluster successfully the data
## Most of them divided the data in half and DBCAN did not perform any clustering (k=5 and h=0.1) 
## Once again (as for the previous datasets) the model based clustering algorithm created more clusters than it should have

###########################data5##############################################

par(mfrow=c(3,3))

##Hierarchical Clustering
data5_eucl <- dist(data5new, method = 'euclidean')
data5_eucl_m <- as.matrix(data5_eucl)

data5_eucl_ward <- hclust(data5_eucl, method='ward.D')
data5_eucl_single <- hclust(data5_eucl, method='single')
data5_eucl_complete <- hclust(data5_eucl, method='complete')
data5_eucl_average <- hclust(data5_eucl, method='average')
data5_eucl_centroid <- hclust(data5_eucl, method='centroid')

data5_eucl_ward_groups <- cutree(data5_eucl_ward, k = 2)
data5_eucl_single_groups <- cutree(data5_eucl_single, k = 2)
data5_eucl_complete_groups <- cutree(data5_eucl_complete, k = 2)
data5_eucl_average_groups <- cutree(data5_eucl_average, k = 2)
data5_eucl_centroid_groups <- cutree(data5_eucl_centroid, k = 2)

plot(data5new , col = data5_eucl_ward_groups , pch=20 , main ="ward")
plot(data5new , col = data5_eucl_single_groups , pch=20 , main="single")
plot(data5new , col = data5_eucl_complete_groups , pch=20 , main = "complete")
plot(data5new , col = data5_eucl_average_groups , pch=20 , main ="average")
plot(data5new , col = data5_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data5new, 2, nstart=25)

plot(data5new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data5new)
#summary(fit)
plot(data5new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
#kNNdistplot(data5new, k=5)
#abline(h = 0.18, lty = 2)
resdb <- dbscan(data5new, 0.18, 5)
print(resdb)
plot(data5new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data5new, k = 2)
plot (data5new , col = fit.kmpam$clustering , pch=20 , main="Pam")

## Again none of the algorithms performed the "correct" clustering as they split the spiral
## shape in half (vertically or horizontally) most of the times.
## All algorithms created two clusters (same number with the initial dataset) but the
## data points belonging to each cluster are completely different.
## The DBSCAN algorithm (k=5 , h=0.18) created 9 clusters and 12 noise points.
## In general none of the algorithms managed to follow the spiral shape


##########################data6###########################################3

par(mfrow=c(3,3))

##Hierarchical Clustering
data6_eucl <- dist(data6new, method = 'euclidean')
data6_eucl_m <- as.matrix(data6_eucl)

data6_eucl_ward <- hclust(data6_eucl, method='ward.D')
data6_eucl_single <- hclust(data6_eucl, method='single')
data6_eucl_complete <- hclust(data6_eucl, method='complete')
data6_eucl_average <- hclust(data6_eucl, method='average')
data6_eucl_centroid <- hclust(data6_eucl, method='centroid')

data6_eucl_ward_groups <- cutree(data6_eucl_ward, k = 2)
data6_eucl_single_groups <- cutree(data6_eucl_single, k = 2)
data6_eucl_complete_groups <- cutree(data6_eucl_complete, k = 2)
data6_eucl_average_groups <- cutree(data6_eucl_average, k = 2)
data6_eucl_centroid_groups <- cutree(data6_eucl_centroid, k = 2)

plot(data6new , col = data6_eucl_ward_groups , pch=20 , main ="ward")
plot(data6new , col = data6_eucl_single_groups , pch=20 , main="single")
plot(data6new , col = data6_eucl_complete_groups , pch=20 , main = "complete")
plot(data6new , col = data6_eucl_average_groups , pch=20 , main ="average")
plot(data6new , col = data6_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data6new, 2, nstart=25)

plot(data6new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data6new)
#summary(fit)
plot(data6new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
#kNNdistplot(data6new, k=7)
#abline(h = 0.4, lty = 2)
resdb <- dbscan(data6new, 0.4, 7)
#print(resdb)
plot(data6new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data6new, k = 2)
plot (data6new , col = fit.kmpam$clustering , pch=20 , main="Pam")

## Hierarchical clustering algorithm with ward and complete method
## as well as the k mean algorithm performed a quite good clustering 
## although there are some misclassifications.
##DBSCAN did not create any cluster and model based clustering algorithm
## created 3 clusters instead of two

######################3data7###############################################

par(mfrow=c(3,3))

##Hierarchical Clustering
data7_eucl <- dist(data7new, method = 'euclidean')
data7_eucl_m <- as.matrix(data7_eucl)

data7_eucl_ward <- hclust(data7_eucl, method='ward.D')
data7_eucl_single <- hclust(data7_eucl, method='single')
data7_eucl_complete <- hclust(data7_eucl, method='complete')
data7_eucl_average <- hclust(data7_eucl, method='average')
data7_eucl_centroid <- hclust(data7_eucl, method='centroid')

data7_eucl_ward_groups <- cutree(data7_eucl_ward, k = 4)
data7_eucl_single_groups <- cutree(data7_eucl_single, k = 4)
data7_eucl_complete_groups <- cutree(data7_eucl_complete, k = 4)
data7_eucl_average_groups <- cutree(data7_eucl_average, k = 4)
data7_eucl_centroid_groups <- cutree(data7_eucl_centroid, k = 4)

plot(data7new , col = data7_eucl_ward_groups , pch=20 , main ="ward")
plot(data7new , col = data7_eucl_single_groups , pch=20 , main="single")
plot(data7new , col = data7_eucl_complete_groups , pch=20 , main = "complete")
plot(data7new , col = data7_eucl_average_groups , pch=20 , main ="average")
plot(data7new , col = data7_eucl_centroid_groups , pch=20 , main = "centroid")

##K-means
fit.km <- kmeans(data7new, 4, nstart=25)

plot(data7new , col = fit.km$cluster , pch=20 , main="K-mean")

##Model based clustering
fit <- Mclust(data7new)
#summary(fit)
plot(data7new , col=fit$classification , pch=20 , main="model based clustering")

## DBSCAN
kNNdistplot(data7new, k=10)
abline(h = 0.25, lty = 2)
resdb <- dbscan(data7new, 0.25, 10)
print(resdb)
plot(data7new , col = resdb$cluster , pch=20, main="DBSCAN")

## Pam
set.seed(1234)
fit.kmpam <- pam(data7new, k = 4)
plot (data7new , col = fit.kmpam$clustering , pch=20 , main="Pam")

## Hierarchical clustering algorithm (all the methods)
## as well as the k means and the pam algorithm performed a correct clustering
## DBSCAN algorithm with k=10 and h= 0.25 (allowing much noise) also managed to find the right clusters
## In the contrary model based clustering algorithms created more cluster (8) as it should have done


## I didn't have the time to try an algorithm on my own. Perhaps there will be some 
## during the spring vocations. If i end up with an idea would it be appropriate to send
## you the algorithms and receive some feedback. (obviously i am nor doing this for the bonus)


