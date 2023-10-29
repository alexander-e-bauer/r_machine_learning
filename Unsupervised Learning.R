######################################################
# Principal Components Analysis
######################################################
data(USArrests)   # load dataset 
names(USArrests)

apply(USArrests,2,mean)   # (column means) mean of variables
apply(USArrests,2,var)   # (column variance) variance of variables

?prcomp
pca=prcomp(USArrests,scale=TRUE)   # perform PCA
pca
summary(pca)

pca$rotation   # principal component loading vectors
dim(pca$x)   # matrix of principal component scores
head(pca$x)

# biplot (Figure 10.1, slide 363)
pca$rotation=-pca$rotation
pca$x=-pca$x
biplot(pca,scale=0,cex=0.6)

pca$sdev   # standard deviation explained by each PC
pca_var=pca$sdev^2   # variance explained by each PC
pca_var

pve=pca_var/sum(pca_var)   # proportion of variance explained
pve

# scree plot (Figure 10.4, slide 367)
par(mfrow=c(1,2))
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",
     ylim=c(0,1),type="b")
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),type="b")

######################################################
# K-Means Clustering
######################################################

set.seed(2)

# generate the dataset
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
cluster=c(rep(1,25),rep(2,25))   # true clusters
plot(x,col=cluster,pch=20)   # plot data with true clusters

?kmeans
km1=kmeans(x,centers=2,nstart=20)   # perform k-means clustering with k=2
km1   # summary

km1$cluster   # cluster assignments for each observation
km1$withinss   # within-cluster sums of squares
km1$tot.withinss   # total within-cluster sum of squares
km1$betweenss   # between cluster sum of squares
km1$totss   # total sum of squares

# plot clusters chosen by k-means with true data
plot(x,col=km1$cluster,main="K-Means Clustering with K=2",pch=1,cex=1.5)
points(x,col=cluster,pch=20)
points(x,col=c(2,1)[cluster],pch=20)
table(km1$cluster,cluster)   # compare with truth

set.seed(4)
# perform k-means clustering with k=3
km2=kmeans(x,centers=3,nstart=20)
km2
plot(x,col=km2$cluster,main="K-Means Clustering with K=3",pch=1,cex=1.5)
points(x,col=cluster,pch=20)
table(km2$cluster,cluster)   # compare with truth

set.seed(3)
# effect of number of initial cluster assignments (nstart)
km3=kmeans(x,3,nstart=1)
km3$tot.withinss
km4=kmeans(x,3,nstart=20)
km4$tot.withinss


######################################################
# Hierarchical Clustering
######################################################

?hclust
?dist
d=dist(x,method="euclidean")   # computes distance matrix

hc_complete=hclust(d,method="complete")   # hierarchical clustering with complete linkage
hc_complete   # summary
hc_average=hclust(d,method="average")   # hierarchical clustering with average linkage
hc_single=hclust(d,method="single")   # hierarchical clustering with single linkage

# plot dendrograms
plot(hc_complete,main="Complete Linkage",xlab="",sub="",cex=.9)
plot(hc_complete,main="Complete Linkage",xlab="",sub="",cex=.9,labels=cluster)

par(mfrow=c(1,3))
plot(hc_complete,main="Complete Linkage",xlab="",sub="",cex=.9,labels=cluster)
plot(hc_average,main="Average Linkage",xlab="",sub="",cex=.9,labels=cluster)
plot(hc_single,main="Single Linkage",xlab="",sub="",cex=.9,labels=cluster)

# cut a dendrogram at a chosen level
?cutree
cutree(hc_complete,k=2)
table(cluster,cutree(hc_complete,k=2))
cutree(hc_complete,h=6)
table(cluster,cutree(hc_complete,h=6))

cutree(hc_average,k=2)
table(cluster,cutree(hc_average,k=2))
cutree(hc_single,k=2)
table(cluster,cutree(hc_single,k=2))

?scale
xsc=scale(x)   # scale the variables
par(mfrow=c(1,1))
plot(hclust(dist(xsc),method="complete"),
     main="Hierarchical Clustering with Scaled Features",labels=cluster)
