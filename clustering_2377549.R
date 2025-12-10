#Reading the files

seeds_real = read.csv('C://Users//User//Downloads//seeds_real - Copy.csv', sep=",")
seeds_dataset = read.csv('C://Users//User//Downloads//seeds_dataset(1).csv', sep=",")
source("C://Users//User//Downloads//WK_R.r")

#preparing the data 

seeds_dataset= na.omit(seeds_dataset)
seeds_real= na.omit(seeds_real)


#standardizing variables


seeds_dataset= scale(seeds_dataset)

#Hierarchical Clustering


d <- dist(seeds_dataset, method = "euclidean")

#method = average
fit_avg <- hclust(d, method="average")
plot(fit_avg, main= "Average linkage")
Hgroups_avg <- cutree(fit_avg, k=3)
rect.hclust(fit_avg, k=3, border="blue")
plot(seeds_dataset, col=Hgroups_avg, main= "Average Linkage Clusters")

#method = single
fit_sin <- hclust(d, method="single")
plot(fit_sin, main= "Single Linkage")
Hgroups_sin <- cutree(fit_sin, k=3)
rect.hclust(fit_sin, k=3, border="green")
plot(seeds_dataset, col=Hgroups_sin, main= "Single Linkage Clusters")

#method = complete
fit_com <- hclust(d, method="complete")
plot(fit_com, main="Complete Linkage")
Hgroups_com <- cutree(fit_com, k=3)
rect.hclust(fit_com, k=3, border="red")
plot(seeds_dataset, col=Hgroups_com, main= "Complete Linkage Clusters")

#weighted kappa for average linkage = 0.6700192

wk_avg <- WK_R(Hgroups_avg, seeds_real$Real)
print(wk_avg)

#weighted kappa for single linkage = 0.0004684259

wk_sin <- WK_R(Hgroups_sin, seeds_real$Real)
print(wk_sin)

#weighted kappa for complete linkage =  0.7444477
wk_com <- WK_R(Hgroups_com, seeds_real$Real)
print(wk_com)

#Creating a vector to store WK values

wk_hier <- c(average = wk_avg, single = wk_sin, complete = wk_com)
print(wk_hier)


#K-means clustering

set.seed(123)

# K = 2 , wk = 0.490661
set.seed(123)
km2 <- kmeans(seeds_dataset, centers = 2, nstart = 25)
groups_k2 <- km2$cluster
plot(seeds_dataset, col = groups_k2, main = "K-means Clustering (K = 2)")
wk_k2 <- WK_R(groups_k2, seeds_real$Real)
print(wk_k2)

#k = 3 ,wk= 0.7990207

set.seed(123)
km3 <- kmeans(seeds_dataset, centers = 3, nstart = 25)
groups_k3 <- km3$cluster
plot(seeds_dataset, col = groups_k3, main = "K-means Clustering (K = 3)")
wk_k3 <- WK_R(groups_k3, seeds_real$Real)
print(wk_k3)

# K = 4, wk= 0.6681031
set.seed(123)
km4 <- kmeans(seeds_dataset, centers = 4, nstart = 25)
groups_k4 <- km4$cluster
plot(seeds_dataset, col = groups_k4, main = "K-means Clustering (K = 4)")
wk_k4 <- WK_R(groups_k4, seeds_real$Real)
print(wk_k4)

# K = 5, wk=0.5572511
set.seed(123)
km5 <- kmeans(seeds_dataset, centers = 5, nstart = 25)
groups_k5 <- km5$cluster
plot(seeds_dataset, col = groups_k5, main = "K-means Clustering (K = 5)")
wk_k5 <- WK_R(groups_k5, seeds_real$Real)
print(wk_k5)


# K = 6, wk= 0.474697
set.seed(123)
km6 <- kmeans(seeds_dataset, centers = 6, nstart = 25)
groups_k6 <- km6$cluster
plot(seeds_dataset, col = groups_k6, main = "K-means Clustering (K = 6)")
wk_k6 <- WK_R(groups_k6, seeds_real$Real)
print(wk_k6) 



#Storing the wk for k means in a vector 
wk_kmeans <- c(K2 = wk_k2, K3 = wk_k3, K4 = wk_k4, K5 = wk_k5, k6 = wk_k6)
print(wk_kmeans)


#Plotting the weighted kappa values for Hierarchical clustering

barplot(wk_hier,
        main = "Weighted Kappa - Hierarchical Clustering",
        ylab = "Weighted Kappa",
        xlab = "Linkage method")



#Plotting the weighted kappa values for K-means clustering
plot(2:6,wk_kmeans,
     type = "b",
     main = "Weighted Kappa - K-means Clustering",
     xlab = "Number of clusters (K)",
     ylab = "Weighted Kappa")










