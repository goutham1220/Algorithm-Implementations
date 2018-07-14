library(ggplot2)

set.seed(2)

#Setting up the datasets
npoints = 100
nclusters = 4
dataset = matrix(data = NA, npoints, 2)
dataset[,1:2] = sample(1:100, npoints * 2, replace = TRUE)

#initialize centroids to random values
centroids = matrix(data = NA, nrow = nclusters, ncol = 3)
centroids[,1:2] =  sample(1:100, nclusters * 2)

  centroids[,3] = 1:nrow(centroids)


# centroid1 = sample(1:100, 2)
# centroid2 = sample(1:100, 2)
# centroid3 = sample(1:100, 2)
# centroid4 = sample(1:100, 2)

ggplot(data = NULL, aes(x = dataset[,1], y = dataset[,2])) +
  geom_point(size=2, shape=23) + xlab("X") + ylab("Y") +
  geom_point(aes(x = data.frame(dataset[,1]), y = data.frame(dataset[,2])), color = "white") + 
  geom_point(aes(x=centroids[,1], y=centroids[,2]), colour=centroids[,3], size=3) 

distance <- function(matrix, centroid){
  
  dist = vector()
  
  for(i in 1:nrow(matrix)){
    dist[i] = sqrt((centroid[1] - matrix[i,1])^2 + (centroid[2] - matrix[i,2])^2)
  }
  
  return(dist)
}

alldistances = matrix(nrow = npoints, ncol = nclusters)

# for(i in 1:nclusters){
# alldistances[i,] = distance(dataset, centroids[i,1:2])
# }

alldistances = apply(centroids[,1:2], MARGIN = 1, distance, matrix = dataset)

clusterAssignment = vector()

for(i in 1:npoints){
  clusterAssignment[i] = which.min(all4distances[,i])
}

cluster1 = matrix(data = NA, npoints, ncol = 2)
cluster2 = matrix(data = NA, npoints, ncol = 2)
cluster3 = matrix(data = NA, npoints, ncol = 2)
cluster4 = matrix(data = NA, npoints, ncol = 2)

for(i in 1:npoints){
    if(clusterAssignment[i] == 1){
      cluster1[i,] = dataset[i,]
    } else if(clusterAssignment[i] == 2){
      cluster2[i,] = dataset[i,]
    } else if(clusterAssignment[i] == 3){
      cluster3[i,] = dataset[i,]
    } else if(clusterAssignment[i] == 4){
      cluster4[i,] = dataset[i,]
    }
  }

cluster1 = cluster1[rowSums(is.na(cluster1)) != ncol(cluster1), ]
cluster2 = cluster2[rowSums(is.na(cluster2)) != ncol(cluster2), ]
cluster3 = cluster3[rowSums(is.na(cluster3)) != ncol(cluster3), ]
cluster4 = cluster4[rowSums(is.na(cluster4)) != ncol(cluster4), ]

ggplot(data = NULL) +
  geom_point(aes(x=centroids[1,1], y=centroids[1,2]), colour="blue", size=5) +
  geom_point(aes(x=cluster1[,1], y=cluster1[,2]), colour="blue", size=2) +
  geom_point(aes(x=centroids[2,1], y=centroids[2,2]), colour="red", size=5) +
  geom_point(aes(x=cluster2[,1], y=cluster2[,2]), colour="red", size=2) +
  geom_point(aes(x=centroids[3,1], y=centroids[3,2]), colour="green", size=5) +
  geom_point(aes(x=cluster3[,1], y=cluster3[,2]), colour="green", size=2) +
  geom_point(aes(x=centroids[4,1], y=centroids[4,2]), colour="black", size=5) +
  geom_point(aes(x=cluster4[,1], y=cluster4[,2]), colour="black", size=2) +
  xlab("X") + ylab("Y")

for(i in 1:nclusters){  
  centroids[1,i] = mean(cluster1[,i])
  centroids[2,i] = mean(cluster2[,i])
  centroids[3,i] = mean(cluster3[,i])
  centroids[4,i] = mean(cluster4[,i])
}
