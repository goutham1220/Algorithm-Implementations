library(ggplot2)

set.seed(2)

#Setting up the datasets
numPoints = 100
numClusters = 4
numIterations = 20

distance <- function(matrix, centroid){
  
  dist = vector()
  
   dist = apply(matrix, MARGIN = 1, FUN = function(x){
     centroid = centroid[1:2]
     sqrt(sum((centroid - x)^2))})
  
  return(dist)
}

kmeans_simulation = function(npoints, nclusters, niterations){
  
  dataset = matrix(data = NA, npoints, 2)
  dataset[,1] = rnorm(npoints, mean = 0, sd = 1)
  dataset[,2] = rnorm(npoints, mean = 0, sd = 1)
  dataset_init = dataset
  
  # initialize centroids to random values
  centroids = matrix(data = NA, nrow = nclusters, ncol = 3)
  centroids[,1:2] =  rnorm(nclusters * 2, mean = 0, sd = 1)
  centroids[,3] = 1:nrow(centroids)
  
  centroids_init = centroids
  
  ggplot(data = NULL, aes(x = dataset[,1], y = dataset[,2])) +
    geom_point(size=2, shape=23) + xlab("X") + ylab("Y") +
    geom_point(aes(x = data.frame(dataset[,1]), y = data.frame(dataset[,2])), color = "white") + 
    geom_point(aes(x=centroids[,1], y=centroids[,2]), colour=centroids[,3], size=3)
  
  alldistances = matrix(nrow = npoints, ncol = nclusters)
  clusterAssignment = vector()
  cost = vector()
  
  for(j in 1:niterations){
    
    alldistances = apply(centroids, MARGIN = 1, distance, matrix = dataset)
    clusterAssignment = apply(alldistances, MARGIN = 1, which.min)
    
    for(i in 1:nclusters){
      cost[j] = sum(distance(dataset[clusterAssignment == i,], centroids[i,]))
    }
    
    for(i in 1:nclusters){  
      centroids[i,1] = mean(dataset[clusterAssignment == i,1])
      centroids[i,2] = mean(dataset[clusterAssignment == i,2])
    }
    
  }
  
  return (list(cost, clusterAssignment, centroids, dataset, centroids_init, dataset_init))
}

list = kmeans_simulation(numPoints, numClusters, numIterations)

# ggplot(data = NULL) +
#   geom_point(size=2, shape="23") + xlab("X") + ylab("Y") +
#   geom_point(aes(x=list[[5]][,1], y=list[[5]][,2]), colour=list[[5]][,3], size=5) +
#   geom_point(aes(x=list[[6]][,1], y=list[[6]][,2]), colour="black",fill = "white", pch=21, size=2) 
# 
# ggplot(data = NULL) +
#   geom_point(aes(x=list[[3]][,1], y=list[[3]][,2]), colour=list[[3]][,3], size=5) +
#   geom_point(aes(x=list[[4]][,1], y=list[[4]][,2]), colour=list[[2]], size=2) +
#   xlab("X") + ylab("Y")
# 
# ggplot(data = NULL) +
#   geom_line(aes(x=1:numIterations, y=list[[1]])) +
#   xlab("# of Iterations") + ylab("Cost")

cost = vector()

for(i in 1:numClusters){
  numClusters = i
  costList = kmeans_simulation(numPoints, numClusters, numIterations)[[1]]
}

ggplot(data = NULL) +
  geom_line(aes(x = 1:length(costList), y = costList)) +
  xlab("# of Clusters") + ylab("Cost")
