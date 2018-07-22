library(ggplot2)

set.seed(6)

#Setting up the datasets
numPoints = 200
numClusters.data = 5
numClusters = 5
numIterations = 10
spread = 4

distance <- function(matrix, centroid){
  
  dist = apply(matrix, MARGIN = 1, FUN = function(x){
     centroid = centroid[1:2]
     sqrt(sum((centroid - x)^2))})
  
  return(dist)
}

kmeans_simulation = function(npoints, nclusters, niterations, nclusters.data = 4, spread = 5){
  
  xvalues = vector()
  yvalues = vector()
  
  for(i in 1:nclusters.data){
    xvalues = c(xvalues, rnorm(npoints/nclusters.data, mean = (25*i), sd = spread))
    yvalues = c(yvalues, rnorm(npoints/nclusters.data, mean = (100*i)/nclusters.data, sd = spread))
  }
  
  dataset = cbind(xvalues, yvalues)

  # initialize centroids to random values
  # xy =  rnorm(nclusters * 2, mean = meanValue, sd = spread)
  xy =  dataset[sample(1:npoints, nclusters),]
  color = 1:nrow(xy)
  centroids = cbind(xy, color)
  
  centroids_init = centroids
  cost = vector()
  
  for(j in 1:niterations){
    
    alldistances = apply(centroids, MARGIN = 1, distance, matrix = dataset)
    clusterAssignment = apply(alldistances, MARGIN = 1, which.min)
    cost[j] = sum(sapply(1:nclusters, 
                         function(x) sum(distance(dataset[clusterAssignment == x,], centroids[x,]))))
    centroids[,1:2] = sapply(1:nclusters, 
                             function(x) mean(dataset[clusterAssignment == x,1:2]))

  }
  
  return (list(cost, clusterAssignment, centroids, dataset, centroids_init))
}

list = kmeans_simulation(numPoints, numClusters, numIterations, numClusters.data, spread)

ggplot(data = NULL) +
  geom_point(size=2, shape="23") + xlab("X") + ylab("Y") +
  geom_point(aes(x=list[[5]][,1], y=list[[5]][,2]), colour=list[[5]][,3], size=5) +
  geom_point(aes(x=list[[4]][,1], y=list[[4]][,2]), colour="black",fill = "white", pch=21, size=2)

Sys.sleep(2)

ggplot(data = NULL) +
  geom_point(aes(x=list[[3]][,1], y=list[[3]][,2]), colour=list[[3]][,3], size=5) +
  geom_point(aes(x=list[[4]][,1], y=list[[4]][,2]), colour=list[[2]], size=2) +
  xlab("X") + ylab("Y")

Sys.sleep(2)

ggplot(data = NULL) +
  geom_line(aes(x=1:numIterations, y=list[[1]])) +
  xlab("# of Iterations") + ylab("Cost")

costList = vector()

for(i in 2:10){
  costList[i] = kmeans_simulation(numPoints, i, numIterations, numClusters.data, spread)[[1]][numIterations]
}

ggplot(data = NULL) +
  geom_line(aes(x = 2:length(costList), y = costList[2:length(costList)])) +
  xlab("# of Clusters") + ylab("Cost")
