set.seed(2)

#Setting up the datasets
npoints = 100
dataset = matrix(data = NA, npoints, 2)
dataset[,1] = sample(1:100, npoints)
dataset[,2] = sample(1:100, npoints)
dataset

#initialize centroids to random values
centroid1 = sample(1:100, 2)
centroid2 = sample(1:100, 2)
centroid3 = sample(1:100, 2)
centroid4 = sample(1:100, 2)

distance <- function(matrix, centroid){
  
  dist = vector()
  
  for(i in 1:nrow(matrix)){
    dist[i] = sqrt((centroid[1] - matrix[i,1])^2 + (centroid[2] - matrix[i,2])^2)
  }
  
  return(dist)
}

distances1 = distance(dataset, centroid1)
distances2 = distance(dataset, centroid2)
distances3 = distance(dataset, centroid3)
distances4 = distance(dataset, centroid4)

all4distances = matrix(ncol = npoints, nrow = 4)

all4distances[1,] = distances1
all4distances[2,] = distances2
all4distances[3,] = distances3
all4distances[4,] = distances4

clusterAssignment = vector()

for(i in 1:npoints){
  clusterAssignment[i] = which.min(all4distances[,i])
}

cluster1 = matrix(data = NA, npoints, 2)
cluster2 = matrix(data = NA, npoints, 2)
cluster3 = matrix(data = NA, npoints, 2)
cluster4 = matrix(data = NA, npoints, 2)

for(i in 1:npoints){
  switch(clusterAssignment[i], clusterAssignment = 1, clusterAssignment = 2)
}

