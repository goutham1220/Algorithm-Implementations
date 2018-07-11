
#Setting up the datasets
npoints = 100
dataset = matrix(data = NA, npoints, 2)
dataset[,1] = sample(1:100, npoints)
dataset[,2] = sample(1:100, npoints)
dataset

#initialize centroids to random values
centroid1 = dataset[sample(1:100, 1),]
centroid2 = dataset[sample(1:100, 1),]
centroid3 = dataset[sample(1:100, 1),]
centroid4 = dataset[sample(1:100, 1),]

distance <- function(matrix, centroid){
  length = sqrt((centroid[1] - matrix[1])^2 + (centroid[2] - matrix[2])^2)
  return(length)
}

clusterAssignment = matrix(data = NA, npoints, 1)

clusterAssignment[,1] = which.min(c(distance(dataset[1,], centroid1), 
                                    distance(dataset[,], centroid2), 
                                    distance(dataset[,], centroid3), 
                                    distance(dataset[,], centroid4)))


clusterAssignment
