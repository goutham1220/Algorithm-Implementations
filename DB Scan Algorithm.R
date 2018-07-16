library(ggplot2)

set.seed(2)

radius = 35/3
minPoints = 4
dataset = matrix(data = NA, npoints, 2)
dataset[,1] = sample(1:100, npoints)
dataset[,2] = sample(1:100, npoints)
randValueCount = 1
cluster = matrix(data = NA, nrow = 100, ncol = 2)
isCluster = matrix(data = NA, nrow = 100, ncol = 2)

randValue = vector()
randValue[1:2] = sample(1:100, 2)

randValue[3] = 1

ggplot(data = NULL, aes(x = dataset[,1], y = dataset[,2])) +
  geom_point(size=2, shape=23) + xlab("X") + ylab("Y") +
  geom_point(aes(x = data.frame(dataset[,1]), y = data.frame(dataset[,2])), color = "white") + 
  geom_point(aes(x=randValue[1], y=randValue[2]), colour=randValue[3], size=3)

distance <- function(matrix, randomValue){
  
  dist = vector()
  
  for(i in 1:nrow(matrix)){
    dist[i] = sqrt((randomValue[1] - matrix[i,1])^2 + (randomValue[2] - matrix[i,2])^2)
  }
  
  return(dist)
}

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

alldistances = vector()

alldistances = distance(dataset, randValue[1:2])

for(i in 1:nrow(dataset)){
  if(alldistances[i] <= radius){
    isCluster[i,1] = 1
  } else {
    isCluster[i,1] = 0
  }
}

for(i in 1:nrow(dataset)){
  if(isCluster[i,1] == 1){
      cluster[i,] = dataset[i,]
     # cluster = rbind(cluster, dataset[i,])
  }
}

ggplot(data = NULL, aes(x = dataset[,1], y = dataset[,2])) +
  geom_point(size=2, shape=23) + xlab("X") + ylab("Y") +
  geom_point(aes(x = data.frame(dataset[,1]), y = data.frame(dataset[,2])), color = "white") + 
  geom_point(aes(x=randValue[1], y=randValue[2]), colour=randValue[3], size=3) +
  geom_point(aes(x=cluster[, 1], y=cluster[, 2], colour=randValue[3], size=3))

clusterResults = matrix(data = NA, nrow = 100, ncol = 2)

for(i in 1:nrow(dataset)){
  clusterResults[i,] = dataset[i,] %in% cluster[,]
}

isCluster = isCluster[rowSums(is.na(isCluster)) != ncol(isCluster), ]
isCluster = clusterResults

cluster = cluster[rowSums(is.na(cluster)) != ncol(cluster), ]
notCluster = matrix(data = NA, nrow = 100, ncol = 2)
notCluster[(clusterResults == FALSE)] = dataset[(clusterResults == FALSE)]
notCluster = notCluster[rowSums(is.na(notCluster)) != ncol(notCluster), ]
dataset = notCluster

randValue = notCluster[sample(1:nrow(notCluster), 1),]

