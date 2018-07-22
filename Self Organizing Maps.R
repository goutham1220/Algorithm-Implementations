#Self Organizing Maps
library(ggplot2)
set.seed(6)

#Setting up the datasets
numPoints = 100
numClusters = 3
numIterations = 5
spread = 4
meanValue = 75

distance <- function(point, centroid){
  
  dist = sqrt((centroid - point)^2)
  
  return(dist)
}

unit_vec <- function(vec){
  return(vec/(sqrt(sum(vec**2,na.rm=TRUE))))
}

xvalues = vector()
yvalues = vector()

for(i in 1:numClusters){
  xvalues = c(xvalues, rnorm(numPoints/numClusters, mean = (25*i), sd = spread))
  yvalues = c(yvalues, rnorm(numPoints/numClusters, mean = (100*i)/numClusters, sd = spread))
}

dataset = cbind(xvalues, yvalues)

xy =  matrix(data = rnorm(nclusters * 2, mean = meanValue, sd = spread), nrow = numClusters, ncol = 2)

color = 1:nrow(xy)
centroids = cbind(xy, color)

xdistances = matrix(data = NA, nrow = nrow(dataset), ncol = 3)
distChange = matrix(data = NA, nrow = nrow(dataset), ncol = 3)
ydistances = matrix(data = NA, nrow = nrow(dataset), ncol = 3)
weight = 0.5

      # distChange[i,] = (centroids[,1:2] - dataset[i,1:2])
   # centroids[,1] = centroids[,1] + xdistChange[i,]
   # centroids[,2] = centroids[,2] + ydistChange[i,]
   # }

ggplot(data = NULL) +
  geom_point(size=2, shape="23") + xlab("X") + ylab("Y") +
  geom_point(aes(x=centroids[,1], y=centroids[,2]), colour=centroids[,3], size=5) +
  geom_point(aes(x=dataset[,1], y=dataset[,2]), colour="black",fill = "white", pch=21, size=2) +
  # geom_point(aes(x=dataset[99,1], y = dataset[99,2]), size = 5)
  xlim(c(0,100))  + ylim(c(0,200))
