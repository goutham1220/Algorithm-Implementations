library(ggplot2)

npoints = 100
nclusters = 10

xvalues = vector()
yvalues = vector()

for(i in 1:nclusters){
  xvalues = c(xvalues, rnorm(npoints/nclusters, mean = (25*i), sd = 2))
  yvalues = c(yvalues, rnorm(npoints/nclusters, mean = (100*i)/nclusters, sd = 2))
}

dataset = cbind(xvalues, yvalues)

ggplot(data = NULL) +
  geom_point(size=2, shape=23) + xlab("X") + ylab("Y") +
  geom_point(aes(x = dataset[,1], y = dataset[,2]), color = "black") 