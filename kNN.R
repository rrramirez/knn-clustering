Complex9_RN32 <- read.csv("Complex9_RN32.txt", header=FALSE)

#Task 0
d<-data.frame(x=Complex9_RN32[,1],y=Complex9_RN32[,2],class=factor(Complex9_RN32[,3]))
plot(d$x,d$y)
require("lattice")
require("ggplot2")
ggplot(d, aes(x=x, y=y, colour=class))+ geom_point()


#Task 2 K nearest neighbor outlier detection algorithm
#function to get distance matrix using manhattan distance
mdist<-function(a)
{
  scaled<-scale(a)
  
  A<-dist(scaled, method = "manhattan")
  
  return (A)
}

#calling distance function and convertion dist obj to a matrix
dMatrix = mdist(data.frame(Complex9_RN32[,1:2]))
dMatrix = as.matrix(dMatrix)

#creating copy of Complex9 dataset and initializing the 'ols' column
C9_32 = Complex9_RN32
C9_32$ols = NA
colnames(C9_32)[3] = "class"

#for loop that gives each data point an outlier score
for(i in 1:nrow(dMatrix)){
  #sorts distance of nearest neighbors to get the closests K neighbors
  tempRow = sort(dMatrix[i,])
  #grabs K neighbors and averages their distances up; K is 20 in this case
  C9_32[i,4] = sum(tempRow[2:20])/20
}

#normalizes outlier scores so that they are between [0,1]
maxs = max(C9_32$ols)
mins = min(C9_32$ols)
C9_32$ols = scale(C9_32$ols, center = mins, scale = maxs - mins)

#sorts dataset based in decreasing order based on a point's outlier score
C9_32 = C9_32[order(C9_32$ols, decreasing = TRUE),]

#setting the first 9% of datapoints to be outliers
outliers = C9_32[1:360,]
remaining = C9_32[361:3999,]
#visualizing outliers
ggplot(outliers, aes(x=V1, y=V2, colour=class))+ geom_point()
ggplot(remaining, aes(x=V1, y=V2, colour=class))+ geom_point()

#setting the first 18% of datapoints to be outliers
outliers = C9_32[1:720,]
remaining = C9_32[721:3999,]
#visualizing outliers
ggplot(outliers, aes(x=V1, y=V2, colour=class))+ geom_point()
ggplot(remaining, aes(x=V1, y=V2, colour=class))+ geom_point()

#setting the first 36% of datapoints to be outliers
outliers = C9_32[1:1440,]
remaining = C9_32[1441:3999,]
#visualizing outliers
ggplot(outliers, aes(x=V1, y=V2, colour=class))+ geom_point()
ggplot(remaining, aes(x=V1, y=V2, colour=class))+ geom_point()

#histogram of the ols column of the first 36%
hist(outliers$ols, main = "", xlab = "OLS")
