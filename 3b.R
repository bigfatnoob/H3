library('rgdal')
library('e1071') # Naive Bayes
library('arules')
library('rpart')
rotate <- function(x) t(apply(x, 2, rev))

getProximity <- function(img, x, y, label, prox=3) {
  prev=floor(prox/2)
  start = (1024)*(y-prev) + (x-prev)
  retDf= data.frame()
  for(j in 0:(prox-1)) {
    for(i in 0:(prox-1)) {
      index = start + 1024*j + i
      pt = img[index,]
      pt$label = label
      retDf = rbind(retDf, pt)
    }
  }
  return(retDf)
}

makeSpatialSet <- function(train, imgdf,prox=3,factorize=TRUE) {
  trainset=data.frame()
  for (i in 1:(dim(train)[1])) {
    t=train[i,]
    trainset =  rbind(trainset, getProximity(imgdf,t$x, t$y, t$label, prox))
  }
  if (factorize==TRUE) {
    trainset$label = as.factor(trainset$label)  
  }
  return(trainset)
}

makeDiscrete <- function(imgdf) {
  imgdf$discBand1=discretize(imgdf$band1, method="frequency",categories = 10, labels=c(1:10))
  imgdf$discBand2=discretize(imgdf$band2, method="frequency",categories = 10, labels=c(1:10))
  imgdf$discBand3=discretize(imgdf$band3, method="frequency",categories = 10, labels=c(1:10))
  return(imgdf)
}

img = readGDAL('ilk-3b-1024.tif')
imgdf = data.frame(img)
train = read.table(file = "ilk-tr-xy.txt", header = FALSE, sep = ",", col.names = c("id","x","y","label"))
test = read.table(file = "ilk-te-xy.txt", header = FALSE, sep = ",", col.names = c("id","x","y","label"))
imgdf = makeDiscrete(imgdf)

trainSet =  makeSpatialSet(train, imgdf)
testSet =  makeSpatialSet(test, imgdf, prox=1)

#NaiveBayes(Discrete)
tree=rpart(label ~ discBand1 + discBand2 + discBand3, data=trainSet, method="class")
trainTable = table(predict(tree, trainSet, type="class"), trainSet$label)
trainTable
testTable = table(predict(tree, testSet, type="class"), testSet$label)
testTable
classified = matrix(predict(decisionTree, imgdf), nrow = 1024, ncol = 1024)
image(rotate(t(classified)), main="Spatial C5.0 Discrete", col=heat.colors(12))

#NaiveBayes(Continuous)
tree=rpart(label ~ band1 + band2 + band3, data=trainSet, method="class")
trainTable = table(predict(tree, trainSet, type="class"), trainSet$label)
trainTable
testTable = table(predict(tree, testSet, type="class"), testSet$label)
testTable
classified = matrix(predict(decisionTree, imgdf), nrow = 1024, ncol = 1024)
image(rotate(t(classified)), main="Spatial C5.0 Continuous", col=terrain.colors(12))