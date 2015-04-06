library('rgdal')
library('e1071') # Svm classifier
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

img = readGDAL('ilk-3b-1024.tif')
imgdf = data.frame(img)
train = read.table(file = "ilk-tr-xy.txt", header = FALSE, sep = ",", col.names = c("id","x","y","label"))
test = read.table(file = "ilk-te-xy.txt", header = FALSE, sep = ",", col.names = c("id","x","y","label"))


#svmModel
trainSet =  makeSpatialSet(train, imgdf)
testSet =  makeSpatialSet(train, imgdf, prox = 1)
svmModel = svm(label ~ band1 + band2 + band3, data=trainSet)
trainTable = table(predict(svmModel, trainSet, type = "class"),trainSet$label)
trainTable
testTable = table(predict(svmModel, testSet, type = "class"),testSet$label)
testTable
#reconstruct image
svmModel = svm(label ~ band1 + band2 + band3, data=makeSpatialSet(train, imgdf, factorize = FALSE))
classified = matrix(predict(svmModel, imgdf), nrow = 1024, ncol = 1024)
image(rotate(t(classified)), main="Spatial SVM", col=rainbow(12))

#naive Bayes
trainSet =  makeSpatialSet(train, imgdf)
testSet =  makeSpatialSet(test, imgdf, prox = 1)
nbModel = naiveBayes(label ~ band1 + band2 + band3, trainSet)
trainTable = table(predict(nbModel, trainSet, type = "class"),trainSet$label)
trainTable
testTable = table(predict(nbModel, testSet, type = "class"),testSet$label)
testTable
#reconstruct image
trainSet=makeSpatialSet(train, imgdf)
nbModel=naiveBayes(trainSet[,1:3], trainSet$label)
classified = matrix(as.integer(predict(nbModel, imgdf)), nrow = 1024, ncol = 1024)
image(rotate(t(classified)), main="Spatial Bayes", col=rainbow(12))

#decision Tree
trainSet =  makeSpatialSet(train, imgdf)
testSet =  makeSpatialSet(test, imgdf, prox = 1)
nbModel = naiveBayes(label ~ band1 + band2 + band3, trainSet)
trainTable = table(predict(nbModel, trainSet, type = "class"),trainSet$label)
trainTable
testTable = table(predict(nbModel, testSet, type = "class"),testSet$label)
testTable
#reconstruct image
trainSet=makeSpatialSet(train, imgdf)
nbModel=naiveBayes(trainSet[,1:3], trainSet$label)
classified = matrix(as.integer(predict(nbModel, imgdf)), nrow = 1024, ncol = 1024)
image(rotate(t(classified)), main="Spatial Bayes", col=rainbow(12))