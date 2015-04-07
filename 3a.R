library('rgdal')
library('e1071') # Naive Bayes
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


inRange <- function(a,b, threshold) {
  if ((a$band1 - b$band1)>threshold) {
    return(FALSE)
  }
  if ((a$band2 - b$band2)>threshold) {
    return(FALSE)
  }
  if ((a$band3 - b$band3)>threshold) {
    return(FALSE)
  }
  return(TRUE)
}

getNeighbors<-function(imgdf, index, prox=3, threshold=5) {
  prev=floor(prox/2)
  start = index-prev*1024-prev
  retDf= data.frame()
  this=imgdf[index,1:3]
  for(j in 0:(prox-1)) {
    for(i in 0:(prox-1)) {
      ind = start + 1024*j + i
      if (ind > 0 && ind<=(dim(imgdf)[1])) {
        pt = imgdf[ind,1:3]
        if (inRange(this, pt, threshold)) {
          retDf = rbind(retDf, pt)   
        }
      }
    }
  }
  return(retDf)
}

segment <- function(imgdf, prox=3) {
  len=c()
  tempDf=imgdf
  
  intDf = data.frame()
  x=sapply(1:nrow(imgdf),function(i){
    print(i)
    round(colMeans(getNeighbors(imgdf,i,3)))
  })
  tempDf=as.data.frame(t(x))
  return(tempDf)
} 

segmentMany <- function(imgdf, prox=3, iterations=1) {
  tempDf = segment(imgdf, prox)
  if (iterations > 1) {
    for (i in 2:iterations){
      tempDf = segment(tempDf, prox)
    }
  }
  return(tempDf)
}

tempDf=segmentMany(imgdf)

#naive Bayes
trainSet =  makeSpatialSet(train, tempDf)
testSet =  makeSpatialSet(train, tempDf, prox = 1)
nbModel = naiveBayes(label ~ band1 + band2 + band3, trainSet)
trainTable = table(predict(nbModel, trainSet, type = "class"),trainSet$label)
trainTable
testTable = table(predict(nbModel, testSet, type = "class"),testSet$label)
testTable
#reconstruct image
trainSet=makeSpatialSet(train, tempDf)
nbModel=naiveBayes(trainSet[,1:3], trainSet$label)
classified = matrix(as.integer(predict(nbModel, tempDf)), nrow = 1024, ncol = 1024)
image(rotate(t(classified)), main="Spatial Bayes", col=rainbow(12))