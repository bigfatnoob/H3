library("caret")

# Get Training rows for
# a certain class
getTrainForClass <- function(inp, cl) {
  mat = as.matrix(subset(inp, CL==cl)[,-4])
  return(mat)
}

# Compute columnwise mean
# for all the rows
meanMLE <- function(inp) {
  return(colMeans(inp))
}

# Compute columnwise variance
# for all the rows
varMLE <- function(inp, mu) {
  dummy <- matrix(0, 3, 3)
  for(i in 1:(dim(inp)[1])) {
    diff = inp[i,] - mu
    dummy = dummy + diff %*% t(diff)
  }
  return(dummy/dim(inp)[1])
}

# Create a model for a particular
# class
uniModel <- function(inp, cl) {
  trainRows = getTrainForClass(inp, cl)
  mu = meanMLE(trainRows)
  sigma =  varMLE(trainRows, mu)
  N = dim(trainRows)[1]
  prior = N/(dim(inp)[1])
  return(list(class=cl, N=N, mu = mu, sigma = sigma, prior = prior))
}

# For all the available classes
# make UniModel
makeModel <- function(inp) {
  classes = unique(inp$CL)
  models = list()
  for (i in 1:length(classes)) {
    models[[i]] = uniModel(inp, classes[i])
  }
  return(models)
} 

# Compute Likelihood score for a test input
# wrt to a certain model
estimateMLE <- function(testInp, model) {
  x = as.matrix(testInp[1:3])
  dif = as.matrix(x - model$mu)
  sig = model$sigma
  N = model$N
  det = determinant(sig)$modulus[1]
  MLE = ((2*pi)**(-N/2))*(det**(-0.5))*exp(-0.5 * dif %*% (sig**-1) %*% t(dif))
  prior = model$prior
  return(MLE)
}

# Based on the MLE score for each model
# we select the class with the maximum
# estimate of MLE score
predictClass <- function(testInp, models) {
  highestMLE = -Inf
  class = 0
  for (i in 1:length(models)) {
    thisMLE = estimateMLE(testInp,models[[i]]) 
    if (thisMLE > highestMLE) {
      highestMLE = thisMLE
      class = models[[i]]$class
    }
  }
  return(class)
}

# Predict class for each row of
# the test dataset
predictTest <- function(test, models) {
  predicts = c()
  for (i in 1:(dim(test)[1])) {
    predicts = c(predicts, predictClass(test[i,], models))
  }
  test$predicts = predicts
  return(test)
}

train = read.csv("ilk-tr-d.csv")
test = read.csv("ilk-te-d.csv")
models = makeModel(train)
test=predictTest(test, models)
print(confusionMatrix(test$predicts, test$CL))
