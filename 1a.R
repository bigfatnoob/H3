library('e1071') # bayesian classifier
library('rpart') # decision trees
library('nnet') # Neural Networks

train = read.table(file = "ilk-tr-xy.txt", header = FALSE, sep = ",", col.names = c("id","x","y","label"))
train$id = as.factor(train$id)
train$label = as.factor(train$label)

test = read.table(file = "ilk-te-xy.txt", header = FALSE, sep = ",", col.names = c("id","x","y","label"))
test$id = as.factor(test$id)
test$label = as.factor(test$label)

#Naive Bayes
nbModel = naiveBayes(train[,2:3], train[,4])
trainTable = table(predict(nbModel, train[,2:3]), train[,4])
trainTable
testTable = table(predict(nbModel, test[,2:3]), test[,4])
testTable

#C5.0 based classification trees
tree=rpart(label ~ x + y, data=train, method="class")
trainTable = table(predict(tree, train, type="class"), train[,4])
trainTable
testTable = table(predict(tree, test, type="class"), test[,4])
testTable

# Neural Networks
nnmodel=nnet(label ~ x + y, data=train, size = 6)
trainTable = table(predict(nnmodel, train[,2:3], type = "class"),train[,4])
trainTable
testTable = table(predict(nnmodel, test[,2:3], type = "class"),test[,4])
testTable

#SVM Model
svmModel = svm(label ~ x + y, data=train)
trainTable = table(predict(svmModel, train[,2:3], type = "class"),train[,4])
trainTable
testTable = table(predict(svmModel, test[,2:3], type = "class"),test[,4])
testTable
