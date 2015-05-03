library('e1071') # Naive Bayes
library("caret")

train = read.csv("ilk-tr-d.csv")
test = read.csv("ilk-te-d.csv")
train$CL = as.factor(train$CL)
test$CL = as.factor(test$CL)
# Creating the naive Bayes Model
nbModel = naiveBayes(CL ~ R + G + B, train)
test$predicts=predict(nbModel, test, type = "class")
print(confusionMatrix(test$predicts, test$CL))