library(rpart)

str(kyphosis)
head(kyphosis)

tree <- rpart(Kyphosis~.,method ='class',data = kyphosis)
printcp(tree)

plot(tree,uniform = T,main = 'Kyphosis Tree')
text(tree)

library(rpart.plot)

prp(tree)

library(randomForest)

rf.model <- randomForest(Kyphosis~.,data = kyphosis)
print(rf.model)

rf.model$confusion

