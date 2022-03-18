library(ISLR)

head(College)

library(ggplot2)

pl <- ggplot(College,aes(x=Room.Board,y=Grad.Rate)) + geom_point(aes(color=Private))
print(pl)

pl2 <-ggplot(College,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),position = "dodge",color='black',bins=50)
print(pl2)

subset(College,Grad.Rate>100)
df <- College 

df['Cazenovia','Grad.Rate'] <- 100

library(caTools)

set.seed(101)

sample <- sample.split(df$Private,SplitRatio = .7)
train.data <- subset(df,sample==TRUE)
test.data <- subset(df,sample==FALSE)

library(rpart)
tree <- rpart(Private~.,method = "class",data = df)
printcp(tree)

tree.predicts <- predict(tree,test.data)
head(tree.predicts)

tree.predicts <- as.data.frame(tree.predicts)
joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}

tree.predicts$Private <- sapply(tree.predicts$Yes,joiner)
head(tree.predicts)

tree.table <- table(tree.predicts$Private,test.data$Private)

library(rpart.plot)
prp(tree)

library(randomForest)
rf.model <- randomForest(Private~.,data = train.data,importance=TRUE)
rf.model$confusion
rf.model$importance

rf.predicts <- predict(rf.model,test.data)
rf.table <- table(rf.predicts,test.data$Private)

tree.table
rf.table


