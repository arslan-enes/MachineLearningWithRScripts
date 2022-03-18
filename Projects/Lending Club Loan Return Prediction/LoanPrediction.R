loans <- read.csv("Projects/Lending Club Loan Return Prediction/loan_data.csv")

head(loans)
summary(loans)
str(loans)

loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)

library(ggplot2)

pl <- ggplot(loans,aes(x=fico)) 
pl <- pl + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
pl + scale_fill_manual(values = c('green','red')) + theme_bw()
print(pl)

pl2 <- ggplot(loans,aes(purpose)) + geom_bar(aes(fill=not.fully.paid),position = "dodge")
print(pl2)

library(caTools)
set.seed(101)
sample <- sample.split(loans$not.fully.paid,SplitRatio = .7)
train.data <- subset(loans,sample==T)
test.data <- subset(loans,sample==F)


library(e1071)

svm.model <- svm(not.fully.paid~.,data = train.data)
summary(svm.model)

svm.model.predicts <- predict(svm.model,test.data)
table(svm.model.predicts,test.data$not.fully.paid)


tune.results <- tune(svm,train.x=not.fully.paid~., data=train.data,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))
summary(tune.results)
