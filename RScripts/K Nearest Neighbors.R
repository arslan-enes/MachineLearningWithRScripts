library(ISLR) ## Getting the data

str(Caravan)
summary(Caravan$Purchase)

any(is.na(Caravan))

var(Caravan[,1]) 
var(Caravan[,2])

purchase <- Caravan$Purchase
standardized.Caravan <- scale(Caravan[,-86]) #Scaling the data frome for avoiding huge variance values

print(var(standardized.Caravan[,1]))
print(var(standardized.Caravan[,2]))

# Train-Test Split

test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]
nrow(test.data)

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]

# Building the KNN model

library(class)

set.seed(101)

predicted.purchase <- knn(train.data,test.data,train.purchase,k=9)
head(predicted.purchase)

misclass.error <- mean(test.purchase != predicted.purchase)
misclass.error

# X=c(1,2,4)
# Y=c(1,2,3)
# misclass.error <- mean(X!=Y)
# misclass.error

# CHOOSING A K VALUE 

predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20){
  predicted.purchase <- knn(train.data,test.data,train.purchase,k=i)
  error.rate <- append(error.rate,mean(test.purchase != predicted.purchase))
}

optimal.k <- match(min(error.rate),error.rate) ## Getting the k value that giving lowest error rate

# VISUALIZE K ELBOW METHOD

library(ggplot2)
df <- data.frame(1:20,error.rate)
colnames(df) <- c("kvalue","errorRate")
df
pl <- ggplot(df,aes(x=kvalue,y=errorRate)) + geom_point() + geom_line(color='red')
print(pl)