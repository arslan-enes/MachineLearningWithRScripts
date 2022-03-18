library(ISLR) ## Calling the ISLR library

head(iris) ## Getting information about dataframe
str(iris)

df <- scale(iris[,-5])
head(df)
var(df[,2])

df <- merge(df,iris[,5])
head(df)

colnames(df)[5] <- "Species"

## Splitting into train and test
library(caTools)
set.seed(101)
sample <- sample.split(df$Species,SplitRatio =.70)

train.data <- subset(df,sample==TRUE)

test.data <- subset(df,sample==FALSE)

library(class)

predicted_species <- knn(train.data[1:4],test.data[1:4],train.data$Species,k=5)
misclass.error <- mean(predicted_species != test.data$Species)
misclass.error

head(sample,20)

data.frame(predicted_species,test.data$Species)[6000,]

### Left unfinished due to an unexpected result.

