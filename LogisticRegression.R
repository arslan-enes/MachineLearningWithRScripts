df.train <- read.csv("titanic_train.csv") #Reading the csv file.
head(df.train)
str(df.train)

library(Amelia) # Calling library for missing data purposes

# Creating a missing map for get a better idea about missing data

missmap(df.train,main = "Missing Map", col=c("yellow","black"),legend = FALSE)

# Exploring the data

library(ggplot2)

pl_survivors <- ggplot(df.train,aes(x=factor(Survived))) + geom_bar(aes(fill="red")) + labs(x="Died or Survived")
print(pl_survivors)

pl_classes <- ggplot(df.train,aes(Pclass,fill=format(Survived))) + geom_bar(position = "stack")
print(pl_classes)

pl_ages <- ggplot(df.train,aes(x=Age)) + geom_histogram(bins = 20,alpha=0.5,fill="blue",color="black")
print(pl_ages)

pl_age_blox <- ggplot(df.train,aes(x=format(Pclass),y=Age)) + 
              geom_boxplot(aes(fill=format(Pclass)))+
              scale_y_continuous(breaks = seq(min(0),max(80),by=2))
print(pl_age_blox)

# Function that will fill 'NA' values with average age of the class they are in

fill.nas <- function(age_column,pclass_column){
  
  out <- age_column
  out_length <- length(out)
  
  for (i in 1:out_length) {
    
    if(is.na(out[i])){
      
      if(pclass_column[i] == 1){
        out[i] <- 42
      }
      else if(pclass_column[i] == 2){
        out[i] <- 28
      }
      else if(pclass_column[i] == 3){
        out[i] <- 26
      }
    }
    
  }
  return(out)
  
}

## Filling 'NA' values

new_ages <- fill.nas(df.train$Age,df.train$Pclass)
df.train$Age <- new_ages

## NA check

missmap(df.train,main = "Missing Map", col=c("yellow","black"),legend = FALSE)

library(dplyr)

df.train <- select(df.train,-PassengerId,-Name,-Cabin,-Ticket)

## Int values turning into factors because this is more easy to work on it

df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)
df.train$Parch <- factor(df.train$Parch)
df.train$Sex <- factor(df.train$Sex)
df.train$Embarked <- factor(df.train$Embarked)

## Creating the model

log.model <- glm(Survived ~.,family = binomial(link = "logit"),data = df.train)
summary(log.model)

## Doing the same data cleaning to the test data

df.test <- read.csv("titanic_test.csv")

missmap(df.test,main = "Missing Map",col = c("yellow","black"),legend = FALSE)

pl_age_box_test <- ggplot(df.test,aes(x=format(Pclass),y=Age)) + 
                   geom_boxplot(aes(fill=format(Pclass)))+
                   scale_y_continuous(breaks = seq(min(0),max(80),by=2))  
print(pl_age_box_test)

new_ages <- fill.nas(df.test$Age,df.test$Pclass)
df.test$Age <- new_ages

df.test <- select(df.test,-PassengerId,-Name,-Cabin,-Ticket)

df.test$Survived <- factor(df.test$Survived)
df.test$Pclass <- factor(df.test$Pclass)
df.test$SibSp <- factor(df.test$SibSp)
df.test$Parch <- factor(df.test$Parch)
df.test$Sex <- factor(df.test$Sex)
df.test$Embarked <- factor(df.test$Embarked)

fitted.probabilities <- predict(log.model,df.test,type = "response")
